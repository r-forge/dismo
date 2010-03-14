# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3

setClass('MaxEnt',
	contains = 'DistModel',
	representation (
		lambdas  = 'vector',
		results = 'matrix'
	),	
	prototype (	
		lambdas = as.vector(NA),
		results = as.matrix(NA)
	),
)


setMethod ('show' , 'MaxEnt', 
	function(object) {
		cat('class    :' , class(object), '\n\n')
		cat('variables:', colnames(object@presence), '\n\n')
		cat('lambdas\n')
		print(object@lambdas)
		cat('\n')
		pp <- nrow(object@presence)
		cat('\npresence points:', pp, '\n')
		if (pp < 5) {
			print(object@presence)
		} else {
			print(object@presence[1:5,])
			cat('\n')
			cat('  (... ...  ...)\n')
			cat('\n')
		}
		pp <- nrow(object@absence)
		cat('\nabsence points:', pp, '\n')
		if (pp < 5) {
			print(object@absence)
		} else {
			print(object@absence[1:5,])
			cat('\n')
			cat('  (... ...  ...)\n')
			cat('\n')
		}
		cat('\nmodel fit\n')
		cat('\n')
		print(object@results)
		cat('\n')
	}
)	



if (!isGeneric("maxent")) {
	setGeneric("maxent", function(x, p, ...)
		standardGeneric("maxent"))
}	



setMethod('maxent', signature(x='SpatialGridDataFrame', p='ANY'), 
	function(x, p, a=NULL,...) {
		factors = NULL
		for (i in 1:colnames(x@data)) {
			if (is.factor(x[,i])) { factors = c(factors, colnames(x@data)[i]) }
		}
		x <- brick(x)
		p <- .getMatrix(p)
		if (! is.null(a) ) { a <- .getMatrix(a) }
		# Signature = raster, ANY
		maxent(x, p, a, factors=factors, ...)
	}
)


.getMatrix <- function(x) {
	if (inherits(x, 'SpatialPoints')) {
		x <- data.frame(coordinates(x))
	} else if (inherits(x, 'matrix')) {
		x <- data.frame(x)
	}
	if (! class(x) == 'data.frame' ) {
		stop('data should be  a matrix, data.frame, or SpatialPoints* object')
	}
	if (dim(x)[2] != 2) {
		stop('presence or absence coordiantes data should be a matrix or data.frame with 2 columns' ) 	
	}
	colnames(x) <- c('x', 'y')
	return(x)
} 


setMethod('maxent', signature(x='Raster', p='ANY'), 
	function(x, p, a=NULL, factors=NULL, ...) {
#extract values for points from stack
		p = .getMatrix(p)
		pv1 <- data.frame(xyValues(x, p))

		pv <- na.omit(pv1)
		nas <- length(as.vector(attr(pv, "na.action")))
		if (nas > 0) {
			if (nas >= 0.5 * nrow(pv1)) {
				stop('more than half of the presence points have NA predictor values')
			} else {
				warning(100*nas/nrow(pv1), '% of the presence points have NA predictor values')
			}
		} 
		
		if (! is.null(a) ) {
			a = .getMatrix(a)
			av <- data.frame(xyValues(x, a))
			avr = nrow(av)
			av <- na.omit(av)
			nas <- length(as.vector(attr(av, "na.action")))
			if (nas > 0) {
				if (nas >= 0.5 * avr) {
					stop('more than half of the absence points have NA predictor values')
				} else {
					warning(100*nas/nrow(avr), '% of the presence points have NA predictor values')
				}
			}
		} else { 
		# random absence
			xy <- randomPoints( raster(x,1), 10000, p, warn=0 )
			av <- data.frame(xyValues(x, xy))
			av <- na.omit(av)
			if (nrow(av) == 0) {
				stop('could not get valid background point values; is there a layer with only NA values?')
			}
			if (nrow(av) < 100) {
				stop('only got:', nrow(av), 'random background point values; is there a layer with many NA values?')
			}
			if (nrow(av) < 1000) {
				warning('only got:', nrow(av), 'random background point values; Small exent? Or is there a layer with many NA values?')
			}
		}
		
		# Signature = data.frame, missing

		x = rbind(pv, av)
		
		if (!is.null(factors)) {
			for (f in factors) {
				x[,f] <- factor(x[,f])
			}
		}
		
		p = c(rep(1, nrow(pv)), rep(0, nrow(av)))
		maxent(x, p, ...)	
	}
)




setMethod('maxent', signature(x='data.frame', p='vector'), 
	function(x, p, args=NULL, ...) {

		x <- cbind(p, x)
		x <- na.omit(x)
		p <- x[,1]
		x <- x[, -1 ,drop=FALSE]

		factors = NULL
		for (i in 1:ncol(x)) {
			if (class(x[,i]) == 'factor') {
				factors = c(factors, colnames(x)[i])
			}
		}
		
		jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
		if (!file.exists(jar)) {
			stop('file missing:', jar, '.\nPlease download it here: http://www.cs.princeton.edu/~schapire/maxent/')
		}
		
		d <- .meTmpDir()
		dirout <- paste(d, '/out', sep='')
		if (! file.exists(dirout)) {
			dir.create(dirout, showWarnings=TRUE )
		}

		pv <- x[p==1, ,drop=FALSE]
		av <- x[p==0, ,drop=FALSE]
		me <- new('MaxEnt')
		me@presence <- as.matrix(pv)
		me@absence <- as.matrix(av)
		me@hasabsence <- TRUE

		pv <- cbind(data.frame(species='species'), x=1:nrow(pv), y=1:nrow(pv), pv)
		av <- cbind(data.frame(species='background'), x=1:nrow(av), y=1:nrow(av), av)
		
		pfn <- .maxentTmpFile()
		afn <- .maxentTmpFile()
		write.table(pv, file=pfn, sep=',', row.names=FALSE)
		write.table(av, file=afn, sep=',', row.names=FALSE)

		mxe <- .jnew("mebridge") 
	
		if (is.null(factors)) {
			.jcall(mxe, "V", "fit", c("autorun", "-e", afn, "-o", dirout, "-s", pfn, args)) 
		} else {
			.jcall(mxe, "V", "fit", c("autorun", "-e", afn, "-o", dirout, "-s", pfn, args), .jarray(factors))
		}
		
		me@lambdas <- unlist( readLines( paste(dirout, '/species.lambdas', sep='') ) )
		d = t(read.csv(paste(dirout, '/maxentResults.csv', sep='') ))
		d = d[-1, ,drop=FALSE]
		dd = matrix(as.numeric(d))
		rownames(dd) = rownames(d)
		me@results <- dd
		
		unlink(paste(d, "/*", sep=""), recursive = TRUE)
		me
	}
)



.meTmpDir <- function() {
	return( paste(dirname(tempdir()), '/R_raster_tmp/maxent', sep="") )
}


.maxentTmpFile <- function()  {
	d <- .meTmpDir()
	if (!file.exists(d)) {
		dir.create(d, showWarnings=TRUE )
	}
	f <- paste(round(runif(10)*10), collapse="")
	d <- paste(d, '/maxent_', f, '.csv', sep="")
	return(d)
}


.maxentRemoveTmpFiles <- function() {
	d <- .meTmpDir()
	if (file.exists(d)) {
		unlink(paste(d, "/*", sep=""), recursive = TRUE)
	}
}



setMethod("plot", signature(x='MaxEnt', y='missing'), 
	function(x, sort=TRUE, main='Variable contribution', xlab='Percentage', ...) {
		r <- x@results
		rnames = rownames(r)
		i <- grep('.contribution', rnames)
		r <- r[i, ]
		names(r) <- gsub('.contribution', '', names(r))
		if (sort) r = sort(r)
		dotchart(r, main=main, xlab=xlab, ...)
	}
)
