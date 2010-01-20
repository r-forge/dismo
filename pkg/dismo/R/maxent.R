# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3

setClass('MaxEnt',
	contains = 'DistModel',
	representation (
		lambdas  = 'vector'
	),	
	prototype (	
		lambdas = as.vector(NA)
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
		if (pp < 10) {
			print(object@presence)
		} else {
			print(object@presence[1:10,])
			cat('\n')
			cat('  (... ...  ...)\n')
			cat('\n')
		}
		pp <- nrow(object@absence)
		cat('\nabsence points:', pp, '\n')
		if (pp < 25) {
			print(object@absence)
		} else {
			print(object@absence[1:25,])
			cat('\n')
			cat('  (... ...  ...)\n')
			cat('\n')
		}
	}
)	


.getMatrix <- function(x) {
	if (inherits(x, 'SpatialPoints')) {
		x <- coordinates(x)
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


if (!isGeneric("maxent")) {
	setGeneric("maxent", function(x, p, ...)
		standardGeneric("maxent"))
}	



setMethod('maxent', signature(x='SpatialGridDataFrame', p='ANY'), 
	function(x, p, a=NULL,...) {
		x <- brick(x)
		p <- .getMatrix(p)
		if (! is.null(a) ) { a <- .getMatrix(a) }

		# Signature = raster, ANY
		maxent(x, p, a, ...)
	}
)

setMethod('maxent', signature(x='Raster', p='ANY'), 
	function(x, p, a=NULL, ...) {
#extract values for points from stack
		p = .getMatrix(p)
		pv <- data.frame(pa=1, species='species')
		pv1 <- cbind(pv, p, xyValues(x, p))

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
			av <- data.frame(pa=0, species='species')
			av <- cbind(av, a, xyValues(x, a))
			avr = nrow(av)
			av <- na.omit(av)
			nas <- length(as.vector(attr(av, "na.action")))
			if (nas > 0) {
				if (nas >= 0.5 * avr) {
					stop('more than half of the abpresence points have NA predictor values')
				} else {
					warning(100*nas/nrow(avr), '% of the presence points have NA predictor values')
				}
			}
		} else { 
		# random absence
			xy <- randomPoints( raster(x,1), 10000, p, warn=0 )
			av <- data.frame(pa=0, species='background', xy, xyValues(x, xy))
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
		maxent(rbind(pv, av), ...)	
	}
)




setMethod('maxent', signature(x='data.frame', p='missing'), 
	function(x, ...) {

		jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
		if (!file.exists(jar)) {
			stop('file missing:', jar, '.\nPlease download it here: http://www.cs.princeton.edu/~schapire/maxent/')
		}
		
		d <- .meTmpDir()
		dirout <- paste(d, '/out', sep='')
		if (! file.exists(dirout)) {
			dir.create(dirout, showWarnings=TRUE )
		}
		pfn <- .maxentTmpFile()
		afn <- .maxentTmpFile()

		pv <- x[x$pa == 1, ][,-1]
		av <- x[x$pa == 0, ][,-1]
		
		write.table(pv, file=pfn, sep=',', row.names=FALSE)
		write.table(av, file=afn, sep=',', row.names=FALSE)

		mxe <- .jnew("mebridge") 
	
		add <- NULL  # to replace with additional arguments supplied with ...
		.jcall(mxe, "V", "fit", c("autorun", "-e", afn, "-o", dirout, "-s", pfn, add)) 
		
		flam <- paste(dirout, '/species.lambdas', sep='')
		lambdas <- readLines(flam)
		me <- new('MaxEnt')
		me@lambdas <- unlist(lambdas)
		me@presence <- as.matrix(pv[,-(1:3)])
		me@absence <- as.matrix(av[,-(1:3)])
		me@hasabsence <- TRUE
#		file.remove(list.files(path=dirout, full.names=TRUE))
#		file.remove(list.files(path=out, full.names=TRUE))
		unlink(paste(d, "/*", sep=""), recursive = TRUE)
		me
	}
)



.meTmpDir <- function() {
	return( paste(dirname(tempdir()), '/R_maxent_tmp', sep="") )
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
