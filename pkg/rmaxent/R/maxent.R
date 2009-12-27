# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3


setClass('MaxEnt',
	representation (
		variables = 'vector',
		lambdas  = 'vector'
	),	
	prototype (	
		variables = as.vector(NA),
		lambdas = as.vector(NA)
	),
	validity = function(object)	{
		return(TRUE)
	}
)

if (!isGeneric("maxent")) {
	setGeneric("maxent", function(x, p, ...)
		standardGeneric("maxent"))
}	


setMethod('maxent', signature(x='Raster', p='SpatialPoints'), 
	function(x, p, ...) {
		p <- coordinates(p)
		maxent(x, p)
	}
)

setMethod('maxent', signature(x='Raster', p='data.frame'), 
	function(x, p, ...) {
		p <- as.matrix(p)
		maxent(x, p)
	}
)

setMethod('maxent', signature(x='SpatialGridDataFrame', p='SpatialPoints'), 
	function(x, p, ...) {
		x <- brick(x)
		p <- coordinates(p)
		maxent(x, p)
	}
)

setMethod('maxent', signature(x='SpatialGridDataFrame', p='matrix'), 
	function(x, p, ...) {
		x <- brick(x)
		maxent(x, p)
	}
)

setMethod('maxent', signature(x='SpatialGridDataFrame', p='data.frame'), 
	function(x, p, ...) {
		x <- brick(x)
		p <- as.matrix(p)
		maxent(x, p)
	}
)

setMethod('maxent', signature(x='Raster', p='matrix'), 
	function(x, p, ...) {
#extract values for points from stack
		colnames(p) <- c('x', 'y')
		pv <- data.frame(pa=1, species='species')
		pv <- cbind(pv, p, xyValues(x, p))
# random absence
		if (ncell(x) < 100000) {
			a <- 1:ncell(x)
		} else {
			a <- cbind(runif(100000)*(xmax(x)-xmin(x))+xmin(x), runif(100000)*(ymax(x)-ymin(x))+ymin(x))
			a <- unique(cellFromXY(x, a))
		}
		v <- na.omit(cbind(a, cellValues(x, a)))
		if (nrow(v) > 10000) {
			v <- sample(v, 10000)
		} 
		xy <- xyFromCell(x, v[,1])
		av <- data.frame(pa=0, species='background', xy, v[,-1])
		maxent(rbind(pv, av), ...)
	}
)


setMethod('maxent', signature(x='data.frame', p='missing'), 
	function(x, ...) {

		jar <- paste(system.file(package="rmaxent"), "/java/maxent.jar", sep='')
		if (!file.exists(jar)) {
			stop('file missing:', jar, '.\nPlease download it here: http://www.cs.princeton.edu/~schapire/maxent/')
		}
		dir <- paste(tempfile(), '/', sep='')
		dirout <- paste(dir, 'out/', sep='')
		dir.create(dir, showWarnings=FALSE, recursive=TRUE)
		dir.create(dirout, showWarnings=FALSE, recursive=TRUE)
		pfn <- paste(dir, basename(tempfile()), '.csv', sep='')
		afn <- paste(dir, basename(tempfile()), '.csv', sep='')

		pv <- x[x$pa == 1, ][,-1]
		av <- x[x$pa == 0, ][,-1]
		
		write.table(pv, file=pfn, sep=',', row.names=FALSE)
		write.table(av, file=afn, sep=',', row.names=FALSE)

		mxe <- .jnew("rmaxent") 
	
		add <- NULL  # to replace with additional arguments supplied with ...
		.jcall(mxe, "V", "fit", c("autorun", "-e", afn, "-o", dirout, "-s", pfn, add)) 
		flam <- paste(dirout, 'species.lambdas', sep='')
		lambdas <- readLines(flam)
		me <- new('MaxEnt')
		me@lambdas <- unlist(lambdas)
		me@variables <- colnames(x)
	
		unlink(dir, recursive = T)
		me
	}
)


if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	


setMethod('predict', signature(object='MaxEnt'), 
	function(object, x, ext=NULL, filename='', progress='', ...) {
		dir <- paste(tempfile(), '/', sep='')
		dir.create(dir, showWarnings=FALSE, recursive=TRUE)
		lambdas <- paste(dir, basename(tempfile()), sep='')
		write.table(object@lambdas, file=lambdas, row.names=FALSE, col.names=FALSE, quote=FALSE)
		mxe <- .jnew("rmaxent") 

		if (inherits(x, "Raster")) {
			out <- raster(x)
			vars <- layerNames(x)
			v <- matrix(ncol=nrow(out), nrow=ncol(out))
			pb <- pbCreate(nrow(out), type=progress)
			for (r in 1:nrow(out)) {
				rowvals <- getValues(x, r) 
				rowv <- na.omit(rowvals)
				if (length(rowv) > 0) {
					p <- .jcall(mxe, "[D", "predict", lambdas, vars, .jarray(rowv)) 
					naind <- as.vector(attr(rowv, "na.action"))
					if (!is.null(naind)) {
						v[-naind,r] <- p
					} else {
						v[,r] <- p
					}
				}
				if (filename != '') {
							
				}
				pbStep(pb, r) 
			} 
			pbClose(pb)
			out <- setValues(out, as.vector(v))
		} else {
			if (inherits(x, "Spatial")) {
				x <- as.data.frame(x)
			}
			rowv <- na.omit(rowvals)
			out <- rep(NA, times=nrow(rowvals))
			if (length(rowv) > 0) {
				p <- .jcall(mxe, "[D", "predict", lambdas, vars, .jarray(rowv)) 
				naind <- as.vector(attr(rowv, "na.action"))
				if (!is.null(naind)) {
					out[-naind] <- p
				} else {
					out <- p
				}
			} 
		}
		unlink(dir, recursive = T)
		out
	}
)

