# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3


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
		if (dim(p)[2] != 2) { stop('p should have 2 columns' ) 	}
		
		colnames(p) <- c('x', 'y')
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
		
# random absence
		if (ncell(x) < 100000) {
			a <- 1:ncell(x)
		} else {
			a <- unique(round(runif(100000)*ncell(x)))
			#a <- cbind(runif(100000)*(xmax(x)-xmin(x))+xmin(x), runif(100000)*(ymax(x)-ymin(x))+ymin(x))
			#a <- unique(cellFromXY(x, a))
		}
		v <- na.omit(cbind(a, cellValues(x, a)))
		if (nrow(v) < 25) {
			stop('absence points generation failed (number of points < 25)')
		}
		if (nrow(v) < 250) {
			warning('very low number of valid absence points:', nrow(v))
		}
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
		me@presence <- as.matrix(pv[,-(1:3)])
		me@absence <- as.matrix(av[,-(1:3)])
		me@hasabsence <- TRUE
#		file.remove(list.files(path=dirout, full.names=TRUE))
#		file.remove(list.files(path=out, full.names=TRUE))
		unlink(dir, recursive = TRUE)
		me
	}
)
