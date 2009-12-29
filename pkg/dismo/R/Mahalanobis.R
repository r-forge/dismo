# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3

setClass('Mahalanobis',
	contains = 'DistModel',
	representation (
		cov = 'matrix' 
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("mahal")) {
	setGeneric("mahal", function(x, p, ...)
		standardGeneric("mahal"))
}	

setMethod('mahal', signature(x='matrix', p='missing'), 
	function(x, p, ...) {
		m <- new('Mahalanobis')
		m@presence <- x
		m@cov <- var(x)
		m
	}
)

setMethod('mahal', signature(x='data.frame', p='missing'), 
	function(x, p, ...) {
		mahal(as.matrix(x))
	}
)

setMethod('mahal', signature(x='Raster', p='matrix'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		mahal(m)
	}
)

setMethod('mahal', signature(x='Raster', p='data.frame'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		mahal(m)
	}
)

setMethod('mahal', signature(x='Raster', p='SpatialPoints'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		mahal(m)
	}
)


setMethod('predict', signature(object='Mahalanobis'), 
function(object, x, ext=NULL, filename='', progress='text', ...) {

	if (! (extends(class(x), 'Raster')) ) {
		if (! all(colnames(object@presence) %in% colnames(x)) ) {
			stop('missing variables in matrix ')
		}
		x <- x[ , colnames(object@presence),drop=FALSE]
		mah <- 1 - apply(data.frame(x), 1, FUN=function(z) min( mahalanobis(object@presence, z, object@cov)))
		return(mah)
		
	} else {
		if (! all(colnames(object@presence) %in% layerNames(x)) ) {
			stop('missing variables in Raster object ')
		}
	
		out <- raster(x)
		if (canProcessInMemory(out, 2)) {
			inmem <- TRUE
			v <- matrix(NA, ncol=nrow(out), nrow=ncol(out))
		} else {
			inmem <- FALSE
			if  (filename == '') {
				filename <- rasterTmpFile()
				if (getOption('verbose')) { cat('writing raster to:', filename)	}						
			}
		}

		cn <- colnames(object@presence)
		pb <- pbCreate(nrow(out), type=progress)
		for (r in 1:nrow(out)) {
			vals <- getValues(x, r)
			vals <- vals[,cn,drop=FALSE]
			mah <- 1 - apply(data.frame(vals), 1, FUN=function(z) min( mahalanobis(object@presence, z, object@cov)))
			if (inmem) {
				v[,r] <- mah
			} else {
				out <- setValues(out, mah)
				out <- writeRaster(out, filename, ...)
			}
			pbStep(pb, r) 
		} 
		if (inmem) {
			out <- setValues(out, as.vector(v))
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
		}
		pbClose(pb)
		return(out)
	}
})
