# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3



setClass('Domain',
	contains = 'DistModel',
	representation (
		range='vector'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("domain")) {
	setGeneric("domain", function(x, p, ...)
		standardGeneric("domain"))
}	


setMethod('domain', signature(x='Raster', p='matrix'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		domain(m)
	}
)

setMethod('domain', signature(x='Raster', p='data.frame'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		domain(m)
	}
)

setMethod('domain', signature(x='data.frame', p='missing'), 
	function(x, p, ...) {
		domain(as.matrix(x))
	}
)

setMethod('domain', signature(x='matrix', p='missing'), 
	function(x, p, ...) {
		d <- new('Domain')
		d@presence <- x
		r <- apply(x, 2, FUN=function(x){range(x, na.rm=TRUE)})
		d@range <-  abs(r[2,] - r[1,])
		d
	}
)

setMethod('domain', signature(x='Raster', p='SpatialPoints'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		domain(m)
	}
)



setMethod('predict', signature(object='Domain'), 
function(object, x, ext=NULL, filename='', progress='text', ...) {

	domdist <- function(xx, ii, y) {
		r <- xx@range[ii]
		xx <- xx@presence[,ii]
		d <- apply(data.frame(y), 1, FUN=function(z)(abs(xx-z)/r))
		d <- apply(d, 2, mean)
		d[which(d > 1)] <- 1
		1-d
	}
		
	if (! (extends(class(x), 'Raster')) ) {
		if (! all(colnames(object@presence) %in% colnames(x)) ) {
			stop('missing variables in x ')
		}
		
		dom <- matrix(ncol=length(colnames(object@presence)), nrow=nrow(x))
		ln <- colnames(object@presence)
		for (i in 1:ncol(dom)) {
			dom[,i] <- domdist(object@presence, ln[i], x[,ln[i]])
		}
		return ( apply(dom, 1, min ) )

	} else {

		if (! all(colnames(object@presence) %in% layerNames(x)) ) {
			stop('missing variables in Raster object')
		}
		
		out <- raster(x)
		if (canProcessInMemory(out, 2)) {
			inmem=TRUE
			v <- matrix(NA, ncol=nrow(out), nrow=ncol(out))
		} else {
			inmem <- FALSE
			if  (filename == '') {
				filename <- rasterTmpFile()
				if (getOption('verbose')) { cat('writing raster to:', filename)	}						
			}
		}

		ln <- colnames(object@presence)
		dom <- matrix(ncol=nlayers(x), nrow=ncol(x))
		pb <- pbCreate(nrow(out), type=progress)
		for (r in 1:nrow(out)) {
			vals <- getValues(x, r)
			for (i in 1:ncol(dom)) {
				dom[,i] <- domdist(object, ln[i], vals[,ln[i]])
			}
			ddom <- apply(dom, 1, min)
			if (inmem) {
				v[,r] <- ddom
			} else {
				out <- setValues(out, ddom)
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

