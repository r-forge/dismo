# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3


setClass('Domain',
	contains = 'matrix',
	representation (
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
		d <- new('Domain', as.matrix(m))
		d
	}
)

setMethod('domain', signature(x='matrix', p='missing'), 
	function(x, p, ...) {
		new('Domain', x)
	}
)

setMethod('domain', signature(x='Raster', p='Spatial'), 
	function(x, p, ...) {
		domain(x, coordinates(p), ...)
	}
)




.domdist <- function(x, y) {
	r <- range(x, na.rm=TRUE)
	r <- r[2]-r[1]
	d <- apply(data.frame(y), 1, FUN=function(z)min(abs(x-z)/r))
	d[which(d > 1)] <- 1
	d
}


setMethod('predict', signature(object='Domain'), 
function(object, x, ext=NULL, filename='', progress='', ...) {
		
	if (! (extends(class(x), 'Raster')) ) {
		if (! all(colnames(object) %in% colnames(x)) ) {
			stop('missing variables in x ')
		}
		
		dom <- matrix(ncol=length(colnames(object)), nrow=nrow(x))
		ln <- colnames(object)
		for (i in 1:ncol(dom)) {
			dom[,i] <- .domdist(object[,ln[i]], x[,ln[i]])
		}
		return ( 1 - apply(dom, 1, max) )

	} else {

		if (! all(colnames(object) %in% layerNames(x)) ) {
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

		ln <- colnames(object)
		dom <- matrix(ncol=nlayers(x), nrow=ncol(x))
		pb <- pbCreate(nrow(out), type=progress)
		for (r in 1:nrow(out)) {
			vals <- getValues(x, r)
			for (i in 1:ncol(dom)) {
				dom[,i] <- .domdist(object[,ln[i]], vals[,ln[i]])
			}
			if (inmem) {
				v[,r] <- 1 - apply(dom, 1, max)
			} else {
				out <- setValues(out, 1 - apply(dom, 1, max))
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

