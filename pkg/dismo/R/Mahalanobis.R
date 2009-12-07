# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3


if (!isGeneric("mahala")) {
	setGeneric("mahala", function(p, x,...)
		standardGeneric("mahala"))
}	


.MAH <- function(x, y) {
	S <- var(x)
	apply(data.frame(y), 1, FUN=function(z) min( mahalanobis(x, z, S)))
}


setMethod('mahala', signature(p='matrix',x='RasterStackBrick'), 
	function(p, x, ext=NULL, filename='', progress='', ...) {
		obs <- xyValues(x, p)
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

		pb <- pbCreate(nrow(out), type=progress)
		for (r in 1:nrow(out)) {
			vals <- getValues(x, r)
			mah <- .MAH(obs, vals)
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
)


setMethod('mahala', signature(p='Spatial', x='Raster'), 
	function(p, x, ...) {
		mahala(coordinates(p), x, ...)
	}
)

