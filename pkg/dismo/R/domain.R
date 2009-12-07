# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3


if (!isGeneric("domain")) {
	setGeneric("domain", function(p, x,...)
		standardGeneric("domain"))
}	


.DOM <- function(x, y) {
	r <- range(x, na.rm=TRUE)
	r <- r[2]-r[1]
	d <- apply(data.frame(y), 1, FUN=function(z)min(abs(x-z)/r))
	d[which(d > 1)] <- 1
	d
}


setMethod('domain', signature(p='matrix',x='RasterStackBrick'), 
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

		dom <- matrix(ncol=nlayers(x), nrow=ncol(x))
		pb <- pbCreate(nrow(out), type=progress)
		for (r in 1:nrow(out)) {
			vals <- getValues(x, r)
			for (i in 1:ncol(dom)) {
				dom[,i] <- .DOM(obs[,i], vals[,i])
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
)


setMethod('domain', signature(p='Spatial', x='Raster'), 
	function(p, x, ...) {
		domain(coordinates(p), x, ...)
	}
)

