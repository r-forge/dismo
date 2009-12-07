# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3

setClass('Bioclim',
	representation (
		x = 'matrix'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


.PR <- function(x, y) {
	x <- sort(as.vector(na.omit(x)))
	y <- data.frame(y)
	b <- apply(y, 1, FUN=function(z)sum(x<z))
	t <- apply(y, 1, FUN=function(z)sum(x==z))
	r <- (b + 0.5 * t)/length(x)
	i <- which(r > 0.5)
	r[i] <- 1-r[i]
	r * 2
}


if (!isGeneric("bioclim")) {
	setGeneric("bioclim", function(p, x,...)
		standardGeneric("bioclim"))
}	


setMethod('bioclim', signature(p='matrix',x='RasterStackBrick'), 
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

		bc <- matrix(ncol=nlayers(x), nrow=ncol(x))
		pb <- pbCreate(nrow(out), type=progress)
		for (r in 1:nrow(out)) {
			vals <- getValues(x, r)
			for (i in 1:ncol(bc)) {
				bc[,i] <- .PR(obs[,i], vals[,i])
			}
			if (inmem) {
				v[,r] <- apply(bc, 1, min)
			} else {
				out <- setValues(out, apply(bc, 1, min))
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


setMethod('bioclim', signature(p='Spatial', x='RasterStackBrick'), 
	function(p, x, ...) {
		bioclim(coordinates(p), x, ...)
	}
)

