# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3

setClass('Bioclim',
	contains = 'matrix',
	representation (
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("bioclim")) {
	setGeneric("bioclim", function(x, p, ...)
		standardGeneric("bioclim"))
}	

setMethod('bioclim', signature(x='Raster', p='matrix'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		bc <- new('Bioclim', as.matrix(m))
		bc
	}
)

setMethod('bioclim', signature(x='matrix', p='missing'), 
	function(x, p, ...) {
		new('Bioclim', x)
	}
)

setMethod('bioclim', signature(x='Raster', p='Spatial'), 
	function(x, p, ...) {
		bioclim(x, coordinates(p), ...)
	}
)

.percRank <- function(x, y) {
	x <- sort(as.vector(na.omit(x)))
	y <- data.frame(y)
	b <- apply(y, 1, FUN=function(z)sum(x<z))
	t <- apply(y, 1, FUN=function(z)sum(x==z))
	r <- (b + 0.5 * t)/length(x)
	i <- which(r > 0.5)
	r[i] <- 1-r[i]
	r * 2
}


setMethod('predict', signature(object='Bioclim'), 
function(object, x, ext=NULL, filename='', progress='', ...) {

	if (! (extends(class(x), 'Raster')) ) {
		if (! all(colnames(object) %in% colnames(x)) ) {
			stop('missing variables in x ')
		}
		ln <- colnames(object)
		bc <- matrix(ncol=length(ln), nrow=nrow(x))
		for (i in 1:ncol(bc)) {
			bc[,i] <- .percRank(object[,ln[i]], x[,ln[i]])
		}
		return( apply(bc, 1, min) )

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
		bc <- matrix(ncol=nlayers(x), nrow=ncol(x))
		pb <- pbCreate(nrow(out), type=progress)
		for (r in 1:nrow(out)) {
			vals <- getValues(x, r, names=TRUE)
			for (i in 1:length(ln)) {
				bc[,i] <- .percRank(object[,ln[i]], vals[,ln[i]])
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
})


