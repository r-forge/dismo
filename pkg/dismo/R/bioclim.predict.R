# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3

if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='Bioclim'), 
function(object, x, ext=NULL, filename='', progress='text', ...) {

	percRank <- function(x, y) {
		x <- sort(as.vector(na.omit(x)))
		y <- data.frame(y)
		b <- apply(y, 1, FUN=function(z)sum(x<z))
		t <- apply(y, 1, FUN=function(z)sum(x==z))
		r <- (b + 0.5 * t)/length(x)
		i <- which(r > 0.5)
		r[i] <- 1-r[i]
		r * 2
	}


	if (! (extends(class(x), 'Raster')) ) {
		if (! all(colnames(object@presence) %in% colnames(x)) ) {
			stop('missing variables in x ')
		}
		ln <- colnames(object@presence)
		bc <- matrix(ncol=length(ln), nrow=nrow(x))
		for (i in 1:ncol(bc)) {
			bc[,i] <- percRank(object@presence[,ln[i]], x[,ln[i]])
		}
		return( apply(bc, 1, min) )

	} else {
		out <- raster(x)
		if (!is.null(ext)) {
			ext <- intersectExtent(extent(ext), extent(x))
			out <- crop(out, ext)
			firstrow <- rowFromY(x, yFromRow(out, 1))
			firstcol <- colFromX(x, xFromCol(out, 1))
			ncols <- colFromX(x, xFromCol(out, ncol(out))) - firstcol + 1
		} else {
			firstrow <- 1
			firstcol <- 1
			ncols <- ncol(x)
		}
		
		if (! all(colnames(object@presence) %in% layerNames(x)) ) {
			stop('missing variables in Raster object')
		}
		
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
		pb <- pbCreate(nrow(out), type=progress)
		
		bbc <- matrix(0, ncol=nlayers(x), nrow=ncols)
	#	for (r in 1:nrow(out)) {
		for (r in 1:nrow(out)) {
			rr <- firstrow + r - 1
			bc <- bbc
			vals <- getValuesBlock(x, rr, 1, firstcol, ncols)
			na <- as.vector(attr(na.omit(vals), 'na.action'))
			bc[na] <- NA
			i <- (apply(t(vals) >= object@min, 2, all) & apply(t(vals) <= object@max, 2, all))
			i[is.na(i)] <- FALSE
			for (j in 1:length(ln)) {
				bc[i,j] <- percRank( object@presence[ ,ln[j]], vals[i, ln[j]] )
			}
			if (inmem) {
				v[,r] <- apply(bc, 1, min)
			} else {
				out <- setValues(out, apply(bc, 1, min))
				out <- writeRaster(out, filename, ...)
			}
			pbStep(pb, r) 
		} 

#		for (r in 1:nrow(out)) {
#			bc <- matrix(ncol=nlayers(x), nrow=ncol(x))
#			vals <- getValues(x, r)
#			for (i in 1:length(ln)) {
#				bc[,i] <- percRank(object@presence[,ln[i]], vals[,ln[i]])
#			}
###			} else {
#				out <- setValues(out, apply(bc, 1, min))
#				out <- writeRaster(out, filename, ...)
#			}
#			pbStep(pb, r) 
#		} 
#	}
	
		if (inmem) {
			out <- setValues(out, as.vector(v))
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
		}
		pbClose(pb)
		return(out)
	}
}
)


