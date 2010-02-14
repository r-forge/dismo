# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3


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
		
		dom <- matrix(ncol=length(colnames(object@presence)), nrow=nrow(x) )
		ln <- colnames(object@presence)
		for (i in 1:ncol(dom)) {
			dom[,i] <- domdist(object, ln[i], x[,ln[i]])
		}
		return ( apply(dom, 1, min ) )

	} else {

		if (! all(colnames(object@presence) %in% layerNames(x)) ) {
			stop('missing variables in Raster object')
		}
		
		out <- raster(x)
		
		if (!is.null(ext)) {
			ext <- intersectExtent(exent(ext), extent(x))
			out <- crop(out, ext)
			firstrow <- rowFromY(x, yFromRow(out, 1))
			firstcol <- colFromX(x, xFromCol(out, 1))
			ncols <- colFromX(x, xFromCol(out, ncol(out))) - firstcol + 1
		} else {
			firstrow <- 1
			firstcol <- 1
			ncols <- ncol(x)
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

		tr <- blockSize(out, n=nlayers(x)+2)

		dom <- matrix(ncol=nlayers(x), nrow=ncols*tr$size )

		pb <- pbCreate(tr$n, type=progress)	
		for (i in 1:tr$n) {
			rr <- firstrow + tr$rows[i] - 1
			vals <- getValuesBlock(x, row=rr, nrows=tr$size, firstcol, ncols)

			for (j in 1:ncol(dom)) {
				dom[,j] <- domdist(object, ln[j], vals[,ln[j]])
			}
			res <- apply(dom, 1, min)
			if (inmem) {
				res <- matrix(res, nrow=ncol(out))
				cols <- tr$rows[i]:(tr$rows[i]+dim(res)[2]-1)
				v[, cols] <- res
			} else {
				writeValues(out, res, tr$rows[i])
			}
			pbStep(pb, i) 
		} 
		if (inmem) {
			out <- setValues(out, as.vector(v))
			if (filename != '') {
				out <- writeRaster(out, filename=filename, ...)
			}
		} else {
			out <- writeStop(out)	
		}
		pbClose(pb)
		return(out)
	}
})

