# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3

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

		tr <- blockSize(out, n=nlayers(x)+2)
		pb <- pbCreate(tr$n, type=progress)	
		for (i in 1:tr$n) {
			rr <- firstrow + tr$rows[i] - 1
			vals <- getValuesBlock(x, row=rr, nrows=tr$size, firstcol, ncols)

			vals <- vals[,cn,drop=FALSE]
			res <- 1 - apply(data.frame(vals), 1, FUN=function(z) min( mahalanobis(object@presence, z, object@cov)))

			if (inmem) {
				res <- matrix(res, nrow=ncol(out))
				v[,tr$rows[i]:dim(res)[2]] <- res
			} else {
				writeValues(out, res, tr$rows[i])
			}
			pbStep(pb, i) 

		} 
		if (inmem) {
			out <- setValues(out, as.vector(v))
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
		} else {
			out <- writeStop(out)	
		}

		pbClose(pb)
		return(out)
	}
})


