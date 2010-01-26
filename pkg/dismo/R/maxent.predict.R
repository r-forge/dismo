# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3



if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	


setMethod('predict', signature(object='MaxEnt'), 
	function(object, x, ext=NULL, filename='', progress='text', ...) {

	
		lambdas <- .maxentTmpFile()
		variables = colnames(me@presence)

		write.table(object@lambdas, file=lambdas, row.names=FALSE, col.names=FALSE, quote=FALSE)
		
		mxe <- .jnew("mebridge") 
		filename <- trim(filename)
		if (inherits(x, "Raster")) {
			out <- raster(x)
			
			if (! all(colnames(object@presence)  %in%  layerNames(x) )) {
				stop('missing layers (or wrong names)')
			}
			
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
			
			filename <- trim(filename)
			if (!canProcessInMemory(out, 3) & filename == '') {
				filename <- rasterTmpFile()
			}
				# check with model object?
				
			if (filename == '') {
				v <- matrix(ncol=nrow(out), nrow=ncol(out))
			}
			pb <- pbCreate(nrow(out), type=progress)
			cv <- rep(NA, times=ncol(out))
			for (r in 1:nrow(out)) {
				rr <- firstrow + r - 1
				rowvals <- getValuesBlock(x, rr, 1, firstcol, ncols)
				rowvals <- rowvals[,variables,drop=FALSE]
				rowv <- na.omit(rowvals)
				res <- cv
				if (length(rowv) > 0) {
					p <- .jcall(mxe, "[D", "predict", lambdas, .jarray(colnames(rowv)), .jarray(rowv)) 
					naind <- as.vector(attr(rowv, "na.action"))
					if (!is.null(naind)) {
						res[-naind] <- p
					} else {
						res <- p
					}
				}
				res[res == -9999] <- NA
				if (filename != '') {
					out <- setValues(out, res, r)
					out <- writeRaster(out, filename=filename, ...)
				} else {
					v[,r] <- res
				}
				pbStep(pb, r) 
			} 
			pbClose(pb)
			if (filename  == '') {
				out <- setValues(out, as.vector(v))
			}
		} else {
			if (inherits(x, "Spatial")) {
				x <- as.data.frame(x)
			}
			
			if (! all(colnames(object@presence) %in% colnames(x))) {
				stop('missing layers (or wrong names)')
			}
			
			
			x <- x[,variables,drop=FALSE]
			x <- na.omit(x)
			out <- rep(NA, times=nrow(x))
			x = as.matrix(x)
			if (nrow(x) > 0) {
				p <- .jcall(mxe, "[D", "predict", lambdas, .jarray(colnames(x)), .jarray(x)) 
				p[p == -9999] <- NA
				naind <- as.vector(attr(x, "na.action"))
				if (!is.null(naind)) {
					out[-naind] <- p
				} else {
					out <- p
				}
			} 
		}
		#try( file.remove(lambdas), silent=TRUE )
		out
	}
)
