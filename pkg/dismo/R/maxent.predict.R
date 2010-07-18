# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3



if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	


setMethod('predict', signature(object='MaxEnt'), 
	function(object, x, ext=NULL, filename='', progress='text', args="", ...) {

		args <- c(args, "")
		lambdas <- .maxentLambdaFile()
		variables = colnames(object@presence)

		write.table(object@lambdas, file=lambdas, row.names=FALSE, col.names=FALSE, quote=FALSE)
		
		mxe <- .jnew("mebridge") 
		filename <- trim(filename)
		if (inherits(x, "Raster")) {
			
			if (! all(colnames(object@presence)  %in%  layerNames(x) )) {
				stop('missing layers (or wrong names)')
			}
			
			out <- raster(x)
			if (!is.null(ext)) {
				out <- crop(out, ext)
				firstrow <- rowFromY(x, yFromRow(out, 1))
				firstcol <- colFromX(x, xFromCol(out, 1))
			} else {
				firstrow <- 1
				firstcol <- 1
			}
			ncols <- ncol(out)
		
			
			filename <- trim(filename)
			if (!canProcessInMemory(out, 3) & filename == '') {
				filename <- rasterTmpFile()
			}
			
			if (filename == '') {
				v <- matrix(ncol=nrow(out), nrow=ncol(out))
			} else {
				out <- writeStart(out, filename=filename, ... )
			}
			
			tr <- blockSize(out, n=nlayers(x)+2)
			pb <- pbCreate(tr$n, type=progress)	
			
			for (i in 1:tr$n) {
				rr <- firstrow + tr$row[i] - 1
				rowvals <- getValuesBlock(x, row=rr, nrows=tr$nrows[i], firstcol, ncols)
				rowvals <- rowvals[,variables,drop=FALSE]
				res <- rep(NA, times=nrow(rowvals))
				rowv <- na.omit(rowvals)
				if (length(rowv) > 0) {
					p <- .jcall(mxe, "[D", "predict", lambdas, .jarray(colnames(rowv)), .jarray(rowv), args) 
					naind <- as.vector(attr(rowv, "na.action"))
					if (!is.null(naind)) {
						res[-naind] <- p
					} else {
						res <- p
					}
				}
				res[res == -9999] <- NA
				if (filename != '') {
					out <- writeValues(out, res, tr$row[i])
				} else {
					res = matrix(res, nrow=ncol(out))		
					cols = tr$row[i]:(tr$row[i]+dim(res)[2]-1)
					v[, cols] <- res
				}
				pbStep(pb, i) 
			} 
			pbClose(pb)
			if (filename  == '') {
				out <- setValues(out, as.vector(v))
			} else {
				out <- writeStop(out)
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
			if (nrow(x) > 0) {
				out <- rep(NA, times=nrow(x))
				xx = as.numeric(as.matrix(x))
				dim(xx) = dim(x)
				colnames(xx) = colnames(x)
				x = xx
				p <- .jcall(mxe, "[D", "predict", lambdas, .jarray(colnames(x)), .jarray(x), args) 
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

