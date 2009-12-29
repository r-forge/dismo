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
		dir <- paste(tempfile(), '/', sep='')
		dir.create(dir, showWarnings=FALSE, recursive=TRUE)
		lambdas <- paste(dir, basename(tempfile()), sep='')
		write.table(object@lambdas, file=lambdas, row.names=FALSE, col.names=FALSE, quote=FALSE)
		mxe <- .jnew("rmaxent") 
		filename <- trim(filename)
		if (inherits(x, "Raster")) {
			out <- raster(x)
			filename <- trim(filename)
			if (!canProcessInMemory(out, 3) & filename == '') {
				filename <- rasterTmpFile()
			}
			vars <- layerNames(x)
				# check with model object?
				
			if (filename == '') {
				v <- matrix(ncol=nrow(out), nrow=ncol(out))
			}
			pb <- pbCreate(nrow(out), type=progress)
			cv <- rep(NA, times=ncol(out))
			for (r in 1:nrow(out)) {
				rowvals <- getValues(x, r) 
				rowv <- na.omit(rowvals)
				res <- cv
				if (length(rowv) > 0) {
					p <- .jcall(mxe, "[D", "predict", lambdas, vars, .jarray(rowv)) 
					naind <- as.vector(attr(rowv, "na.action"))
					if (!is.null(naind)) {
						res[-naind] <- p
					} else {
						res <- p
					}
				}
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
			rowv <- na.omit(rowvals)
			out <- rep(NA, times=nrow(rowvals))
			if (length(rowv) > 0) {
				p <- .jcall(mxe, "[D", "predict", lambdas, vars, .jarray(rowv)) 
				naind <- as.vector(attr(rowv, "na.action"))
				if (!is.null(naind)) {
					out[-naind] <- p
				} else {
					out <- p
				}
			} 
		}
		unlink(dir, recursive = T)
		out
	}
)

