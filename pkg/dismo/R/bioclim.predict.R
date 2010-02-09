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



setMethod("plot", signature(x='Bioclim', y='missing'), 
	function(x, a=1, b=2, p=0.9, ...) {
		
		d <- x@presence
	
		myquantile <- function(x, p) {
			p <- min(1, max(0, p))
			x <- sort(as.vector(na.omit(x)))
			if (p == 0) return(x[1])
			if (p == 1) return(x[length(x)])
			i = (length(x)-1) * p + 1
			ti <- trunc(i)
			below = x[ti]
			above = x[ti+1]
			below + (above-below)*(i-ti)  
		}
	
		p <- min(1,  max(0, p))
		if (p > 0.5) p <- 1 - p
		p <- p / 2
		prd <- predict(x, d)
		i <- prd > p & prd < (1-p)
		plot(d[,a], d[,b], xlab=colnames(d)[a], ylab=colnames(d)[b], cex=0)
		type=6
		x1 <- quantile(d[,a], probs=p, type=type)	
		x2 <- quantile(d[,a], probs=1-p, type=type)	
		y1 <- quantile(d[,b], probs=p, type=type)	
		y2 <- quantile(d[,b], probs=1-p, type=type)	
#		x1 <- myquantile(x[,a], p)	
#		x2 <- myquantile(x[,a], 1-p)	
#		y1 <- myquantile(x[,b], p)	
#		y2 <- myquantile(x[,b], 1-p)	
		polygon(rbind(c(x1,y1), c(x1,y2), c(x2,y2), c(x2,y1), c(x1,y1)), border='blue', lwd=2)
		points(d[i,a], d[i,b], xlab=colnames(x)[a], ylab=colnames(x)[b], col='green' )
		points(d[!i,a], d[!i,b], col='red', pch=3)
	}
)
