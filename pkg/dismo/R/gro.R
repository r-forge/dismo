


setClass('Ecolim',
	representation (
		contains = 'DistModel',
		parameters  = 'matrix'
	)
)



if (!isGeneric("ecolim")) {
	setGeneric("ecolim", function(x, ...)
		standardGeneric("ecolim"))
}	


setMethod ('ecolim' , 'matrix', 
	function(x, ...) {
		if (any(dim(x)==0)) {
			stop('matrix has no rows or columns')
		}
		i <- match(c('minkill', 'mingrow', 'minopt', 'maxopt', 'maxgrow', 'maxkill', 'tsum'), colnames(x))
		if ( any( is.na(i) ) ) {
			stop("colnames of 'x' should include 'minkill', 'mingrow', 'minopt', 'maxopt', 'maxgrow', and 'maxkill', 'tsum'")
		}
		if (is.null(rownames(x))) {
			stop("rownames of x is NULL")
		}
		x <- x[rownames(x) != '', , drop=FALSE]
		if (nrow(x) == 0) {
			stop("x has no rows with names")
		}
		m <- new('Ecolim')
		m@parameters <- x[, i, drop=FALSE]
		m
	}
)



setMethod ('plot', signature(x='Ecolim', y='missing'),
	function(x, col='red', lwd=2, nr, nc, ...) {
		y <- c(0, 0, 1, 1, 0, 0)
		p <- x@parameters
		n <- nrow(p)
		
		if (missing(nc)) {
			nc <- ceiling(sqrt(n))
		} else {
			nc <- max(1, min(n, round(nc)))
		}
		if (missing(nr)) {
			nr <- ceiling(n / nc)
		} else {
			nr <- max(1, min(n, round(nr)))
			nc <- ceiling(n / nr)
		}
		old.par <- par(no.readonly = TRUE)
		on.exit(par(old.par))
		par(mfrow=c(nr, nc))
		rn <- rownames(p)
		p[!is.finite(p)] <- NA
		for (i in 1:n) {
			plot(p[i,], y, xlab=rn[i], ylab='', type='l', col=col, lwd=lwd, ...)	
		}
	}
)

	


if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	




setMethod('predict', signature(object='Ecolim'), 
	function(object, x, ...) {
		if (inherits(x, 'Raster')) {
			i <- names(x) %in% rownames(object@parameters)
			return(i)
		
		} else if (inherits(x, 'data.frame')) {
			i <- colnames(x) %in% rownames(object@parameters)
			return(TRUE)
			
		} else if (inherits(x, 'matrix')) {
			i <- colnames(x) %in% rownames(object@parameters)
			return(TRUE)
			
		} else {
			return(FALSE)
		}
	}
)





.getY <- function(a, x) {
	inter <- function(x1, y1, x2, y2, x) {
		y1 + (y2-y1) * (x-x1) / (x2-x1) 
	}
	y <- x
	if (is.null(a)) {
		y[] <- 1
	} else {
		y[] <- NA
		y[ x <= a[1] ] <- 0
		y[ x <= a[2] & x > a[1] ] <- inter(a[1], 0, a[2], 1, x[ x <= a[2] & x > a[1] ] )
		y[ x <= a[3] & x > a[2] ] <- 1
		y[ x <= a[4] & x > a[3] ] <- inter(a[3], 1, a[4], 0, x[ x <= a[4] & x > a[3] ] )
		y[ x >= a[4] ] <- 0
	}
	return(y)
}


.doEcocrop <- function(crop, tmin, tavg, prec, rainfed) {
	
	
	duration <- round((crop@GMIN + crop@GMAX) / 60) 
	tmp <- c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX)
	temp <- .getY(tmp, tavg)
	ktmp <- c(crop@KTMP, crop@KTMP, Inf, Inf)
	tmin <- .getY(ktmp, tmin-5)
	if (rainfed) {
		pre <- c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX)
		shftprec <- c(prec[12], prec[-12])
		cumprec <- movingFun(prec, n=duration+1, fun=sum, type='from', circular=TRUE)  + shftprec
		prec <- .getY(pre, cumprec)
		allv <- cbind(temp, tmin, prec)
	} else {
		allv <- cbind(temp, tmin)
	}	
	minv <-  apply(allv, 1, min)
	obj <- new('ECOCROP')
	obj@crop <- crop
	obj@suitability <- movingFun(minv, n=duration, fun=min, type='from', circular=TRUE) 
	obj@maxsuit <- max(obj@suitability)
	if (obj@maxsuit > 0) {
		obj@maxper <- which(obj@suitability==max(obj@suitability))
	} else {
		obj@maxper <- 0
	}
	return(obj)
}


ecocrop <- function(crop, tmin, tavg, prec, rainfed=TRUE, ...) {
	if (class(crop) == 'character') {
		crop <- getCrop(crop)
	}
	if (missing(prec) & rainfed) {
		stop('prec missing while rainfed=TRUE' )
	}
	
	if (inherits(tmin, 'Raster')) {
		if (nlayers(tmin) != 12) {
			stop()
		}
		.ecoSpat(crop, tmin, tavg, prec, rainfed)
	} else {
		.doEcocrop(crop=crop, tmin=tmin, tavg=tavg, prec=prec, rainfed=rainfed, ...)
	}
}


.ecoSpat <- function(crop, tmin, tavg, prec, rainfed, div=10, filename='', ...) { 

	outr <- raster(tmin)
	filename <- trim(filename)
	v <- vector(length=ncol(outr))
	if (filename=='') {
		vv <- matrix(ncol=nrow(outr), nrow=ncol(outr))
	}
	for (r in 1:nrow(outr)){
		v[] <- NA
		tmp <- getValues(tavg, r) / div
        tmn <- getValues(tmin, r) / div
        if (rainfed) { 
			pre <- getValues(prec, r)
		}
        nac <- which(!is.na(tmn[,1]))
        for (c in nac) {
            if(sum(is.na(tmp)) == 0) {
				e <- .doEcocrop(crop, tmn, tmp, pre, rainfed=rainfed)
                v[c] <- e@maxper[1]
            }
        }
        if (filename=='') {
            vv[,r] <- v
        } else {
            outr <- setValues(outr, v, r)
            outr <- writeRaster(outr, filename, ...)
        }
    }
    
	if (filename=='') { 
		outr <- setValues(outr, as.vector(vv))  
	}
	
    return(outr)
 }


 