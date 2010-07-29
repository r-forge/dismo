# Robert Hijmans, r.hijmans@gmail.com
# September 2009
# Version 0.1


setClass('ECOCROPcrop',
	representation (
		name  = 'character',
		famname  = 'character',
		scientname  = 'character',
		code = 'integer',
		GMIN    = 'numeric', 
		GMAX   = 'numeric', 
		KTMP    = 'numeric',
		TMIN   = 'numeric', 
		TOPMN    = 'numeric',
		TOPMX  = 'numeric', 
		TMAX   = 'numeric', 
		RMIN   = 'numeric', 
		ROPMN    = 'numeric', 
		ROPMX   = 'numeric', 
		RMAX  = 'numeric', 
		LIG = 'numeric', 
		LIGR  = 'numeric', 
		PP   = 'numeric', 
		PPMIN     = 'numeric',
		PPMAX    = 'numeric',
		TEXT   = 'numeric',
		TEXTR   = 'numeric',
		DEP  = 'numeric',
		DEPR  = 'numeric',
		DRA  = 'numeric', 
		DRAR   = 'numeric', 
		PHMIN   = 'numeric', 
		PHOPMN   = 'numeric',
		PHOPMX  = 'numeric',
		SAL  = 'numeric',
		SALR  = 'numeric',
		FER  = 'numeric',
		FERR  = 'numeric',
		LIMITS  = 'numeric'
	)
)


setMethod ('show' , 'ECOCROPcrop', 
	function(object) {
		cat('name:',object@name, '\n')
		cat('latn:',object@scientname, '\n')
		cat('gdur:', object@GMIN, object@GMAX, '\n')
		cat('temp:', object@TMIN, object@TOPMN, object@TOPMX, object@TMAX, '\n')
		cat('prec:', object@RMIN, object@ROPMN, object@ROPMX, object@RMAX, '\n')
	}
)	


setMethod ('plot', signature(x='ECOCROPcrop', y='missing'),
	function(x, ...) {
		par(mfrow=c(2, 1))
		plot(c(0,1,1,0) ~ c(x@TMIN, x@TOPMN, x@TOPMX, x@TMAX), xlab='temperature', ylab='response')
		lines(c(0,1,1,0) ~ c(x@TMIN, x@TOPMN, x@TOPMX, x@TMAX), col='red')
		plot(c(0,1,1,0) ~ c(x@RMIN, x@ROPMN, x@ROPMX, x@RMAX), xlab='precipitation', ylab='response')
		lines(c(0,1,1,0) ~ c(x@RMIN, x@ROPMN, x@ROPMX, x@RMAX), xlab='precipitation', ylab='', col='blue')
	}
)


.getECOCROPcrops <- function() {
	thisenvir = new.env()
	get( data(ECOCROPcrops, thisenvir), thisenvir)
}

.showECOCROPcrops <- function() {
	tab <- .getECOCROPcrops() 
	tab[,c('NAME', 'SCIENTNAME')]
}

ecocropCrop <- function(name) {
	if (missing(name)) {
		.showECOCROPcrops() 
	}
	tab <- .getECOCROPcrops() 
	tab1 <- toupper(as.vector(tab[,'NAME']))
	tab2 <- toupper(as.vector(tab[,'SCIENTNAME']))
	ind1 <- which(toupper(name) == tab1)
	ind2 <- which(toupper(name) == tab2)
	
	if (length(ind1) == 0 & length(ind2) == 0) {
		stop('Unknown crop. Choose from: ', .showECOCROPcrops())
	} 
	r <- max(ind1, ind2)
	r <- as.matrix(tab[r,])
	crop <- new('ECOCROPcrop')
	crop@name  <- r[,'NAME']
	crop@famname <- r[,'FAMNAME']
	crop@scientname <- r[,'SCIENTNAME']
	crop@code <- as.integer(r[,'CODE'])
	crop@GMIN  <- as.numeric(r[,'GMIN'])
	crop@GMAX   <- as.numeric(r[,'GMAX'])
	crop@KTMP   <- as.numeric(r[,'KTMP'])
	crop@TMIN   <- as.numeric(r[,'TMIN'])
	crop@TOPMN  <- as.numeric(r[,'TOPMN'])
	crop@TOPMX  <- as.numeric(r[,'TOPMX'])
	crop@TMAX   <- as.numeric(r[,'TMAX'])
	crop@RMIN   <- as.numeric(r[,'RMIN'])
	crop@ROPMN  <- as.numeric(r[,'ROPMN'])
	crop@ROPMX  <- as.numeric(r[,'ROPMX'])
	crop@RMAX   <- as.numeric(r[,'RMAX'])
	return(crop)	
}



setClass('ECOCROP',
	representation (
		crop = 'ECOCROPcrop',
		suitability = 'vector',
		maxper = 'vector',
		maxsuit = 'numeric'
	),
	prototype (	
		crop = new('ECOCROPcrop'),
		suitability = rep(NA, 12),
		maxper = c(NA),
		maxsuit = as.numeric(NA)
	),	
	validity = function(object)
	{
		return(TRUE)
	}
)
	

setMethod ('show' , 'ECOCROP', 
	function(object) {
		cat('class      :', class(object), '\n')
		cat('Crop       :', object@crop@name, '\n')
		cat('Suitability:', object@suitability, '\n')
		cat('Best period:', object@maxper, '\n')
	}
)



setMethod ('plot', signature(x='ECOCROP', y='missing'),
	function(x, ...) {
		plot(1:length(x@suitability), x@suitability, xlab='periods', ylab='suitability')
		lines(1:length(x@suitability), x@suitability, col='green', lwd=2)
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


ecocrop <- function(clm, crop, rain=TRUE) {
	if (rain) { nasum <- sum(is.na(c(clm$tmin, clm$tmp, clm$pre)))
	} else { nasum <- sum(is.na(c(clm$tmin, clm$tmp))) }
	if (nasum > 0) { return( new('ECOCROP')) }
	
	if (class(crop) == 'character') {
		crop <- ecocropCrop(crop)
	}
	
	duration <- round((crop@GMIN + crop@GMAX) / 60) 
	tmp <- c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX)
	temp <- .getY(tmp, clm$temp)
	ktmp <- c(crop@KTMP, crop@KTMP, Inf, Inf)
	tmin <- .getY(ktmp, clm$tmin-5)
	if (rain) {
		pre <- c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX)
		shftprec <- c(clm$prec[12], clm$prec[-12])
		cumprec <- movingFun(clm$prec, n=duration+1, fun=sum, type='from', circular=TRUE)  + shftprec
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

