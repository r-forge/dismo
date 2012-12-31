#author: Jean-Pierre Rossi <jean-pierre.rossi@supagro.inra.fr>

.messi2 <- function(p,v){
	v <- na.omit(v)
	f <- 100*findInterval(p, sort(v)) / length(v)
	minv <- min(v)
	maxv <- max(v)
	ifelse(f == 0, 100*(p-minv)/(maxv-minv), 
		ifelse(f <= 50, 2*f, 
		ifelse(f < 100, 2*(100-f),
			100*(maxv-p)/(maxv-minv)
	)))
}


.messOld <- function(x, v, full=FALSE) {

	stopifnot(NCOL(v) == nlayers(x))
	out <- raster(x)
	E <- getValues(x)

	nl <- nlayers(x)
	if (nl == 1) {
		rmess <- .messi2(E, v[,i])
		names(out) <- 'mess'
		return( setValues(out, rmess) )

	} else {
		E <- sapply(1:ncol(E), function(i) .messi2(E[,i], v[,i]))
		rmess <- raster:::.rowMin(E)
		if (full) {
			out <- brick(out, nl=nl+1)
			names(out) <- c(names(x), "mess")
			return( setValues(out, cbind(E, rmess)) )
		} else {
			names(out) <- 'mess'
			return( setValues(out, rmess) )
		}
	}	
}


mess <- function(x, v, full=FALSE, filename='', ...) {

	stopifnot(NCOL(v) == nlayers(x))
	out <- raster(x)
	nl <- nlayers(x)
	filename <- trim(filename)
	nms <- paste(names(x), '_mess', sep='')
	
	if (canProcessInMemory(x)) {
		x <- getValues(x)
		if (nl == 1) {
			rmess <- .messi2(x, v)
			names(out) <- 'mess'
			out <- setValues(out, rmess)
		} else {
			x <- sapply(1:ncol(x), function(i) .messi2(x[,i], v[,i]))
			rmess <- raster:::.rowMin(x)
			if (full) {
				out <- brick(out, nl=nl+1)
				names(out) <- c(nms, "mess")
				out <- setValues(out, cbind(x, rmess))
			} else {
				names(out) <- 'mess'
				out <- setValues(out, rmess)
			}
		}	
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		}
		return(out)
		
	} else {

		if (nl == 1) {
		
			names(out) <- "mess"
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, ...)	
			out <- writeStart(out, filename, ...)
			for (i in 1:tr$n) {
				vv <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				vv <- .messi2(vv, v)
				out <- writeValues(out, vv, tr$row[i])
				pbStep(pb) 
			}
		
		} else {
	
			if (full) {
				out <- brick(out, nl=nl+1)
				names(out) <- c(nms, "mess")
				tr <- blockSize(out)
				pb <- pbCreate(tr$n, ...)	
				out <- writeStart(out, filename, ...)
				for (i in 1:tr$n) {
					vv <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					vv <- sapply(1:ncol(v), function(i) .messi2(vv[,i], v[,i]))
					m <- raster:::.rowMin(vv)
					out <- writeValues(out, cbind(vv, m), tr$row[i])
					pbStep(pb) 
				}
				
			} else {
			
				names(out) <- "mess"
				tr <- blockSize(out)
				pb <- pbCreate(tr$n, ...)	
				out <- writeStart(out, filename, ...)
				for (i in 1:tr$n) {
					vv <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					vv <- sapply(1:ncol(v), function(i) .messi2(vv[,i], v[,i]))
					m <- raster:::.rowMin(vv)
					out <- writeValues(out, m, tr$row[i])
					pbStep(pb) 
				}
			}
		}
		out <- writeStop(out)
		pbClose(pb) 
	}	
	out
}
