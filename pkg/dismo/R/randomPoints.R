# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3


randomPoints <- function(mask, n, p, ext=NULL, extf=1.1, excludep=TRUE, tryf=5, warn=2) {
	if (class(mask) != 'RasterLayer') { 
		mask <- raster(mask, 1)
	}
	
	if (n > ncell(mask)) {
		n <- ncell(mask)
		if (warn>0) { warning('changed n to ncell(mask)') }
	}
	tryf <- max(tryf, 1)
	
	if (missing(p)) { 
		excludep <- FALSE
	} else {
		if (class(p) == 'SpatialPoints' | class(p) == 'SpatialPointsDataFrame') {
			p <- coordinates(p)
		}
	}
	
	if (class(ext)=='character') {
		if (! ext%in% c('points')) { 
			stop("if ext is a character variable it should be 'points'") 
		} else if (missing(p)) { 
			warning("if p is missing, 'ext=points' is meaningless") 
			ext <- extent(mask)  
		} else {
			ext <- extent(min(p[,1]), max(p[,1]), min(p[,2]), max(p[,2]))
		}
	} 

	if (! is.null(ext)) {
		ext <- extent(ext)
		ext <- ext * extf
		ext <- intersectExtent(ext, extent(mask))
		mask2 <- crop(raster(mask), ext)
	}  else {
		mask2 <- raster(mask)
	}
	
	if (excludep) {
		pcells <- cellFromXY(mask2, p)
	}

	nn = n * tryf
	nn = max(nn, 250)
	nn = min(ncell(mask2), nn)
	
	if (nn == ncell(mask2)) {
		cells <- 1:ncell(mask2)
	} else {
		cells <- sampleInt(ncell(mask2), nn)
	}
	xy <- xyFromCell(mask2, cells)
	cells <- cellFromXY(mask, xy)
	if (excludep) {	
		cells <- cells[!(cells%in%pcells)] 	
	}
	vals <- cbind(cells, cellValues(mask, cells))
	cells <- na.omit(vals)[,1]

	if (length(cells) >= n) { 
			cells <- sample(cells, n)
	} else {
		frac <- length(cells) / n
		if (frac < 0.1) {
			stop("generated absence points = ", frac," times requested number; Use a higher value for tryf" )
		}
		if (frac < 0.5  & warn==1) {
			warning("generated absence points = ", frac," times requested number; Use a higher value for tryf" )
		} else if (warn > 1) {
			warning("generated absence points = ", frac," times requested number")
		}
	}
	return(xyFromCell(mask, cells))
}



#	if (canProcessInMemory(mask, 2)) {
#		if (dataContent(mask) != 'all') { mask <- readAll(mask) }
#		if (e < extent(mask)) { mask <- crop(mask, e) }
#		if (dataContent(mask) == 'all') {
#			cells <- na.omit(cbind(1:ncell(r), values(mask)))[,1]
#		} else {
#			cells <- 1:ncell(mask)
#		}
#		if (excludep) {	
#			pcells <- cellFromXY(mask, p)
#			cells <- cells[!(cells%in%pcells)] 	}
#		}
#		if (n > length(cells)) {
#			warning('there are only ',length(cells),' cells available')	
#		} else {
#			cells <- sample(cells, n)
#		}

