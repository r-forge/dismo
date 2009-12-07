# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3


randomPoints <- function(mask, n, p, ext=NULL, extf=1.1, excludep=TRUE, tryf=3) {
# stub function
	if (class(mask) != 'RasterLayer') { 
		mask <- raster(mask, 1)
	}
	
	if (n > ncell(mask)) {
		stop('n > ncell(mask)')
	}

	if (class(ext)=='character') {
		if (! ext%in%c('points', 'raster')) { 
			stop("if ext is a character variable it should be either 'points' or 'raster'") 
		}
		if (ext == 'raster') { ext <- extent(mask) 
		} else if (missing(p)) {  ext <- extent(mask)  }
	} 
	if (missing(p)) { excludep <- FALSE
	} else {
		if (class(p) == 'SpatialPoints' | class(p) == 'SpatialPointsDataFrame') {
			p <- coordinates(p)
		}
		if ( class(ext)=='character') {
			if (ext == 'points' ) {
				ext <- extent(min(p[,1]), max(p[,1]), min(p[,2]), max(p[,2]))
			}
		}
	}

	if (! is.null(ext)) {
		ext <- extent(ext)
		ext <- ext * extf
		ext <- intersectExtent(ext, extent(mask))
		mask <- crop(raster(mask), ext)
	} 
	cells <- unique(round(runif(n*tryf) * ncell(mask)))
	xy <- xyFromCell(mask, cells)
	cells <- cellFromXY(mask, xy)
	if (excludep) {	
		pcells <- cellFromXY(mask, p)
		cells <- cells[!(cells%in%pcells)] 	
	}
	vals <- cbind(cells, cellValues(mask, cells))
	cells <- na.omit(vals)[,1]
	if (length(cells) >= n) { 
		cells <- cells[1:n]
	} else {
		frac <- length(cells) / n
		if (frac < 0.5) {
			stop("generated absence points = ", frac," times requested number; Use a higher value for tryf" )
		} else {
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

