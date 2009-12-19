# Download geographic data and return as R object
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.9
# October 2008


.pointsToMatrix <- function(p) {
	latlon <- FALSE
	if (class(p) == 'SpatialPoints' | class(p) == 'SpatialPointsDataFrame') {
		if (isLatLon(p)) { latlon <- TRUE 
		} else {
			if (projection(p)!='NA') {
				stop('points are projected')
			}
		}
		p <- coordinates(p)
	}
	if (is.data.frame(p)) {
		p <- as.matrix(p)
	}
	if (is.vector(p)){
		if (length(p) != 2) {
			stop('Wrong length for a vector, should be 2')
		} else {
			p <- matrix(p, ncol=2) 
		}
	} else if (is.matrix(p)) {
		if (length(p[1,]) != 2) {
			stop( 'A points matrix should have 2 columns')
		}
		cn <- colnames(p)
		if (length(cn) == 2) {
			if (toupper(cn[1]) == 'Y' | toupper(cn[2]) == 'X')  {
				stop('Highly suspect column names (x and y reversed?)')
			}
			if (toupper(substr(cn[1],1,3) == 'LAT' | toupper(substr(cn[2],1,3)) == 'LON'))  {
				stop('Highly suspect column names (longitude and latitude reversed?)')
			}
		}		
	} else {
		stop('points should be vectors of length 2, matrices with 2 columns, or a SpatialPoints* object')
	}

	if (!latlon) {
		if  ( min(p[,1], na.rm=T) < -180 & max(p[,1], na.rm=T) > 180 & min(p[,2], na.rm=T) < -90 & max(p[,2], na.rm=T) > 90) { 
			stop('points are outside range for longitude / latitude')
		}
	}
	
	return(p)
}
	
	