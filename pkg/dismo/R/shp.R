# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3


shp <- function(filename) {
	if (!(require(rgdal))) {
		stop('This functions requires the rgdal package; please install it')
	}
	fn <- basename(filename) 
	ext(fn) <- ''
	vec <- readOGR(filename, fn) 
	return(vec)
}

