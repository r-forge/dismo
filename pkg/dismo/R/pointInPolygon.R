# Author: Robert J. Hijmans
# Date : Febrary 2010
# Version 0.1
# Licence GPL v3


.pointsInPolygons <- function(xy, polygons, fun=NULL) {
	stopifnot( require(rgeos) )
	#proj4string(xy) <- proj4string(polygons)
	result <- gIntersects(xy, polygons, byid=TRUE)
	if (! is.null(fun)) {
		result <- apply(result, 1, fun)
	}
	return(result)
}


# a =  pointInPolygon(xy, wrld_simpl)
# b = apply(a, 1, max)
# c = apply(a, 1, which.max)
# c[b==0] = NA

