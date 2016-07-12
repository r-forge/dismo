
circleHull <- function(xy, lonlat, ...) {

	crs  <- crs(xy)
	
	if (missing(lonlat)) {
		if (is.na(crs)) {
			lonlat <-  couldBeLonLat(xy)
			if (lonlat) {
				warning('crs unknown, assuming lonlat')
			}
		} else {
			lonlat <- isLonLat(crs)
		}
	}
	
	xy <- na.omit(unique(.pointsToMatrix(xy, checkLonLat=lonlat)))
	
	# arbitrary threshold to decide to simplify by 
	# first getting the points on the covex hull
	if (nrow(xy) > 25) {
		xy <- geom(polygons(covHull(xy)))[,5:6]
	}
	
	f <- function(p) { max(pointDistance(rbind(p), xy, lonlat=lonlat)) }
	p <- optim(colMeans(xy), f)
	if (is.na(crs)) crs <- CRS(as.character(NA))
	b <- buffer(SpatialPoints(rbind(p$par), proj4string=crs), width=p$value, quadsegs=45)
	SpatialPolygonsDataFrame(b, data.frame(x=p$par[1], y=p$par[2], r=p$value), match.ID = FALSE)
	
}


