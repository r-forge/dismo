# Download geographic data and return as R object
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.9
# October 2008

# compare some of this with geonames package, and perhaps use that instead. 


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
	
	

biogeomancer <- function(country='', adm1='', adm2='', locality='') {
	theurl <- paste("http://bg.berkeley.edu:8080/ws/single?cy=", country, "&sp=", adm1, "&co=", adm2, "&locality=", locality, sep='')
	doc <- xmlInternalTreeParse(theurl)
# to do: improved parsing:	
	nodes <- getNodeSet(doc, "//georeference")
	if(length(nodes) == 0)   return(data.frame())

	varNames <- c("DecimalLongitude", "DecimalLatitude", "GeodeticDatum", "CoordinateUncertaintyInMeters")
	dims <- c(length(nodes), length(varNames)) 
   # create an empty data frame with as many rows and columns as needed.
	ans <- as.data.frame(replicate(dims[2], rep(as.character(NA), dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
	names(ans) <- varNames
    # Fill in the rows based on the names.
	for(i in seq(length = dims[1])) 
			ans[i, varNames] = xmlSApply(nodes[[i]], xmlValue)[varNames]
	
	ans <- ans[,-3]
	names(ans) <- c("lon", "lat", "coordUncertaintyM")
	ans[which.min(ans[,"coordUncertaintyM"]),][1,]
}


alt <- function(lonlat) {
	lonlat <- .pointsToMatrix(lonlat)
	theurl <- paste("http://ws.geonames.org/srtm3?lat=", lonlat[,2], "&lng=", lonlat[,2], sep='')
	elevation <- scan(theurl, what='character', quiet=TRUE)
	if (elevation < -32000) { elevation <- NA }
	return(elevation)
}


country <- function(lonlat, radius=0) {
	cnts <- getData('ISO3')
	lonlat <- .pointsToMatrix(lonlat)

	res <- matrix(ncol=3,nrow=length(lonlat[,1]))
	for (i in 1:length(lonlat[,1])) {
		theurl <- paste("http://ws.geonames.org/countryCode?lat=", lonlat[i,2], "&lng=", lonlat[i,1], "&radius=", radius, sep='')
		country <- scan(theurl, what='character', quiet=TRUE)
		if (length(country) > 1) { res[i,] <- c(NA,NA,NA)
		} else {
			rec <- subset(cnts, cnts[,3] == country) 
			if (length(rec) == 0) { res[i,] <- c(NA,NA,NA) 
			} else res[i,] <- rec
		}	
	}	
	colnames(res) <- c("NAME_ENGLISH", "ISO3", "ISO2")
	return(res)
}


adm <- function(lonlat, radius=0, maxrows=1) {
	lonlat <- .pointsToMatrix(lonlat)
	theurl <- paste("http://ws.geonames.org/countrySubdivision?lat=", lonlat[,1], "&lng=", lonlat[,2], "&radius=", radius, "&maxrows=", maxrows, sep='')
	subdivs <- scan(theurl, what='character', quiet=TRUE)
	return(subdivs)
}


#http://ws.geonames.org/findNearbyPlaceName?lat=47.3&lng=9 
#http://ws.geonames.org/findNearby?lat=47.3&lng=9 
#http://ws.geonames.org/findNearbyWikipedia?lat=47&lng=9


