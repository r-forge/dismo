# Download geographic data and return as R object
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.1
# October 2008



biogeomancer <- function(country='', adm1='', adm2='', locality='', singleRecord=TRUE) {
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
	if (singleRecord) {
		ans <- ans[which.min(ans[,"coordUncertaintyM"]),][1,]
	}
	ans
}
