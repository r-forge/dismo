# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3

gbif <- function(genus, species='', geo=TRUE, sp=FALSE, removeZeros=TRUE) {

	if (! require(XML)) { stop() }

	gbifxmlToDataFrame <- function(s) {
		# this sub-funciton was hacked from xmlToDataFrame in the XML package by Duncan Temple Lang
		doc = xmlInternalTreeParse(s)
		nodes <- getNodeSet(doc, "//to:TaxonOccurrence")
		if(length(nodes) == 0)   return(data.frame())
		varNames <- c("continent", "country", "stateProvince", "county", "locality",  "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "maximumElevationInMeters", "minimumElevationInMeters", "maximumDepthInMeters", "minimumDepthInMeters", "institutionCode", "collectionCode", "catalogNumber",  "basisOfRecordString", "collector", "earliestDateCollected", "latestDateCollected",  "gbifNotes")
		dims <- c(length(nodes), length(varNames)) 
   # create an empty data frame with as many rows and columns as needed.
		ans <- as.data.frame(replicate(dims[2], rep(as.character(NA), dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
		names(ans) <- varNames
    # Fill in the rows based on the names.
		for(i in seq(length = dims[1])) 
			ans[i, varNames] <- xmlSApply(nodes[[i]], xmlValue)[varNames]

		nodes <- getNodeSet(doc, "//to:Identification")
		varNames <- c("taxonName")
		dims = c(length(nodes), length(varNames)) 
		tax = as.data.frame(replicate(dims[2], rep(as.character(NA), dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
		names(tax) = varNames
    # Fill in the rows based on the names.
		for(i in seq(length = dims[1])) 
			tax[i, varNames] = xmlSApply(nodes[[i]], xmlValue)[varNames]

		cbind(tax, ans)
	}



    spec <- paste('scientificname=', trim(genus),'+', trim(species), sep='')
	if (geo) { cds <- '&coordinatestatus=true' 
	} else { cds <- '' }
    base <- 'http://data.gbif.org/ws/rest/occurrence/'
    url <- paste(base, 'count?', spec, cds, sep='')
    x <- readLines(url, warn=FALSE)
    x <- x[substr(x, 1, 20) == "<gbif:summary totalM"]
    n <- as.integer(unlist(strsplit(x, '\"'))[2])
    if (n==0) {
        cat('no occurrences found\n')
        return(invisible(NULL))
    } else {
		cat(n, 'occurrences found\n')
		flush.console()
	}
    iter <- n %/% 1000
	first <- TRUE
    for (group in 0:iter) {
        start <- group * 1000
        if (group == iter) { end <- n } else { end <- start + 999 }
        cat('downloading records', start+1, 'to', end+1, '\n')
        flush.console()
        aurl <- paste(base, 'list?', spec, '&format=darwin&startindex=', start, cds, sep='')
		s <- readLines(aurl, warn=FALSE)
		
        test <- try(zz <- gbifxmlToDataFrame(s), silent=TRUE)
		if (class(test) == 'try-error') {
		# sofar the only unacceptable character found
			s <- sub("\002", "", s)
			zz <- gbifxmlToDataFrame(s)
		}
		if (first) {
			z <- zz
			first <- FALSE
		} else {
			z <- rbind(z, zz)
		}
	}

	d <- as.Date(Sys.time())
	z <- cbind(z, d)
	names(z) <- c("species", "continent", "country", "adm1", "adm2", "locality", "lat", "lon", "coordUncertaintyM", "maxElevationM", "minElevationM", "maxDepthM", "minDepthM", "institution", "collection", "catalogNumber",  "basisOfRecord", "collector", "earliestDateCollected", "latestDateCollected",  "gbifNotes", "downloadDate")
	z[,'lon'] <- as.numeric(z[,'lon'])
	z[,'lat'] <- as.numeric(z[,'lat'])
	
	if (removeZeros) {
		z <- subset(z, z[,'lon'] != 0 & z[,'lat'] !=0 )
	} else {
		z <- subset(z, !(z[,'lon'] == 0 & z[,'lat'] == 0) )
	}
		
	if (sp & geo) {
		if (dim(z)[1] > 0)
		coordinates(z) <- ~lon+lat
	}
	return(z)
}

#sa <- gbif('solanum')
#sa <- gbif('solanum', 'acaule', sp=TRUE)

