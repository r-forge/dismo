# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : March 2010
# Version 0.1
# Licence GPL v3

disData <- function(...) {
	args <- unique(toupper(unlist(list(...))))
	if (length(args) == 0) { 
		args = 'ALL' 
	}
	if (args[1] == 'ALL') {
		args <- c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')
	}
	path = system.file("data", package="disdat")
	for (i in 1:length(args)) {
		r = toupper(args[[i]])
		if (! r %in% c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')) {
			warning('unknown region:', r)
		} else {
			d = paste(path, '/', r, c('train', 'background', 'test'), '.RData', sep='')
			for (j in 1:3) {
				res <- try( load(d[j], envir = .GlobalEnv) )
			}
		}
	}
}


getDisData <- function(region, type) {
	region = toupper(region[1])
	type = tolower(type[1])
	regions = c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')
	if (! region %in% regions) {
		stop('unknown region: ', region, '. Should be one of: ', regions)
	}
	types = c('train', 'test', 'background')
	if (! type %in% types ) {
		stop('unknown data type: ', type, '. Shoud be one of: ', types)
	}
	path = system.file("data", package="disdat")
	x = paste(path, '/', region, type, '.RData', sep='')
	thisenvir = new.env()
	d <- get(load(x, thisenvir), thisenvir)
	return(d)
}
