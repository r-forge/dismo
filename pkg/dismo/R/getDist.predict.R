# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : Febrary 2010
# Version 0.1
# Licence GPL v3


setMethod('predict', signature(object='GeographicDistance'), 
	function(object, x, ext=NULL, filename='', mask=FALSE, progress='text', ...) {
	
		if ( extends(class(x), 'Raster'))  {
			if (! mask) {
				x = raster(x)
			}
			if (! is.null(ext)) { x = crop(x, ext) }
			
			xx <- distanceFromPoints(x, object@presence)
			if (mask) {
				xx <- mask(xx, x)
			}
			return(xx)
			
		} else {
		
			if ( inherits(x, 'SpatialPoints') )  { x = coordinates(x) }
			
			res <- vector(length=nrow(x))
			for (i in nrow(x)) {
				res[i] <- min( pointDistance(x[i,], object@presence) )
			}
			
			return(res)
		}
		
	}
)

