# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : Febrary 2010
# Version 0.1
# Licence GPL v3



if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='ConvexHull'), 
	function(object, x, ext=NULL, filename='', progress='text', ...) {
	
		pol = SpatialPolygons( list(  Polygons(list(Polygon(object@hull)), 1)) )
		
		if ( extends(class(x), 'Raster'))  {
			x = raster(x)
			if (! is.null(ext)) { x = crop(x, ext) }
			x = polygonsToRaster(pol, x, field=0, overlap='last', mask=FALSE, updateRaster=FALSE, updateValue="NA", getCover=FALSE, filename=filename, silent=TRUE, progress=progress, ...)
			return(x)
		} else {
			if (! inherits(x, 'SpatialPoints') )  {
				x = data.frame(x[,1:2])
				colnames(x) = c('x', 'y')
				coordinates(x) = ~ x + y
			}
			v = overlay(x, pol)
			v[is.na(v)] <- 0
			return(v)
		}
	}
)


