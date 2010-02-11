

setClass('ConvexHull',
	contains = 'DistModel',
	representation (
		hull='matrix'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("convHull")) {
	setGeneric("convHull", function(p, ...)
		standardGeneric("convHull"))
}	


setMethod('convHull', signature(p='matrix'), 
	function(p, ...) {
		p <- p[,1:2]
		p <- unique(p)
		if (nrow(p) < 3) { stop('you need at least 3 (unique) points to make a convex hull') }
		h <- chull(p)
		h <- p[h,]
		h <- rbind(h, h[1,])
		ch <- new('ConvexHull')
		ch@presence <- p
		ch@hull <- h
		return(ch)
	}
)


setMethod('convHull', signature(p='data.frame'), 
	function(p, ...) {
		convHull(as.matrix(p))
	}
)

setMethod('convHull', signature(p='SpatialPoints'), 
	function(p, ...) {
		convHull(coordinates(p))
	}
)



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


