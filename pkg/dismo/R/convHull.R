# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : Febrary 2010
# Version 0.1
# Licence GPL v3


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

