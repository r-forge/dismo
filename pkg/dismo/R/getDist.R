# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : Febrary 2010
# Version 0.1
# Licence GPL v3


setClass('GeographicDistance',
	contains = 'DistModel',
	representation (
		presence='matrix',
		absence='matrix'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("geoDist")) {
	setGeneric("geoDist", function(p, ...)
		standardGeneric("geoDist"))
}	


setMethod('geoDist', signature(p='matrix'), 
	function(p, a, ...) {
		gd <- new('GeographicDistance')
		gd@presence <- p
		if (! missing(a)) {
			gd@absence <- a
		}
		return(gd)
	}
)


setMethod('geoDist', signature(p='data.frame'), 
	function(p, a, ...) {
		p <- as.matrix(p)
		if (missing(a)) { 
			geoDist(p, ...) 
		} else {
			geoDist(p, a=as.matrix(a), ...)
		}
	}
)

setMethod('geoDist', signature(p='SpatialPoints'), 
	function(p, a, ...) {
		p <- coordinates(p)
		if (missing(a)) { 
			geoDist(p, ...) 
		} else {
			geoDist(p, a=coordinates(a), ...)
		}
	}
)

