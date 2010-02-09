# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3



setClass('Domain',
	contains = 'DistModel',
	representation (
		range='vector'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("domain")) {
	setGeneric("domain", function(x, p, ...)
		standardGeneric("domain"))
}	


setMethod('domain', signature(x='Raster', p='matrix'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		domain(m)
	}
)

setMethod('domain', signature(x='Raster', p='data.frame'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		domain(m)
	}
)

setMethod('domain', signature(x='data.frame', p='missing'), 
	function(x, p, ...) {
		domain(as.matrix(x))
	}
)

setMethod('domain', signature(x='matrix', p='missing'), 
	function(x, p, ...) {
		d <- new('Domain')
		d@presence <- x
		r <- apply(x, 2, FUN=function(x){range(x, na.rm=TRUE)})
		d@range <-  abs(r[2,] - r[1,])
		d
	}
)

setMethod('domain', signature(x='Raster', p='SpatialPoints'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		domain(m)
	}
)


