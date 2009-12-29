# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3

setClass('DistModel',
	contains = 'VIRTUAL',
	representation (
		presence = 'matrix',
		absence = 'matrix',
		hasabsence = 'logical'
	),	
	prototype (	
		presence = matrix(NA),
		absence = matrix(NA),
		hasabsence = FALSE
	),
	validity = function(object)	{
		if (object@hasabsence) {
			t1 <- ncol(object@presence) == ncol(object@absence)
			t2 <- sort(colnames(object@presence)) == sort(colnames(object@absence))
			return(t1 & t2)
		} else {
			return(TRUE)
		} 
	}
)	


