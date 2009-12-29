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



setClass('MaxEnt',
	contains = 'DistModel',
	representation (
		lambdas  = 'vector'
	),	
	prototype (	
		lambdas = as.vector(NA)
	),
)



setMethod ('show' , 'MaxEnt', 
	function(object) {
		cat('class    :' , class(object), '\n\n')
		cat('variables:', colnames(object@presence), '\n\n')
		cat('lambdas\n')
		print(object@lambdas)
		cat('\n')
		pp <- nrow(object@presence)
		cat('\npresence points:', pp, '\n')
		if (pp < 20) {
			print(object@presence)
		} else {
			print(object@presence[1:10,])
			cat('\n')
			cat('  (... ...  ...)\n')
			cat('\n')
		}
		pp <- nrow(object@absence)
		cat('\nabsence points:', pp, '\n')
		if (pp < 25) {
			print(object@absence)
		} else {
			print(object@absence[1:10,])
			cat('\n')
			cat('  (... ...  ...)\n')
			cat('\n')
		}
	}
)	


