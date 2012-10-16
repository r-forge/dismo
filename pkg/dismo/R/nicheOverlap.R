# based on function Istat in package SDMTools by Jeremy VanDerWal
# adapted by Robert J. Hijmans 
# Date : October 2012
# Version 1.0
# Licence GPL v3


nicheOverlap <- function (x, y) {
	s <- stack(x, y)

	# to assure that both have the same NA cells
	s <- mask(s,  x * y)
	
	minv <- cellStats(s, 'min', na.rm=TRUE)
	if (any(minv < 0)) {
		stop('values of "x" and "y" should be non-negative')
	}
	
    s <- s / cellStats(s, 'sum', na.rm=TRUE)
	r <- overlay(s, fun=function(i,j) (sqrt(i) - sqrt(j))^2)
	1 - 0.5 * sqrt(cellStats(r, 'sum', na.rm=TRUE))
}

