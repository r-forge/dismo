# Author: Robert J. Hijmans
# based on function Istat in package SDMTools by Jeremy VanDerWal
# Date : October 2012
# Version 1.0
# Licence GPL v3


nicheOverlap <- function (x, y) {
	compareRaster(x,y)
	stopifnot(cellStats(x, 'min') >= 0)
	stopifnot(cellStats(y, 'min') >= 0)
    rx <- x/cellStats(x, 'sum')
    ry <- y/cellStats(y, 'sum')
	r <- overlay(rx, ry, fun=function(i,j) (sqrt(i) - sqrt(j))^2)
	1 - 0.5 * sqrt(cellStats(r, 'sum'))
}

