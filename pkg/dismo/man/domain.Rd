\name{domain}
\alias{domain}
\alias{domain,Raster,Spatial-method}
\alias{domain,Raster,matrix-method}
\alias{domain,Raster,data.frame-method}
\alias{domain,matrix,missing-method}
\alias{domain,data.frame,missing-method}


\title{
The Domain environmental similarity algorithm that has been extensively used for species distribution modeling.
}

\description{
.. description ...
}

\usage{
domain(x, p, ...)
}
 
\arguments{
  \item{x}{ Raster* object or matrix}
  \item{p}{ two column matrix or SpatialPoints* object}
  \item{...}{ Additional arguments }
}

\details{
The Domain algorithm computes the minimum Gower distance of environmental variables at a location to the values at any of the known 
locations of occurence ('training sites'). The maximum distance across environmental variables is used. This distance is substracted
from one, and low values are truncated at zero so that the scores are between 0 (low) and 1 (high).
}

\value{
A RasterLayer or (if x is a matrix), a vector.
}

\references{
Carpenter G., A.N. Gillison and J. Winter, 1993. Domain: a flexible modelling procedure for  mapping potential distributions of plants and animals. Biodiversity Conservation 2:667-680.
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

