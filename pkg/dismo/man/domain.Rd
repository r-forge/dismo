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
An object of class 'Domain'
}


\examples{
logo <- stack(system.file("pictures/Rlogo.jpg", package="rgdal"), bands=-1)
#presence data
pts <- matrix(c(48.243420, 48.243420, 47.985820, 52.880230, 49.531423, 46.182616, 54.168232, 69.624263, 83.792291, 85.337894, 74.261072, 83.792291, 95.126713, 84.565092, 66.275456, 41.803408, 25.832176, 3.936132, 18.876962, 17.331359,7.048974, 13.648543, 26.093446, 28.544714, 39.104026, 44.572240, 51.171810, 56.262906, 46.269272, 38.161230, 30.618865, 21.945145, 34.390047, 59.656971, 69.839163, 73.233228, 63.239594, 45.892154, 43.252326, 28.356155), ncol=2)
d <- domain(logo, pts)
}

\seealso{
\code{\link[dismo]{predict}}, \code{\link[dismo]{bioclim}}, \code{\link[dismo]{mahal}}
}

\references{
Carpenter G., A.N. Gillison and J. Winter, 1993. Domain: a flexible modelling procedure for  mapping potential distributions of plants and animals. Biodiversity Conservation 2:667-680.
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

