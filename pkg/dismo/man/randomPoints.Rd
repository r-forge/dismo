\name{randomPoints}
\alias{randomPoints}

\title{
random points
}

\description{
Generate randome background values ("pseudo-absence").
}

\usage{
randomPoints(mask, n, p, ext = NULL, extf = 1.1, excludep = TRUE, tryf=5)
}
 
\arguments{
  \item{mask}{Raster* object; Cells with NA in this object (or first layer of the object) are excluded}
  \item{n}{Integer. Number of points}
  \item{p}{Presence points (if provided, random points won't be in the same cells (as defined by mask)}
  \item{ext}{Exent; to restrict sampling to a spatial extent}
  \item{extf}{Numeric. Multiplyer to make the change extent ext }
  \item{excludep}{Logical. If \code{TRUE}, presence points are exluded from background}
  \item{tryf}{numeric > 1. Multiplyer used for initial sample size from which the requested sample size is extracted after removing rows NA points (outside of mask) }
}

\value{
matrix
}

\author{
Robert J. Hijmans
}


\keyword{spatial}

