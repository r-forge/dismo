\name{randomPoints}
\alias{randomPoints}

\title{
random points
}

\description{
Generate randome background values ("pseudo-absence").
}

\usage{
randomPoints(mask, n, p, ext = NULL, extf = 1.1, excludep = TRUE, tryf = 3)
}
 
\arguments{
  \item{mask}{Raster* object}
  \item{n}{Integer. Number of poinst}
  \item{p}{Presence points}
  \item{ext}{Exent}
  \item{extf}{}
  \item{excludep}{Logical. If \code{TRUE}, presence points are exluded from background}
  \item{tryf}{}
}


\value{
matrix
}

\author{
Robert J. Hijmans
}


\keyword{spatial}

