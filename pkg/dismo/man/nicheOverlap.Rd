\name{nicheOverlap}

\alias{nicheOverlap}

\title{Niche Overlap}

\description{
Compute niche overlap from predictions of species' distributions with the I similarity statistic of Warren et al. (2009). The I similarity statistic ranges from a value of 0, where two distributions have no overlap, to 1 where two distributions are identical.
}

\usage{ nicheOverlap(x,y) }

\arguments{
\item{x}{RasterLayer}
\item{y}{RasterLayer}
}

\details{All values in \code{x} and \code{y} should be non-negative}

\value{numeric}

\author{Based on SDMTools::Istat by Jeremy VanDerWal}

\references{ Warren, D.L., R.E. Glor, M. Turelli, and D. Funk. 2009. Environmental niche equivalency versus conservatism: quantitative approaches to niche evolution. Evolution 62:2868-2883. }

\examples{
r1 <- raster(nr=18, nc=36)
r2 <- raster(nr=18, nc=36)
set.seed(0)
r1[] <- runif(ncell(r1))
r2[] <- runif(ncell(r1))
nicheOverlap(r1, r2)
}
