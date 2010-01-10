\name{density}

\alias{density}
\alias{density,DistModel-method}

\docType{methods}

\title{density}

\description{
density plots
}

\usage{
density(x, ...)
}

\arguments{
\item{x}{Object of class DistModel or derived from that class (such as 'MaxEnt', 'Bioclim')}
\item{...}{Additional arguments. See Details}
}

\details{
An additional argument \code{v} can be supplied to select a subset of variables. Either by number, e.g. \code{v=1:4}, or by the names of the variables.
}

\value{
A density plot. Presence data are in red, and absence data (if available) are in blue.
}

\author{Robert J. Hijmans}


\keyword{spatial}
