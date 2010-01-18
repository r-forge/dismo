\name{density}

\alias{density}
\alias{density,DistModel-method}

\docType{methods}

\title{density}

\description{
Create a density plots of presence and absence data
}

\usage{
density(x, ...)
}

\arguments{
\item{x}{Object of class 'ModelEvaluation' or of a class that inherits from 'DistModel, (such as 'MaxEnt', 'Bioclim')}
\item{...}{Additional arguments passed to plot}
}

\value{
A density plot. Presence data are in red, and absence data (if available) are in blue.
}

\author{Robert J. Hijmans}


\keyword{spatial}
