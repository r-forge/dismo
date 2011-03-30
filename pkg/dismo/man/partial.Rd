\name{partial}

\alias{partial}

\title{Partial plots}

\description{
Generate 'partial plots', i.e. single variable response curves for a model
}

\usage{
partial(model, var=NULL, at=median) 
}

\arguments{
  \item{model}{ Model object that inherits from 'DistModel', e.g. 'MaxEnt'}
  \item{var}{ Variable to be plotted (if NULL, all variables will be plotted) }
  \item{at}{ Function to indicate at what level the other variables should be. E.g. median, mean, min, max. Note that mean fails when one of the variables is a factor. Or a matrix or named vector with values for each of the variables (except for variabls var) }
}

\value{
Used for the side-effect of a plot
}

\author{
Robert J. Hijmans
}

\keyword{spatial}
