\name{evaluate}

\alias{evaluate}

\title{
Model testing
}

\description{
Model testing using AUC and other statistics.
}

\usage{
evaluate(model, p, a, x, tr)
}
 
\arguments{
  \item{model}{any fitted model, including objects inherting from 'DistModel'}
  \item{p}{presence points (x and y coordinates or SpatialPoints* object). 
             Or, if \code{x} is missing, values at presence points
			   Or, a matrix with values to compute predictions for}
		   
  \item{a}{absence points (x and y coordinates or SpatialPoints* object). 
			Or, if \code{x} is missing, values at presence points.
			Or, a matrix with values to compute predictions for}
			
  \item{x}{Optional. Predictor variables (object of class Raster*). If present, \code{p} and \code{a} are interpreted as (spatial) points }
  
  \item{tr}{Optional. a vector of threshold values to use for computing the confusion matrices}
}

\value{
An object of class ModelEvaluation
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

