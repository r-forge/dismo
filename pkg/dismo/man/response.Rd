\name{response}

\alias{response}
\alias{response,DistModel-method}


\title{response plots}


\description{
Generate 'response plots', i.e. single variable response curves for a model
}


\usage{
response(x, ...) 
}


\arguments{
  \item{x}{ Model object that inherits from 'DistModel', e.g. 'MaxEnt' }
  \item{...}{ Additional arguments. See Details }  
}


\details{  
\tabular{rll}{  
\tab \code{var} \tab Variable to be plotted (if NULL, all variables will be plotted) \cr
\tab \code{at}  \tab Function to indicate at what level the other variables should be. E.g. median (the default), mean, min, max. Note that currently the function fails when 'mean' is used and one of the variables is a factor. Instead of a function you can also supply a single-row matrix with values for each of the variables \cr
\tab \code{rug} \tab Logical. If \code{TRUE} (the default) a 'rug' of deciles is plotted on the horizontal axes) \cr
\tab \code{...}  \tab Additional graphical parameters  \cr
}
}


\seealso{
\code{\link[dismo]{density}, \link[dismo]{plot}, \link[dismo]{pairs}}
}


\value{
Used for the side-effect of a plot
}

\author{
Robert J. Hijmans
}

\keyword{spatial}