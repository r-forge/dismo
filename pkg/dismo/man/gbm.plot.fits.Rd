\name{gbm.plot.fits}

\Rdversion{1.1}

\alias{gbm.plot.fits}

\title{gbm plot fits}

\description{
Plots the fitted values from a gbm object returned by any of the model fitting options. This can give a more reliable guide to the shape of the fitted surface than can be obtained from the individual functions, particularly when predictor variables are correlated and/or samples are unevenly distributed in environmental space. Allows masking out of absences to enable focus on sites with high predicted values.
}

\usage{
gbm.plot.fits(gbm.object, mask.presence = FALSE, use.factor = FALSE, plot.layout = c(3, 4))
}

\arguments{
  \item{gbm.object}{a gbm object}
  \item{mask.presence}{Logical. If \code{TRUE}, the function only plots fitted values for presences}
  \item{use.factor}{Logical. If \code{TRUE}, forces to use quicker printing box and whisker plot}
  \item{plot.layout}{define the default layout for graphs on the page}
}

\references{
Elith, J., J.R. Leathwick and T. Hastie, 2009. A working guide to boosted regression trees. Journal of Animal Ecology 77: 802-81
}

\author{
John R. Leathwick and Jane Elith
}


\keyword{ spatial }

