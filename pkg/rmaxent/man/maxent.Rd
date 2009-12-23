\name{maxent}
\alias{maxent}

\title{MaxEnt}

\description{
  R link to MaxEnt species distribution model
}

\usage{
  maxent(samples, input, output=getwd())
}

\arguments{
\item{samples}{data.frame or Character (file name); see Details}
\item{input}{Character. Folder with predictors}
\item{output}{Character. Ouput folder}
}


\details{
This function runs the maxent model by Steven Phillips and colleagues (see references below). You can download the model and documentation here: \url{http://www.cs.princeton.edu/~schapire/maxent/}

The samples data.frame must consists of at least 3 columns: species, longitude (x), and latitude (y)
}

\value{
none. Run for the side effect of files on disk
}


\author{Steven Phillips and Robert J. Hijmans}


\references{

\url{http://www.cs.princeton.edu/~schapire/maxent/}

Steven J. Phillips, Miroslav Dudik, Robert E. Schapire.  A maximum entropy approach to species distribution modeling. In Proceedings of the Twenty-First International Conference on Machine Learning, pages 655-662, 2004.

Steven J. Phillips, Robert P. Anderson, Robert E. Schapire. Maximum entropy modeling of species geographic distributions. Ecological Modelling, 190:231-259, 2006. 
}

\examples{
pfile <- paste(system.file(package="rmaxent"), "/samples/bradypus.csv", sep='')
samp <- read.table(samp, header=TRUE, sep=',')

input <- paste(system.file(package="rmaxent"), "/layers", sep='')
maxent(samp, input)
# or run:  maxent(pfile, input) 

library(raster)
r <- raster("bradypus_variegatus.asc")
plot(r)
points(samp[,2:3])
	
}


\keyword{spatial}