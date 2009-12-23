\name{maxent}
\alias{maxent}

\title{MaxEnt}

\description{
R link to MaxEnt species distribution model. You need to first donwload 'maxent.jar' from http://www.cs.princeton.edu/~schapire/maxent/ and put 
that in the 'java' folder of this package. That is folder \code{system.file("java", package="rmaxent")}. Note that this program can not be redistributed or use for comercial purposes.
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

The samples data.frame must consists of at least 3 columns: species, longitude (x), and latitude (y). Alternatively provide a valid maxent samples file.
}

\value{
None. Run for the side effect of files on disk
}

\author{Steven Phillips and Robert J. Hijmans}

\references{

\url{http://www.cs.princeton.edu/~schapire/maxent/}

Steven J. Phillips, Miroslav Dudik, Robert E. Schapire, 2004.  A maximum entropy approach to species distribution modeling. Proceedings of the Twenty-First International Conference on Machine Learning. p. 655-662.

Steven J. Phillips, Robert P. Anderson, Robert E. Schapire, 2006. Maximum entropy modeling of species geographic distributions. Ecological Modelling 190:231-259. 
}

\examples{
pfile <- system.file("/samples/bradypus.csv", package="rmaxent")
input <- paste(system.file(package="rmaxent"), "/layers", sep='')

samp <- read.table(pfile, header=TRUE, sep=',')
maxent(samp, input)
# or:  maxent(pfile, input) 

library(raster)
r <- raster("bradypus_variegatus.asc", native=TRUE)
plot(r)
points(samp[,2:3])
}

\keyword{spatial}
