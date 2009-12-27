\name{maxent}
\alias{maxent}
\alias{maxent,data.frame,missing-method}
\alias{maxent,Raster,matrix-method}
\alias{maxent,Raster,data.frame-method}
\alias{maxent,Raster,SpatialPoints-method}
\alias{maxent,SpatialGridDataFrame,matrix-method}
\alias{maxent,SpatialGridDataFrame,data.frame-method}
\alias{maxent,SpatialGridDataFrame,SpatialPoints-method}

\title{MaxEnt}

\description{
R link to MaxEnt species distribution model. You need to first donwload 'maxent.jar' from http://www.cs.princeton.edu/~schapire/maxent/ and put 
that in the 'java' folder of this package. That is folder returned by \code{system.file("java", package="rmaxent")}. 
Please note that this program can not be redistributed or used for comercial purposes.
}

\usage{
maxent(x, p, ...)
}

\arguments{
\item{x}{data.frame or Character (file name); see Details}
\item{p}{Character. Folder with predictors}
\item{...}{Additional arguments. See Details}
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
#The R logo
#predictor variables
logo <- stack(system.file("pictures/Rlogo.jpg", package="rgdal"), bands=-1)
layerNames(logo) <- c('red', 'green', 'blue')

#presence points
presence <- matrix(c(48.243420, 48.243420, 47.985820, 52.880230, 49.531423, 46.182616, 54.168232, 69.624263, 83.792291, 85.337894, 74.261072, 83.792291, 95.126713,
 84.565092, 66.275456, 41.803408, 25.832176, 3.936132, 18.876962, 17.331359,7.048974, 13.648543, 26.093446, 28.544714, 39.104026, 44.572240, 51.171810, 56.262906,
 46.269272, 38.161230, 30.618865, 21.945145, 34.390047, 59.656971, 69.839163, 73.233228, 63.239594, 45.892154, 43.252326, 28.356155), ncol=2)

#fit a model 
me <- maxent(logo, presence)
# predict to entire dataset
r <- predict(me, logo, progress='text') 

plot(r)
points(presence)

# a real species:
predictors <- stack(list.files(path=paste(system.file(package="rmaxent"), '/layers', sep=''), pattern='grd', full.names=TRUE ))
occurence <- paste(system.file(package="rmaxent"), '/samples/bradypus.csv', sep='')
occ <- read.table(occurence, header=TRUE, sep=',')[,-1]
me2 <- maxent(predictors, occ)
# predict to entire dataset
r2 <- predict(me2, predictors, progress='text') 

plot(r2)
points(occ)
}

\keyword{spatial}
