\name{maxent}

\alias{maxent}

\alias{maxent,Raster,ANY-method}
\alias{maxent,SpatialGridDataFrame,ANY-method}
\alias{maxent,data.frame,vector-method}

\title{MaxEnt}

\description{

NOTE: This function will work after the next release of maxent. This currently does not work!

R link to the Maximum Entropy (maxent) species distribution model (see references below). 
First download 'maxent.jar' from \url{http://www.cs.princeton.edu/~schapire/maxent/} and put 
that in the 'java' folder of this package. That is the folder returned by \code{system.file("java", package="dismo")}. 
Please note that this program can not be redistributed or used for commercial purposes.
}

\usage{
maxent(x, p, ...)
}

\arguments{
\item{x}{Predictors. Raster* object or SpatialGridDataFrame; Or a data.frame, see Details}
\item{p}{Occurrence data. data.frame, matrix, SpatialPoints* object, vector, or missing. 
If it is a data.frame or matrix it must have two columns with the first being the x-coordinate (or longitude) and 
the second the y-coordinate (or latitude).  If missing, it is assumed that x is a data.frame with all the values to fit the model with}. 
If p is a vector, it should have a length equal to nrow(x) and contain values 0 (absence) and 1 (presence). 
\item{...}{Additional arguments. See Details}
}

\details{
if \code{x} is a \code{data.frame}, each column should be a predictor variable; and you also need to sup=ply an additional argument y (see below). 

Additional arguments:
\tabular{rll}{
\tab \code{a} \tab Absence points. Only if p is not NULL and not a vector. Should be of the same class as \code{p} \cr
\tab \code{factors} \tab Which (if any) variables should be considered as categorical? Either by (layer)name or by index. Only available when argument 
   \code{x} is a Raster* object because it is not needed in other cases as you can set the appropriate class to the variables in the data.frame \cr 
}

}

\value{
A RasterLayer object. Sometimes with the side effect of a file on disk.
}

\seealso{
\code{\link[dismo]{predict}}
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
predictors <- stack(list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE ))
occurence <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
occ <- read.table(occurence, header=TRUE, sep=',')[,-1]
me2 <- maxent(predictors, occ)
# predict to entire dataset
r2 <- predict(me2, predictors, progress='text') 

plot(r2)
points(occ)
}

\keyword{spatial}
