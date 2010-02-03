\name{maxent}

\alias{maxent}

\alias{maxent,Raster,ANY-method}
\alias{maxent,SpatialGridDataFrame,ANY-method}
\alias{maxent,data.frame,vector-method}

\title{MaxEnt}

\description{
This function calls the Maximum Entropy (maxent) species distribution model (see references below). Maximum Entropy is a generic machine learning method, but in the context of this function it is treated as a species distribution model. The function uses presence and absence (or random 'background') data for a number of locations, and the values of a number of predictors (independent variables) for those locations. The result is a predictive model that can be used to predict the suitability of other locations, for example to predict the entire range of a species, or its potential range in another region, or under changed conditions. 

For this function to work, you must first download the maxent software from \url{http://www.cs.princeton.edu/~schapire/maxent/} and put 
the file 'maxent.jar' in the 'java' folder of this package. That is the folder returned by \code{system.file("java", package="dismo")}. 
Please note that this program can not be redistributed or used for commercial purposes.
}

\usage{
maxent(x, p, ...)
}

\arguments{
\item{x}{Predictors. Raster* object or SpatialGridDataFrame, containing grids with predictor variables. These will be used to extract values from for the point locations. \code{x} can also be a data.frame, in which case each column should be a predictor variable and each row a presence or absence record. }

\item{p}{Occurrence data. This can be a data.frame, matrix, SpatialPoints* object, or a vector 
If \code{p} is a data.frame or matrix it represents a set of point locations; and the object must have two columns with the first being the x-coordinate (longitude) and the second the y-coordinate (latitude). Coordinates can also be specified with a SpatialPoints* object (defined in the sp package). Background (absence) points are sampled randomly from the cells that are not NA in the first predictor variable, unless absence points are specified with an additional argument \code{a} (see Details).}

if \code{x} is a data.frame, \code{p} should be a vector with a length equal to \code{nrow(x)} and contain 0 (absence) and 1 (presence) values, to indicate which records (rows) in data.frame \code{x} are presence records, and which are absence records. 

\item{...}{Additional arguments. See Details}
}

\details{
Additional arguments:
\tabular{rll}{
\tab \code{a} \tab Absence points. Only used if \code{p} is not missing, and not a vector.  \cr
\tab \code{factors} \tab Which (if any) variables should be considered as categorical? Either by (layer)name or by index. Only used when argument \code{x} is a Raster* object because it is not needed in other cases as you can set the appropriate class to the variables in the data.frame \cr 
\tab \code{args} \tab Additional argument that can be passed to maxent   \cr
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
# get predictor variables
predictors <- stack(list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE ))
# file with presence points
occurence <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
occ <- read.table(occurence, header=TRUE, sep=',')[,-1]
# witholding a 20% sample for testing 
fold <- kfold(occ, k=5)
occtrain <- occ[fold != 1, ]
# fit model
me <- maxent(predictors, occ)
# plot showing importance of each variable
plot(me)
# predict to entire dataset
r <- predict(me, predictors, progress='text') 
plot(r)
points(occ)

# equivalent to:
# extract values
pv <- data.frame(xyValues(predictors, occ))
fold <- kfold(pv, k=5)
pvtrain <- pv[fold != 1, ]
pvtest <- pv[fold == 1, ]
# background data
abs <- randomPoints(predictors, 5000)
av <- data.frame(xyValues(predictors, abs))
fold <- kfold(av, k=5)
avtrain <- av[fold != 1, ]
avtest <- av[fold == 1, ]
# combine train presence and absence data
train <- rbind(pvtrain, avtrain)
# vector indicating presence or absence
p <- c(rep(1, nrow(pvtrain)), rep(0, nrow(avtrain)))
# fit maxent model
me2 <- maxent(x=train, p=p)
# predict to training points
trainp <- predict(me, pvtrain) 
traina <- predict(me, avtrain) 
evaluate(p=trainp, a=traina)

# predict to testing points 
testp <- predict(me, pvtest) 
testa <- predict(me, avtest) 
evaluate(p=testp, a=testa)

# predict to entire dataset
# r <- predict(me, predictors, progress='text') 
# plot(r)
}

\keyword{spatial}

