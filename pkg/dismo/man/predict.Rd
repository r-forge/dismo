\name{predict}

\docType{methods}

\alias{predict}
\alias{predict,Bioclim-method}
\alias{predict,Domain-method}
\alias{predict,Mahalanobis-method}
\alias{predict,MaxEnt-method}
\alias{predict,ConvexHull-method}

\title{Distribution model predictions}

\description{
Make a RasterLayer with a prediction based on a model object. Provide a Raster* object with the independent variables. The layerNames in the Raster* object should include those expected by the model. 
}

\section{Methods}{
\describe{
\code{predict(object, x, ext=NULL, filename='', progress='text', ...)}
  
\tabular{rll}{  
  \tab  \code{object}  \tab A fitted model of class Bioclim, Domain, MaxEnt, ConvexHull, or Mahalanobis (classes that inherit from DistModel)\cr
  \tab  \code{x} \tab A Raster* object \cr
  \tab  \code{ext} \tab An Extent object that can be used to make a prediction for a sub-region of \code{x}. You can also supply an 
  object that can be coerced to an Extent object by \code{\link[raster]{extent}}; such as a Raster* or Spatial* object \cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab \code{progress} \tab Character. Valid values are "" (no progress bar), "text", "tcltk", and "windows" (on that platform only) \cr
  \tab  \code{...}  \tab Additional arguments for file writing (see below)\cr
}

The following additional arguments can be passed, to replace default values, when writing the results to a file
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If TRUE, "filename" will be overwritten if it exists \cr
 }

}}

\value{
A RasterLayer or, if \code{x} is a matrix), a vector. 
}

\seealso{
For spatial predictions with GLM, GAM, BRT, randomForest, etc., see \link[raster]{predict} in the Raster package.

To fit models related to this predict method, see \code{ \link[dismo]{maxent}, \link[dismo]{bioclim}, \link[dismo]{mahal}, \link[dismo]{domain}, \link[dismo]{convHull}}
}

\author{ Robert J. Hijmans}

\examples{
logo <- stack(system.file("pictures/Rlogo.jpg", package="rgdal"), bands=-1)
pts <- matrix(c(48.243420, 48.243420, 47.985820, 52.880230, 49.531423, 46.182616, 54.168232, 69.624263, 83.792291, 85.337894, 74.261072, 83.792291, 95.126713, 
            84.565092, 66.275456, 41.803408, 25.832176, 3.936132, 18.876962, 17.331359,7.048974, 13.648543, 26.093446, 28.544714, 39.104026, 44.572240, 51.171810,
			56.262906, 46.269272, 38.161230, 30.618865, 21.945145, 34.390047, 59.656971, 69.839163, 73.233228, 63.239594, 45.892154, 43.252326, 28.356155), ncol=2)
b <- bioclim(logo, pts)
# prediction for a sub-region
e <- extent(30,90,20,60)
p <- predict(b, logo, progress='text', ext=e)
plot(p)
}

\keyword{methods}
\keyword{spatial}
