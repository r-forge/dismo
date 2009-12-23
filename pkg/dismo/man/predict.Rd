\name{predict}

\docType{methods}

\alias{predict,Bioclim-method}
\alias{predict,Domain-method}
\alias{predict,Mahalanobis-method}

\title{Model predictions}

\description{
Make a RasterLayer with a prediction based on a model object. Provide a Raster* object with the independent variables. The layerNames in the RasterStack should include those expected by the model. 
}

\section{Methods}{
\describe{
 
\code{predict(object, x, filename='', progress='', ...)}
  
\tabular{rll}{  
  \tab  \code{model}  \tab A fitted model of class Bioclim, Domain, or Mahalanobis\cr
  \tab  \code{object} \tab A Raster* object \cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
  \tab  \code{...}  \tab Additional arguments (see below)\cr
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

\author{ Robert J. Hijmans}

\examples{
logo <- stack(system.file("pictures/Rlogo.jpg", package="rgdal"), bands=-1)
pts <- matrix(c(48.243420, 48.243420, 47.985820, 52.880230, 49.531423, 46.182616, 54.168232, 69.624263, 83.792291, 85.337894, 74.261072, 83.792291, 95.126713, 84.565092, 66.275456, 41.803408, 25.832176, 3.936132, 18.876962, 17.331359,7.048974, 13.648543, 26.093446, 28.544714, 39.104026, 44.572240, 51.171810, 56.262906, 46.269272, 38.161230, 30.618865, 21.945145, 34.390047, 59.656971, 69.839163, 73.233228, 63.239594, 45.892154, 43.252326, 28.356155), ncol=2)

b <- bioclim(logo, pts)
p <- predict(b, logo, progress='text')
plot(p)
points(pts)

#m <- mahal(logo, pts)
#rm <- predict(m, logo, progress='text')

#d <- domain(logo, pts)
#rd <- predict(d, logo, progress='text')
}

\keyword{methods}
\keyword{spatial}
