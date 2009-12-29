\name{predict}

\docType{methods}

\alias{predict,MaxEnt-method}

\title{Model predictions}

\description{
Make a RasterLayer with a prediction based on a MaxEnt fitted model object. Provide a Raster* object with the independent variables. The layerNames in the Raster* object should include those expected by the model. 
}

\section{Methods}{
\describe{
 
\code{predict(object, x, filename='', progress='', ...)}
  
\tabular{rll}{  
  \tab  \code{model}  \tab A fitted model of class MaxEnt\cr
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


\keyword{methods}
\keyword{spatial}
