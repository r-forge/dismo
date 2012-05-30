\name{mess}
\Rdversion{XXXXXXXXXX}
\alias{mess}
\title{Multivariate environmental similarity surfaces (MESS)}
\description{Computes the MESS index}
\usage{mess(X,V,full=TRUE)}
\arguments{
  \item{X}{A RasterStack object}
  \item{V}{A data frame containing the reference values. Each column should correspond to one layer of the RasterStack object.}
  \item{full}{If \code{full=TRUE} \code{mess} returns a RasterStack object with n layers corresponding to the index for each individual layer of the input RasterStack and an additioanl layer with the MESS values.
If \code{full=FALSE} \code{mess} returns a raster layer with the MESS values.}
}
\details{
\code{V} can be produced by the function \code{extract} from the package \code{raster}.
}

\value{
Either a RasterStack object with n layers corresponding to the values for each individual layer of the input RasterStack and an additional layer with the mess values (if \code{full=TRUE}) or a single Raster object with the MESS values (if \code{full=FALSE}).
}

\references{
Elith J., Kearney M., Phillips S. 2010. The art of modelling range-shifting species. Met.Ecol.Evol 1:330-342.
}

}
\author{
Jean-Pierre Rossi, <jean-pierre.rossi@supagro.inra.fr>
}


\examples{
r1 <- raster(ncol=10, nrow=10) ; r1[] <- (1:ncell(r1))/10 + rnorm(ncell(r1))
r2 <- raster(ncol=10, nrow=10) ; r2[] <- (1:ncell(r1))/10 + rnorm(ncell(r2))
r3 <- raster(ncol=10, nrow=10) ; r3[] <- (1:ncell(r1))/10 + rnorm(ncell(r3))

rasters<-stack(r1,r2,r3)
plot(rasters)
xy<-expand.grid(c(10,30,50),c(10,30,50))
#plot(r1) ; points(xy)
refpt<-extract(rasters,xy)
mess<-mess(X=rasters,V=refpt,full=TRUE)
plot(mess)
}

