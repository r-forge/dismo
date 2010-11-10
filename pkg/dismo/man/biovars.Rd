\name{biovars}

\docType{methods}

\alias{biovars}
\alias{biovars,matrix,matrix,matrix-method}
\alias{biovars,Raster,Raster,Raster-method}
\alias{biovars,vector,vector,vector-method}

\title{bioclimatic variables}

\description{
Function to create 'bioclimatic variables' from monthly climate data. 
}

\usage{
biovars(prec, tmin, tmax, ...) 
}

\arguments{
  \item{prec}{vector, matrix, or RasterStack/Brick of precipitation data}
  \item{tmin}{vector, matrix, or RasterStack/Brick of minimum temperature data}
  \item{tmax}{vector, matrix, or RasterStack/Brick of maximum temperature data}
  \item{...}{ Additional arguments }
}

\details{
Input data is normaly monthly. I.e. there should be 12 values (layers) for each variable, but the function should also work for e.g. weekly data (with some changes in the meaning of the output variables. E.g. #8 would then not be for a quater (3 months), but for a 3 week period). 
}

\value{
Depending on the class of the input data, an object of class 'vector', 'matrix' or 'RasterBrick' with 19 variables (columns, layers)

BIO1 = Mean annual temperature

BIO2 = Mean diurnal range (mean of max temp - min temp)

BIO3 = Isothermality (BIO2/BIO7) (* 100)

BIO4 = Temperature seasonality (standard deviation *100)

BIO5 = Max temperature of warmest month

BIO6 = Min temperature of coldest month

BIO7 = Temperature annual range (BIO5-BIO6)

BIO8 = Mean temperature of the wettest quarter

BIO9 = Mean temperature of driest quarter 

BIO10 = Mean temperature of warmest quarter

BIO11 = Mean temperature of coldest quarter

BIO12 = Total (annual) precipitation

BIO13 = Precipitation of wettest month

BIO14 = Precipitation of driest month

BIO15 = Precipitation seasonality (coefficient of variation)

BIO16 = Precipitation of wettest quarter

BIO17 = Precipitation of driest quarter

BIO18 = Precipitation of warmest quarter
}

\author{
Robert J. Hijmans
}

\examples{
tmin <- c(10,12,14,16,18,20,22,21,19,17,15,12)
tmax <- tmin + 5
prec <- c(0,2,10,30,80,160,80,20,40,60,20,0)
biovars(prec, tmin, tmax)
}


\keyword{spatial}

