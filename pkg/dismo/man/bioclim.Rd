\name{bioclim}

\alias{bioclim}
\alias{bioclim,Raster,Spatial-method}
\alias{bioclim,Raster,matrix-method}
\alias{bioclim,Raster,data.frame-method}
\alias{bioclim,matrix,missing-method}
\alias{bioclim,data.frame,missing-method}

\title{bioclim}

\description{
The BIOCLIM environmental similarity algorithm that has been extensively used for species distribution modeling.
}


\usage{
bioclim(x, p, ...)
}
 

\arguments{
  \item{x}{ Raster* object or matrix}
  \item{p}{ two column matrix or SpatialPoints* object}
  \item{...}{ Additional arguments }
}


\details{
BIOCLIM is a classic 'climate-envelope-model'. Although it does rather poorly in species distribution model comparisons (Elith et al. 2006, Hijmans and Graham, 2006), 
it is still used, among other reasons because the algorith is easy to understand and thus useful in teaching species distribution modeling.
The BIOCLIM algorithm computes the similarity of a location by comparing the values of environmental variables at the location to percentile 
distributions of the values at known locations of occurence ('training sites'). 
The closer to the 50th percentile (the median), the more similar the location is. The two tails are not distinguished, i.e., 10 percentile is
equivalent to 90 percentile. In this implementation in R, the values upper tail values are transformed to the lower tail, and the minimum percentile 
score across all the environmental variables is used. This value is substracted from 1 and then mutlipled with two so that the results are between 0 and 1. 
The reason for scaling this way is that the results become more like that of other distributon modeling methods and are thus easoer to interpret.
The value 1 will rarely be observed as it would require a location that has the median value of the training data for all the variables considered. The value 0 
is very common as it is assinged to all cells with a value of an environmental variable that is outside the percentile distribtions (the range of the training data).

DIVA-GIS (\url{www.diva-gis.org}) is a windows (and mac with some effort) program that has also implemented bioclim together with a number of methods
to visually explore the environmental distribution of sets of points in the context of the bioclim model./
}


\value{
A RasterLayer or (if \code{x} is a matrix), a vector.
}


\references{
Nix, H.A., 1986. A biogeographic analysis of Australian elapid snakes. In: Atlas of Elapid Snakes of Australia. (Ed.) R. Longmore, pp. 4-15. Australian Flora and Fauna Series Number 7. Australian Government Publishing Service: Canberra.

Elith, J., C.H. Graham, R.P. Anderson, M. Dudik, S. Ferrier, A. Guisan, R.J. Hijmans, F. Huettmann, J. Leathwick, A. Lehmann, J. Li, L.G. Lohmann, B. Loiselle, 
G. Manion, C. Moritz, M. Nakamura, Y. Nakazawa, J. McC. Overton, A.T. Peterson, S. Phillips, K. Richardson, R. Scachetti-Pereira, R. Schapire, J. Soberon, 
S. Williams, M. Wisz and N. Zimmerman, 2006. 
Novel methods improve prediction of species' distributions from occurrence data. Ecography 29: 129-151. \url{http://dx.doi.org/10.1111/j.2006.0906-7590.04596.x}

Hijmans R.J., and C.H. Graham, 2006. Testing the ability of climate envelope models to predict the effect of climate change on species distributions. Global change biology 12: 2272-2281. \url{http://dx.doi.org/10.1111/j.1365-2486.2006.01256.x}
}

\author{
Robert J. Hijmans
}


\keyword{spatial}

