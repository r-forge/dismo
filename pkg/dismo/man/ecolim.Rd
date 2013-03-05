\name{ecolim}

\alias{ecolim}
\alias{ecolim,matrix,matrix-method}
\alias{EcoLim-class}
\alias{predict,EcoLim-method}


\title{Ecolim model}

\description{
Simple generic limiting factor based ("mechanistic-light") model, similar to PlantGro and EcoCrop. 
}

\usage{
ecolim(x, y, ...) 
}
 

\arguments{
  \item{x}{Matrix with driver variables}
  \item{y}{Matrix with responses (between 0 and 1)}
  \item{...}{Additional arguments. None implemented}
}



\author{
Robert J. Hijmans
}

\examples{
# get predictor variables
fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), 
              pattern='grd', full.names=TRUE ) 
env <- stack(fnames)

bio1 <- c(0,200,250,400)
bio12 <- c(0,1000,3000, 4000)
r1 <- c(0, 0, 0.5, 1)
r2 <- c(0, 0, 1, 1)
e <- ecolim(cbind(bio1, bio12), cbind(r1, r2)) 
#plot(e)
p <- predict(e, env)
#plot(p)

occurence <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
occ <- read.table(occurence, header=TRUE, sep=',')[,-1]
e1 <- evaluate(e, p=occtest, a=bg, x=env)


}


\keyword{spatial}


