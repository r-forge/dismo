\name{ecolim}

\alias{ecolim}
\alias{ecolim,matrix,matrix-method}
\alias{EcoLim-class}
\alias{predict,EcoLim-method}


\title{Ecolim model}

\description{
Simple generic limiting factor based model, in the tradition of the PLANTGRO model (Hackett, 1991)
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
fold <- kfold(occ, k=5)
occtest <- occ[fold == 1, ]
occtrain <- occ[fold != 1, ]
bg <- randomPoints(env, 1000)


\dontrun{
# An approach to optimize the values based on
# some known presences and (here random) absences
# for the same species as in the maxent example

# intial parameters
v <- c(0, 200, 250, 400, 0, 1000, 3000, 4000)

# function to be minimized
f <- function(p) {
	bio1 <- p[1:4]
	bio12 <- p[5:8]
	# numbers must go up
	if(any((bio1[2:4] - bio1[1:3]) <= 0)) return(999)
	if(any((bio12[2:4] - bio12[1:3]) <= 0)) return(999)
	r1 <- c(0, 0, 0.5, 1)
	r2 <- c(0, 0, 1, 1)
	e <- ecolim(cbind(bio1, bio12), cbind(r1, r2)) 
	# we are minimizing, hence 1-AUC
	1-evaluate(e, p=occtrain, a=bg, x=env)@auc
}

# patience...
z <- optim(v, f)

bio1 <- z$par[1:4]
bio12 <- z$par[5:8]
eco <- ecolim(cbind(bio1, bio12), cbind(r1, r2)) 
evaluate(eco, p=occtest, a=bg, x=env)

pwd <- pwdSample(occtest,bg,occtrain)
ptest <- occtest[!is.na(pwd),]
atest <- bg[na.omit(pwd),]
evaluate(eco, p=ptest, a=atest, x=env)

p2 <- predict(eco, env)
plot(p2)
}
}

\keyword{spatial}


