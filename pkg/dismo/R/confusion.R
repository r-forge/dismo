# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

setClass('ConfusionMatrix',
	contains = 'matrix',
	representation (
		np = 'integer',
		na = 'integer',
		auc = 'numeric',
		pauc = 'numeric',
		cor = 'numeric',
		pcor = 'numeric',
		kappa = 'vector',
		maxkth = 'vector'
	),	
	prototype (	
		np = as.integer(0),
		na = as.integer(0),
		auc = as.numeric(NA),
		pauc = as.numeric(NA),
		cor = as.numeric(NA),
		pcor = as.numeric(NA),
		kappa = c(NA),
		maxkth = c(NA)
	),
	validity = function(object)	{
		return(TRUE)
	}
)


confusion <- function(x, t) {
	x <- na.omit(x)
	if (length(x[,1]) == 0) {
		return(  new('ConfusionMatrix') )
	}
	p <- subset(x, x[,1] == 1)[,2]
	np <- length(p)
	n <- subset(x, x[,1] == 0)[,2]
	na <- length(n)
	res <- matrix(ncol=5, nrow=length(t))
	colnames(res) <- c('x', 'tp', 'fp', 'fn', 'tn')
	res[,1] <- t
	for (i in 1:length(t)) {
		res[i,2] <- length(p[p>=t[i]]) / np
		res[i,3] <- length(n[n>=t[i]]) / na
		res[i,4] <- length(p[p<t[i]]) / np
		res[i,5] <- length(n[n<t[i]]) / na
	}
	xc <- as(res, 'ConfusionMatrix')
	xc@np <- as.integer(np)
	xc@na <- as.integer(na)
	mv <- wilcox.test(p, n)
	xc@pauc <- mv$p.value
	xc@auc <- as.vector(mv$statistic) / (na * np)
	cr <- cor.test(x[,1], x[,2])
	xc@cor <- cr$estimate
	xc@pcor <- cr$p.value
	xc@kappa <- .kappa(xc)
	xc@maxkth <- which(xc@kappa == max(xc@kappa))
	return(xc)
}



setMethod ('show' , 'ConfusionMatrix', 
	function(object) {
		cat('class     :' , class(object), '\n')
		cat('presences :' , object@np, '\n')
		cat('absences  :' , object@na, '\n')
		cat('AUC       :' , object@auc,'\n')
		cat('p(AUC)    :' , object@pauc,'\n')
		cat('cor       :' , object@cor,'\n')
		cat('p(cor)    :' , object@pcor,'\n')
	}
)	


if (!isGeneric("plot")) {
	setGeneric("plot", function(x,y,...)
		standardGeneric("plot"))
}	


setMethod("plot", signature(x='ConfusionMatrix', y='ANY'), 
	function(x, y, xlab='FPR',ylab='TPR',...) {
		plot(x[,3], x[,2], ...)
		lines(x[,3], x[,2])
	}
)


.kappa <- function(x) {
	PrA <- x[,2] + x[,5]
	a <- sum(x[,2:3])/sum(x[,2:5])
	b <- (x[,2] + x[,4])/sum(x[,2:5])
	ra <- a * b
	rd <- (1-a) * (1-b)
	PrE <- ra + rd
	k <- (PrA-PrE)/(1-PrE)
	return(k)
}
