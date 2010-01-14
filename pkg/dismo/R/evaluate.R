# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

setClass('ModelEvaluation',
	representation (
		presence = 'vector',
		absence = 'vector',
		np = 'integer',
		na = 'integer',
		auc = 'numeric',
		pauc = 'numeric',
		cor = 'numeric',
		pcor = 'numeric',
		confusion = 'matrix',
		prevalence = 'vector',
		overallDiagnosticPower = 'vector',
		correctClassificationRate = 'vector',
		sensitivity = 'vector',
		specificity = 'vector',
		falsePositiveRate ='vector',
		falseNegativeRate ='vector',
		PPP = 'vector',
		NPP = 'vector',
		misclassificationRate = 'vector',
		oddsRatio = 'vector',
		kappa = 'vector'
	),	
	prototype (	
		np = as.integer(0),
		na = as.integer(0)
	),
	validity = function(object)	{
		return(TRUE)
	}
)


evaluate <- function(model, p, a, x=NULL, tr) {
	if (!missing(x)) {
		p <- predict(model, xyValues(x, p))
		a <- predict(model, xyValues(x, a))
	} else if (is.matrix(p) & is.matrix(a)) {
			if (ncol(p) >= ncol(model@presence) & nrow(p) >= nrow(model@presence) ) {
			p <- predict(model, p)
			a <- predict(model, a)
		} else if (is.vector(p) & is.vector(a)) {
			# do nothing
		} else {
			stop('I do not undertand these data')
		}
	}

	if (missing(tr)) {
		tr = as.vector(quantile(c(p, a), 0:100/100))
	}
	
	np <- length(p)
	na <- length(a)
	if (na == 0 | np == 0) {
		stop('cannot evaluate a model without absence and presence data that are not NA')
	}

	N <- na + np

	xc <- new('ModelEvaluation')
	xc@presence = p
	xc@absence = a
		
	mv <- wilcox.test(p, a)
	xc@pauc <- mv$p.value
	xc@auc <- as.vector(mv$statistic) / (na * np)
	cr <- cor.test(a, p)
	xc@cor <- cr$estimate
	xc@pcor <- cr$p.value
	
	res <- matrix(ncol=5, nrow=length(tr))
	colnames(res) <- c('x', 'tp', 'fp', 'fn', 'tn')
	res[,1] <- tr
	for (i in 1:length(tr)) {
		res[i,2] <- length(p[p>=tr[i]]) / np  # a
		res[i,3] <- length(p[p<tr[i]]) / np   # b
		res[i,4] <- length(a[a>=tr[i]]) / na  # c
		res[i,5] <- length(a[a<tr[i]]) / na   # d
	}
	xc@confusion = res
	a = res[,2]
	b = res[,3]
	c = res[,4]
	d = res[,5]
# after Fielding and Bell	
	xc@np <- as.integer(np)
	xc@na <- as.integer(na)
	xc@prevalence = a + c / N
	xc@overallDiagnosticPower = b + d / N
	xc@correctClassificationRate = a + d / N
	xc@sensitivity = a / (a + c)
	xc@specificity = d / (b + d)
	xc@falsePositiveRate = b / (b + d)
	xc@falseNegativeRate = c/(a + c)
	xc@PPP = a/(a + b)
	xc@NPP = d/(c + d)
	xc@misclassificationRate = (b + c)/N
	xc@oddsRatio = (a*d)/(c*b)

	kappa <- function(x) {
		PrA <- x[,2] + x[,5]
		a <- sum(x[,2:3])/sum(x[,2:5])
		b <- (x[,2] + x[,4])/sum(x[,2:5])
		ra <- a * b
		rd <- (1-a) * (1-b)
		PrE <- ra + rd
		k <- (PrA-PrE)/(1-PrE)
		return(k)
	}
	xc@kappa = kappa(res)
	
	return(xc)
}



setMethod ('show' , 'ModelEvaluation', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('n presences :' , object@np, '\n')
		cat('n absences  :' , object@na, '\n')
		cat('AUC         :' , object@auc,'\n')
		cat('p(AUC)      :' , object@pauc,'\n')
		cat('cor         :' , object@cor,'\n')
		cat('p(cor)      :' , object@pcor,'\n')
#		cat('prevalence  :' , object@prevalence,'\n')
#		cat('overallDiagnosticPower :', object@overallDiagnosticPower,'\n')
#		cat('correctClassificationRate :', object@correctClassificationRate,'\n')
#		cat('sensitivity :', object@sensitivity,'\n')
#		cat('specificity :', object@specificity,'\n')
#		cat('falsePositiveRate :', object@falsePositiveRate,'\n')
#		cat('falseNegativeRate :', object@falseNegativeRate,'\n')
#		cat('PPP :', object@PPP,'\n')
#		cat('NPP :', object@NPP,'\n')
#		cat('misclassificationRate :', object@misclassificationRate,'\n')
#		cat('oddsRatio :', object@oddsRatio,'\n')
#		cat('kappa :', object@kappa,'\n')
	}
)	


if (!isGeneric("plot")) {
	setGeneric("plot", function(x,y,...)
		standardGeneric("plot"))
}	


setMethod("plot", signature(x='ModelEvaluation', y='ANY'), 
	function(x, y, xlab='1-specificity',ylab='sensitivty', col='red', ...) {
		plot(1-e@specificity, e@sensitivity, xlim=c(0,1), ylim=c(0,1), xlab=xlab, ylab=ylab, col=col, ...)
		lines(1-e@specificity, e@sensitivity, , col=col)
	}
)



setMethod('density', signature(x='ModelEvaluation'), 
	function(x, ...) {
		ab <- density(x@absence)
		pr <- density(x@presence)
		yl = c(min(ab$y, pr$y), max(ab$y, pr$y))
		xl = c(min(ab$x, pr$x), max(ab$x, pr$x))
		plot(ab, main='', ylab='', xlab='', xlim=xl, ylim=yl, lwd=2, lty=2, col='blue', ...)
		lines(pr, col='red', lwd=2)
	} 
)

