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
		t = 'vector',
		confusion = 'matrix',
		prevalence = 'vector',
		ODP = 'vector', # overall diagnostic power
		CCR = 'vector', # correct classification rate
		TPR = 'vector', # sensitivity, or true poistive rate
		TNR = 'vector', # specificity, or true negative rate
		FPR ='vector', # False positive rate
		FNR ='vector', # False negative rate
		PPP = 'vector',
		NPP = 'vector',
		MCR = 'vector', # misclassification rate
		OR = 'vector', # odds ratio
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


evaluateROCR <- function(model, p, a, x) {
	require(ROCR)
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
	p <- na.omit(p)
	a <- na.omit(a)
	if (length(p) < 1) { stop('no valid presence (p) values') }
	if (length(a) < 1) { stop('no valid absence (a) values') }
	predictions = c(p, a)
	labels = c( rep(1, length(p)), rep(0, length(a)) )
	pred <- prediction( predictions, labels)
	return(pred)
}


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
	cr <- cor.test(c(p,a), 	c(rep(1, length(p)), rep(0, length(a))) )
	xc@cor <- cr$estimate
	xc@pcor <- cr$p.value
	
	res <- matrix(ncol=4, nrow=length(tr))
	colnames(res) <- c('tp', 'fp', 'fn', 'tn')
	xc@t <- tr
	for (i in 1:length(tr)) {
		res[i,1] <- length(p[p>=tr[i]])  # a  true positives
		res[i,2] <- length(a[a>=tr[i]])  # b  false positives
		res[i,3] <- length(p[p<tr[i]])    # c  false negatives
		res[i,4] <- length(a[a<tr[i]])    # d  true negatives
	}
	xc@confusion = res
	a = res[,1]
	b = res[,2]
	c = res[,3]
	d = res[,4]
# after Fielding and Bell	
	xc@np <- as.integer(np)
	xc@na <- as.integer(na)
	xc@prevalence = a + c / N
	xc@ODP = b + d / N
	xc@CCR = a + d / N
	xc@TPR = a / (a + c)
	xc@TNR = d / (b + d)
	xc@FPR = b / (b + d)
	xc@FNR = c/(a + c)
	xc@PPP = a/(a + b)
	xc@NPP = d/(c + d)
	xc@MCR = (b + c)/N
	xc@OR = (a*d)/(c*b)

	kappa <- function(x) {
		PrA <- x[,1] + x[,4]
		a <- sum(x[,1:2])/sum(x[,1:4])
		b <- (x[,1] + x[,3])/sum(x[,1:4])
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


setMethod("plot", signature(x='ModelEvaluation', y='character'), 
	function(x, y='ROC', col='red', ...) {
		if (y == 'ROC') {
			txt = paste('AUC=', round(e@auc,3))
			plot(x@FPR, x@TPR, xlim=c(0,1), ylim=c(0,1), xlab='False postive rate', ylab='True positive rate', col=col, main=txt, ...)
			lines(x@FPR, x@TPR, col=col)
			lines(rbind(c(0,0), c(1,1)), lwd=2, col='grey')
		} else if (y == 'kappa') {
			txt = paste('max kappa at:', round(x@t[which.max(e@kappa)], 2))
			plot(x@t, x@kappa, xlab='threshold', ylab='kappa', col=col, main=txt, ...)
			lines(x@t, x@kappa, col=col)
		} else {
			stop('unknown value for "y"')
		}
	}
)


setMethod('density', signature(x='ModelEvaluation'), 
	function(x, ...) {
		pr <- density(x@presence)
		ab <- density(x@absence, bw=pr$bw )
		yl = c(min(ab$y, pr$y), max(ab$y, pr$y))
		xl = c(min(x@t), max(x@t))
		plot(ab, main='', ylab=paste('density. Bandwidth=',round(pr$bw,5),paste=''), xlab='predicted value', xlim=xl, ylim=yl, lwd=2, lty=2, col='blue', ...)
		lines(pr, col='red', lwd=2)
#		x1 <- xl[1]+(xl[2]-xl[1]) / 3
#		x2 <- xl[1]+ 2 * (xl[2]-xl[1]) / 3
#		y = yl[1] + 0.5 * (yl[2] - yl[1])
#		text(x1,y,'absence',col='blue')
#		text(x2,y,'presence',col='red')
	} 
)

