

partial <- function(model, var=NULL, at=median) {
	
	d <- model@presence
	if (is.null(var)) {
		var <- colnames(d)
	}
	
	var <- var[var %in% colnames(d)]
	if (length(var) == 0) { stop('var not found')	}
	
	if (length(var) > 1) {
		on.exit(par(par(no.readonly = TRUE) ))
		x <- floor(sqrt(length(var)))
		y <- ceiling(length(var) / x)
		par(mfrow=c(x,y))
	}
	
	for (vr in var) {
		i <- which(colnames(d)==vr)
		v <- d[,i]
		if (is.factor(v)) {
			v <- levels(v)
		} else {
			v <- range(v)
			v <- v[1] + -10:110 * (v[2]-v[1])/100
		}
		if (is.function(at)) {
			m <- as.numeric(apply(d[,-i], 2, at))
			m <- data.frame(matrix(rep(m, each=length(v)), nrow=length(v)))
			colnames(m) <- colnames(d)[-i]
		} else {
			m <- as.vector(at)
			if (length(m) != ncol(d)-1) {
				stop('length of "at" is not correct')
			}
			m <- data.frame(matrix(rep(m, each=length(v)), nrow=length(v)))
			colnames(m) <- names(at)
		}

		a <- cbind(v, m)
		colnames(a)[1] <- vr
		p <- predict(model, a)
		plot(a[,1], p, type='l', xlab=vr, ylab='predicted value', col='red', lwd=2)
	}
	
	if (length(var) == 1) {
		return(invisible(cbind(a[,1], p)))
	}
}


