

maxent <- function(samples, input, output=getwd()) {
	# create instance of class
	
	fn <- paste(basename(tempfile()), '.csv', sep='')
	if (class(samples) == 'data.frame' | class(samples) == 'matrix') {
		samples <- samples[,1:3]
		colnames(samples) <- c('species', 'dd.long', 'dd.lat')
		fn <- paste(basename(tempfile()), '.csv', sep='')
		write.table(samples, file=fn, row.names=FALSE, sep=',')
		samples <- fn
	} 
	samples <- samples[1]
	if (class(samples) != 'character') {
		stop('invalid samples argument')
	}
	if (!file.exists(samples)) {
		stop('samples file does not exist')
	}
	if (!file.exists(input)) {
		stop('input folder does not exist')
	}
	if (!file.exists(output)) {
		stop('output folder does not exist')
	}
	
	
	mxe <- .jnew("rmaxent") 
	out <- .jcall(mxe, "S", "invoke", c("autorun", "-e", input, "-o", output, "-s", samples)) 
	
	if (file.exists(fn)) file.remove(fn)
	
	return(invisible(out))
}
