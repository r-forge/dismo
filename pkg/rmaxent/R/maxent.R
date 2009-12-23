

maxent <- function(samples, input, output=getwd()) {
	# create instance of class
	
	jar <- paste(system.file(package="rmaxent"), "/java/maxent.jar", sep='')
	if (!file.exists(jar)) {
		stop('file missing:', jar, '.\nPlease download it here: http://www.cs.princeton.edu/~schapire/maxent/')
	}
	
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
	input <- input[1]
	if (!file.exists(input)) {
		stop('input folder does not exist')
	}
	output <- output[1]
	if (!file.exists(output)) {
		stop('output folder does not exist')
	}
	
	mxe <- .jnew("rmaxent") 
	
	test <- try (  
		out <- .jcall(mxe, "S", "invoke", c("autorun", "-e", input, "-o", output, "-s", samples)) 
	)
	if (class(test) == 'try-error') {
		out <- 'BOO'
		warning('something went wrong')
	}
	
	if (file.exists(fn)) file.remove(fn)
	
	return(invisible(out))
}
