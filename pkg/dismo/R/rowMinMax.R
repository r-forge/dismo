# minor changes from function by Sebastian Kranz <skr...@uni-bonn.de>
#http://www.mail-archive.com/r-devel@r-project.org/msg20044.html

.rowMin <- function(x, na.rm=TRUE) {
   # Construct a call pmin(x[,1],x[,2],...x[,NCOL(x)])
    #code <- paste("x[,",1:(NCOL(x)),"]",sep="",collapse=",")
    #code <- paste("pmin.int(",code,", na.rm =",na.rm,")")
    #return(eval(parse(text=code)))
	apply(x, 1, min, na.rm=na.rm)
}

.rowMax <- function(x, na.rm=TRUE) {
#    code <- paste("x[,",1:(NCOL(x)),"]",sep="",collapse=",")
#    code <- paste("pmax.int(",code,", na.rm =",na.rm,")")
#    return(eval(parse(text=code)))
	apply(x, 1, max, na.rm=na.rm)
	
}

.colMin <- function(x, na.rm=TRUE) {
#	.rowMin(t(x), na.rm=na.rm)
	apply(x, 2, min, na.rm=na.rm)

}

.colMax <- function(x, na.rm=TRUE) {
#	.rowMax(t(x), na.rm=na.rm)
	apply(x, 2, max, na.rm=na.rm)

}

