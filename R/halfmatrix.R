is.square <- function(x,...)UseMethod('is.square')
is.square.matrix <- function(x,...)dim(x)[[1]]==dim(x)[[2]]
ord <- function(x,...)UseMethod('ord')
ord.matrix <- function(x,...){
	if(!is.square(x))stop('matrix is not square')
	dim(x)[[1]]
}
half <- function(x,...)UseMethod('half')
half.matrix <- function(x,...) {
    if(!isSymmetric(x))stop('matrix is not symmetric')
    d <- ord(x)
    dex <- data.frame(row=rep(1:d,each=d),col=rep(1:d,d))
    dex <- dex[dex$col <= dex$row,]
    x <- x[as.matrix(dex)]
    names(x) <- do.call(paste,c(dex,list(sep='.')))
    structure(x,class='halfmatrix')
}
ord.halfmatrix <- function(x,...){
	ord <- sqrt(0.25+2*length(x))-0.5
	if(as.integer(ord)!=ord)stop('invalid length for half matrix')
	ord
}
print.halfmatrix <- function(x,...)print(unclass(x))
as.halfmatrix <- function(x,...)UseMethod('as.halfmatrix')
as.halfmatrix.halfmatrix <- function(x,...)x
as.halfmatrix.default <- function(x,...)half(as.matrix.halfmatrix(x))
as.matrix.halfmatrix <- function(x,...){
  d <- ord.halfmatrix(x)
  y <- matrix(nrow=d,ncol=d)
  dex <- data.frame(row=rep(1:d,each=d),col=rep(1:d,d))
  dex <- dex[dex$col <= dex$row,]
  y[as.matrix(dex)] <- x
  y[is.na(y)] <- t(y)[is.na(y)]
  y  
}

as.data.frame.halfmatrix <- function(x,...){
  d <- ord.halfmatrix(x)
  y <- matrix(nrow=d,ncol=d)
  dex <- data.frame(row=rep(1:d,each=d),col=rep(1:d,d))
  dex <- dex[dex$col <= dex$row,]
  dex$x <- x
  dex
}

offdiag <- function(x,...)UseMethod('offdiag')
offdiag.halfmatrix <- function(x,...)x[sapply(strsplit(names(x),'\\.'),`[`,1)!=sapply(strsplit(names(x),'\\.'),`[`,2)]

