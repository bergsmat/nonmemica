#' Test If Something is Square
#' 
#' Tests if something is square.  Generic, with method for matrix.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
is.square <- function(x,...)UseMethod('is.square')

#' Test If Matrix is Square
#' 
#' Tests if matrix is square. 
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
is.square.matrix <- function(x,...)dim(x)[[1]]==dim(x)[[2]]

#' Calculate Order
#' 
#' Calculates order.  Generic, with method for matrix.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
ord <- function(x,...)UseMethod('ord')

#' Calculate Order for Matrix
#' 
#' Calculates order for matrix. I.e., length of one dimension of a square matrix.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
ord.matrix <- function(x,...){
	if(!is.square(x))stop('matrix is not square')
	dim(x)[[1]]
}

#' Find Half of Something
#' 
#' Finds half of something.  Generic, with method for matrix.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
half <- function(x,...)UseMethod('half')


#' Find Half of Matrix
#' 
#' Finds half of matrix.  I.e., lower or upper triangle of symmetric matrix.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
half.matrix <- function(x,...) {
    if(!isSymmetric(x))stop('matrix is not symmetric')
    d <- ord(x)
    dex <- data.frame(row=rep(1:d,each=d),col=rep(1:d,d))
    dex <- dex[dex$col <= dex$row,]
    x <- x[as.matrix(dex)]
    names(x) <- do.call(paste,c(dex,list(sep='.')))
    structure(x,class='halfmatrix')
}

#' Calculate Order for Half Matrix
#' 
#' Calculates order for half matrix. I.e., length of one dimension of the equivalent square matrix.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
ord.halfmatrix <- function(x,...){
	ord <- sqrt(0.25+2*length(x))-0.5
	if(as.integer(ord)!=ord)stop('invalid length for half matrix')
	ord
}

#' Print Half Matrix
#' 
#' Prints half matrix.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
print.halfmatrix <- function(x,...)print(unclass(x))


#' Coerce to  Half Matrix
#' 
#' Coerces to half matrix. Generic, with default methods.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
as.halfmatrix <- function(x,...)UseMethod('as.halfmatrix')

#' Coerce Half Matrix to Half Matrix
#' 
#' Coerces half matrix to half matrix. A non-operation.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
as.halfmatrix.halfmatrix <- function(x,...)x


#' Coerce to Half Matrix by Default
#' 
#' Coerces to half matrix.  Treats x as halfmatrix, coerces to matrix and takes half.
#' @export
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
as.halfmatrix.default <- function(x,...)half(as.matrix.halfmatrix(x))


#' Coerce Half Matrix to Matrix
#' 
#' Coerces half matrix to matrix.
#' @export
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
as.matrix.halfmatrix <- function(x,...){
  d <- ord.halfmatrix(x)
  y <- matrix(nrow=d,ncol=d)
  dex <- data.frame(row=rep(1:d,each=d),col=rep(1:d,d))
  dex <- dex[dex$col <= dex$row,]
  y[as.matrix(dex)] <- x
  y[is.na(y)] <- t(y)[is.na(y)]
  y  
}

#' Coerce Half Matrix to Data Frame
#' 
#' Coerces half matrix to data.frame.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
as.data.frame.halfmatrix <- function(x,...){
  d <- ord.halfmatrix(x)
  y <- matrix(nrow=d,ncol=d)
  dex <- data.frame(row=rep(1:d,each=d),col=rep(1:d,d))
  dex <- dex[dex$col <= dex$row,]
  dex$x <- x
  dex
}

#' Isolate Off-diagonal
#' 
#' Isolates off-diagonal.  Generic, with method for halfmatrix.
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
offdiag <- function(x,...)UseMethod('offdiag')

#' Isolate Off-diagonal of Half Matrix
#' 
#' Isolates off-diagonal of halfmatrix.
#' @export
#' @param x object
#' @param ... passed arguments
#' @family halfmatrix
offdiag.halfmatrix <- function(x,...)x[sapply(strsplit(names(x),'\\.'),`[`,1)!=sapply(strsplit(names(x),'\\.'),`[`,2)]

