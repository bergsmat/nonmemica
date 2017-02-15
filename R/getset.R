#' Check If nmctl is Fixed
#' 
#' Checks if nmctl is fixed.  Returns a logical vector with element for each init, in canonical order.
#' @inheritParams fixed
#' @return logical
# @describeIn getset nmctl method
#' @export
fixed.nmctl <- function(x,...){
  i <- initDex(x)
  j <- initSubscripts(x)
  nms <- as.canonical(x)
  stopifnot(length(i) == length(j),length(i) == length(nms))
  f <- logical(0)
  for(e in seq_along(nms))f <- append(f,fixed(x[[i[[e]]]][[j[[e]]]]))
  f
}

#' Set fixed attribute of nmctl
#' 
#' Sets fixed attribute of nmctl.
#' @param x nmctl
#' @param value logical
#' @return nmctl
# @describeIn getset nmctl method for setting fixed
#' @export
#' @keywords internal

`fixed<-.nmctl` <- function(x,value){
  stopifnot(is.logical(value))
  i <- initDex(x)
  j <- initSubscripts(x)
  nms <- as.canonical(x)
  stopifnot(length(i) == length(j),length(i) == length(nms))
  stopifnot(length(value) %in% c(1,length(nms)))
  value <- rep(value,length.out=length(nms))
  for(e in seq_along(value))fixed(x[[i[[e]]]][[j[[e]]]]) <- value[[e]]
  x
}

.getInitDetail <- function(x,...)UseMethod('.getInitDetail')
.getInitDetail.nmctl <- function(x, y, ...){
  i <- initDex(x)
  j <- initSubscripts(x)
  nms <- as.canonical(x)
  stopifnot(length(i) == length(j),length(i) == length(nms))
  d <- numeric(0)
  for(e in seq_along(nms))d <- append(d,x[[i[[e]]]][[j[[e]]]][[y]])
  d
}

.setInitDetail <- function(x,...)UseMethod('.setInitDetail')
.setInitDetail.nmctl <- function(x, value, y, ...){
  i <- initDex(x)
  j <- initSubscripts(x)
  nms <- as.canonical(x)
  stopifnot(length(i) == length(j),length(i) == length(nms), length(i) == length(value))
  for(e in seq_along(value)) x[[i[[e]]]][[j[[e]]]][[y]] <- value[[e]]
  x
}

### GET/SET LOWER ###################################33
#' Get Lower Value
#' 
#' Gets lower Value.
#' @param x object of dispatch
#' @param ... dots
#' @export
lower <- function(x,...)UseMethod('lower')

#' Get Lower Bounds for Model Initial Estimates
#' 
#' Gets lower bounds for model initial estimates.
#' @param x nmctl
#' @param ... dots
# @describeIn getset nmctl method for getting lower bounds
#' @export
lower.nmctl <- function(x,...).getInitDetail(x,y='low',...)

#' Set Lower Value
#' 
#' Sets lower Value.
#' @param x object of dispatch
#' @param value right hand side
#' @export
`lower<-` <- function(x, value)UseMethod('lower<-')

#' Set Lower Bounds for Model Initial Estimates
#' 
#' Sets lower bounds for model initial estimates.
#' @param x nmctl
#' @param value numeric
# @describeIn getset nmctl method for setting lower bounds
#' @export
`lower<-.nmctl` <- function(x, value).setInitDetail(x, value = value, y = 'low')

### GET/SET UPPER ###################################33
#' Get Upper Value
#' 
#' Gets upper Value.
#' @param x object of dispatch
#' @param ... dots
# @describeIn getset get upper generic
#' @export
upper <- function(x,...)UseMethod('upper')

#' Get Upper Bounds for Model Initial Estimates
#' 
#' Gets upper bounds for model initial estimates.
#' @param x nmctl
#' @param ... dots
# @describeIn getset nmctl method for getting upper bounds
#' @export
upper.nmctl <- function(x,...).getInitDetail(x,y='up',...)

#' Set Upper Value
#' 
#' Sets upper Value.
#' @param x object of dispatch
#' @param value right hand side
#' @export
`upper<-` <- function(x, value)UseMethod('upper<-')

#' Set Upper Bounds for Model Initial Estimates
#' 
#' Sets upper bounds for model initial estimates.
#' @param x nmctl
#' @param value numeric
# @describeIn getset nmctl method for setting upper bounds
#' @export
`upper<-.nmctl` <- function(x, value).setInitDetail(x,value = value, y = 'up')

### GET/SET INITIAL ESTIMATE ###################################33
#' Get Initial Value
#' 
#' Gets initial Value.
#' @param x object of dispatch
#' @param ... dots
#' @export
initial <- function(x,...)UseMethod('initial')

#' Get Model Initial Estimates
#' 
#' Gets model initial estimates.
#' @param x nmctl
#' @param ... dots
# @describeIn getset nmctl method for getting initial estimates
#' @export
initial.nmctl <- function(x,...).getInitDetail(x,y = 'init',...)

#' Set Initial Value
#' 
#' Sets initial Value.
#' @param x object of dispatch
#' @param value right hand side
#' @export
`initial<-` <- function(x, value)UseMethod('initial<-')

#' Set Upper Bounds for Model Initial Estimates
#' 
#' Sets upper bounds for model initial estimates.
#' @param x nmctl
#' @param value numeric
# @describeIn initials nmctl method for setting upper bounds
#' @export
`initial<-.nmctl` <- function(x, value).setInitDetail(x, value = value, y = 'init')
