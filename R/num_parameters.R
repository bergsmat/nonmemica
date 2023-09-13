#' Get the Number of Parameters
#' 
#' Gets the Number of Parameters associated with an object.
#' Generic, with method \code{\link{num_parameters.default}}.
#' @param x object of dispatch
#' @param ... passed arguments
#' @export
#' @keywords internal
num_parameters <-
  function(x,...)UseMethod('num_parameters')

#' Get the Number of Declared Parameters for a NONMEM Model
#' 
#' Gets the number of declared parameters for a NONMEM model.
#' 
#' Parameters fixed to some value are included, but unlike (*.ext) matrix off-diagonals
#' are not included unless specified.
#' 
#' @param x object of dispatch
#' @param ... passed arguments
#' @return integer
#' @export
#' @keywords internal
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% num_parameters
num_parameters.default <-
  function(x,...){
    y <- as.model(x,parse=TRUE,...)
    y <- y[names(y) %in% c('theta','omega','sigma')]
    len <- sapply(y,length)
    sum(len)
  }
