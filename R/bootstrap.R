#' Create a Bootstrap Table
#'
#' Creates a bootstrap table.
#' 
#' @param x object of dispatch
#' @param ... arguments to methods
#' @seealso \code{\link{as.bootstrap.character}}
#' @family as.bootstrap
#' @export
#' @keywords internal
#' @return data.frame
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% as.bootstrap
as.bootstrap <- function(x,...)UseMethod('as.bootstrap')

#' Create Bootstrap from Bootstrap
#' 
#' Creates bootstrap from bootstrap.
#' 
#' @inheritParams as.bootstrap
#' @return data.frame
#' @describeIn as.bootstrap bootstrap method
#' @family as.bootstrap
#' @export
as.bootstrap.bootstrap <- function(x,...)x

#' Create Bootstrap from Numeric
#' 
#' Creates bootstrap from numeric.
#' 
#' @inheritParams as.bootstrap
#' @return data.frame
#' @family as.bootstrap
#' @keywords internal
#' @export
as.bootstrap.numeric  <- function(x,...)as.bootstrap(as.character(x),...)


#' Create a Bootstrap Table from Character
#'
#' Creates a bootstrap table from a PsN bootstrap results csv filepath. If \code{x} is not an existing file it is treated as a modelname and the results file is sought.
#' 
#' @import magrittr
#' @inheritParams as.bootstrap
#' @param skip number of lines to skip in bootstrap_results.csv
#' @param check.names passed to bootstrap reader
#' @param lo the PsN bootstrap lower confidence limit (\%)
#' @param hi the PsN bootstrap upper confidence limit (\%)
#' @param verbose display messages
#' @param pattern pattern to search for bootstrap file
#' @param bootcsv path to bootstrap_results.csv or equivalent
#' @return data.frame
#' @describeIn as.bootstrap character method
#' @family as.bootstrap
#' @export

as.bootstrap.character <- function(
  x,
  skip=28,
  check.names=FALSE,
  lo='5',
  hi='95',
  verbose=TRUE,
  pattern='bootstrap_results.csv',
  bootcsv = dir(
    modeldir(x, ...),
    pattern = pattern,
    recursive=TRUE,
    full.names=TRUE
  ),  
  ...
){
  if(!file_test('-f',x)){ # x is modelname
    if(!length(bootcsv))stop('bootcsv: file(s) not found')
    x <- rev(bootcsv)[[1]] # use last
  }
  if(verbose) message('reading ',x)
  x <- utils::read.csv(x, skip=skip,check.names=check.names,as.is=TRUE,...)
  y <- x
  x <- x[c(1:8),] 
  row.names(x) <- text2decimal(x[,1])
  x <- x[,-1]
  x <- t(x)
  x <- data.frame(x,stringsAsFactors = F,check.names=F)
  suppressWarnings(x[] <- lapply(x,as.numeric))
  x <- x[,names(x) %in% c(lo,hi)]
  x  
}
