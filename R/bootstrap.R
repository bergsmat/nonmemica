#' Create a Bootstrap Table
#'
#' Creates a bootstrap table.
#' 
#' @param x object of dispatch
#' @param ... arguments to methods
#' @seealso \code{\link{as.bootstrap.modelname}}
#' @export
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
#' @export
as.bootstrap.bootstrap <- function(x,...)x

#' Create Bootstrap from Numeric
#' 
#' Creates bootstrap from numeric.
#' 
#' @inheritParams as.bootstrap
#' @return data.frame
#' @describeIn as.bootstrap numeric method
#' @export
as.bootstrap.numeric  <- function(x,...)as.bootstrap(as.character(x),...)

#' Create Bootstrap from Character
#' 
#' Creates bootstrap from character.
#' 
#' @inheritParams as.bootstrap
#' @return data.frame
#' @describeIn as.bootstrap character method
#' @export
as.bootstrap.character <- function(x,...){
  class(x) <- if(file.exists(x)) 'filepath' else 'modelname'
  as.bootstrap(x,...)
}

#' Create a Bootstrap Table from Filepath
#'
#' Creates a bootstrap table from a PsN bootstrap results csv filepath.
#' 
#' @import magrittr
#' @inheritParams as.bootstrap
#' @param skip number of lines to skip in bootstrap_results.csv
#' @param check.names passed to bootstrap reader
#' @param lo the PsN bootstrap lower confidence limit (\%)
#' @param hi the PsN bootstrap upper confidence limit (\%)
#' @param verbose display messages
#' @return data.frame
#' @describeIn as.bootstrap filepath method
#' @export

as.bootstrap.filepath <- function(x,skip=28,check.names=FALSE,lo='5',hi='95',verbose=TRUE,...){
  if(verbose) message('reading ',x)
  x <- x %>% 
    utils::read.csv(skip=skip,check.names=check.names,as.is=TRUE,...)
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

#' Create a Bootstrap Table from Modelname in Project Context
#'
#' Creates a bootstrap table from a modelname in Project Context.
#' 
#' Assumes project has been identified, model directory exists, and PsN bootstrap method has been run for the model.  Scavenges for the last file matching pattern.  
#' 
#' @inheritParams as.bootstrap
#' @param project path to model directories
#' @param opt alternative specification of project
#' @param rundir model specific run directory
#' @param pattern pattern to search for bootstrap file
#' @param bootcsv path to bootstrap_results.csv or equivalent
#' @return data.frame
#' @describeIn as.bootstrap modelname method
#' @export


as.bootstrap.modelname <- function(
  x,
  project = if(is.null(opt)) getwd() else opt, 
  opt = getOption('project'),
  rundir = file.path(project,x),
  pattern='bootstrap_results.csv',
  bootcsv = dir(
    rundir,
    pattern = pattern,
    recursive=TRUE,
    full.names=TRUE
  ),
  ...
){
  if(!length(bootcsv))stop(
    'no file found with pattern ',
    pattern,
    '. Perhaps set arg project= or options(project=)'
  )
  file <- rev(bootcsv)[[1]] # use last
  class(file) <- 'filepath'
  as.bootstrap(file,...)
}

