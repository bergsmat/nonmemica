#' Resolve A Path to a Model-related File
#' 
#' Resolves a path to a model-related file.
#' 
#' @param x object
#' @param ... passed arguments
#' @return character
#' @export
modelpath <- function(x,...)UseMethod('modelpath')

#' Resolve A Path to a Model-related File for Numeric
#' 
#' Resolves a path to a model-related file, coercing \code{x} to character.
#' 
#' @param x object
#' @param ... passed arguments
#' @return character
#' @export
modelpath.numeric <- function(x,...)modelpath(as.character(x),...)

#' Resolve A Path to a Model-related File for Character
#' 
#' Resolves a path to a model-related file, treating \code{x} as a model name. By default (\code{ext} is NULL) the run directory is returned.
#' 
#' @param x object
#' @param ext file extension, no leading dot
#' @param project project directory
#' @param nested whether 
#' @param ... passed arguments
#' @return character
#' @export
modelpath.character <- function(
  x,
  ext = NULL,
  project = getOption('project', getwd()),
  nested = getOption('nested', TRUE),
  ...
){
  rundir <- if(nested) file.path(project, x) else project
  if(is.null(ext)) return(rundir)
  filename <- paste(sep='.', x, ext)
  filepath <- file.path(rundir, filename)
  filepath
}


#' Identify the Modelfile for a Model
#' 
#' Identifies the modelfile used by a model.
#' 
#' @param x the model name
#' @param ext model file extension
#' @param ... passed arguments
#' @return character
#' @export

modelfile <- function(x, ext = getOption('modex','ctl'), ...)modelpath(x, ext = ext, ...)

#' Identify the Directory for a Model
#' 
#' Identifies the directory used by a model.
#' 
#' @param x the model name
#' @param ext model file extension
#' @param ... passed arguments
#' @return character
#' @export
modeldir <- function(x, ext, ...)modelpath(x, ext = NULL, ...)

#' Identify the Datafile for a Model
#' 
#' Identifies the datafile used by a model.
#' 
#' @param x the model name or path to a control stream
#' @param ... ext can be passed to modelfile, etc.
#' @return character
#' @export

datafile <- function(
  x,
  ...
){
  if(!file.exists(x)) x <- modelfile(x, ...)
  control <- read.model(x,...)
  dname <- getdname(control)
  datafile <- resolve(dname,modeldir(x, ...))
  datafile <- relativizePath(datafile)
  datafile
}
#' Identify the Data Specification File for a Model
#' 
#' Identifies the data specification file associated with the datafile used by a model. Identifies the datafile specified in the control stream, and substitutes a different extension.
#' 
#' @param x the model name
#' @param find file extension to replace
#' @param use file extension to use
#' @param ... pass ext over-ride default file extension in datafile()
#' @seealso datafile
#' @return character
#' @export

specfile <- function(
  x,
  find = '\\.csv$',
  use = '.spec',
  ...
){
  datafile <- datafile(x,...)
  specfile <- sub(find,use,datafile)
  specfile
}




