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
#' @param nested whether model files are nested in eponymous directories
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


#' Identify Datafile
#' 
#' Identifies datafile.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
datafile <- function(x,...)UseMethod('datafile')

#' Identify Datafile from Numeric
#' 
#' Identifies datafile from numeric.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
datafile.numeric <- function(x,...)datafile(as.character(x),...)

#' Identify the Datafile for a Model
#' 
#' Identifies the datafile used by a model.#' 
#' @param x the model name or path to a control stream
#' @param ... ext can be passed to modelfile, etc.
#' @return character
#' @export

datafile.character <- function(
  x,
  ...
){
  ctlfile <- if(file.exists(x)) x else modelfile(x,...)
  rundir  <- if(file.exists(x)) dirname(x) else modeldir(x,...)
  if(!file.exists(x)) x <- modelfile(x, ...)
  control <- read.model(ctlfile,...)
  dname <- getdname(control)
  datafile <- resolve(dname,rundir)
  datafile <- relativizePath(datafile)
  datafile
}

#' Identify Specfile
#' 
#' Identifies specfile.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
specfile <- function(x,...)UseMethod('specfile')

#' Identify Specfile from Numeric
#' 
#' Identifies specfile from numeric.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
specfile.numeric <- function(x,...)specfile(as.character(x),...)

#' Identify the Data Specification File for a Model
#' 
#' Identifies the data specification file associated with the datafile used by a model. Locates the datafile specified in the control stream, and substitutes a different extension.
#' 
#' @param x the model name
#' @param find file extension to replace
#' @param use file extension to use
#' @param ... pass ext over-ride default file extension in datafile()
#' @seealso datafile
#' @return character
#' @export

specfile.character <- function(
  x,
  find = '\\.csv$',
  use = '.spec',
  ...
){
  datafile <- datafile(x,...)
  if(!grepl(find,datafile))stop(find, ' not found in ',datafile)
  specfile <- sub(find,use,datafile)
  specfile
}




