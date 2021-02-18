#' Resolve A Path to a Model-related File
#' 
#' Resolves a path to a model-related file.
#' 
#' @param x object
#' @param ... passed arguments
#' @return character
#' @export
#' @family path
#' @examples 
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% modelpath
modelpath <- function(x,...)UseMethod('modelpath')

#' Resolve A Path to a Model-related File for Numeric
#' 
#' Resolves a path to a model-related file, coercing \code{x} to character.
#' 
#' @param x object
#' @param ... passed arguments
#' @return character
#' @export
#' @family path
#' @keywords internal
modelpath.numeric <- function(x,...)modelpath(as.character(x),...)

#' Resolve A Path to a Model-related File for Character
#' 
#' Resolves a path to a model-related file, treating \code{x} as a model name.
#' By default (\code{ext} is NULL) the run directory is returned.
#' As of version 0.9.2, \code{nested} can be a function of \code{ext} and \dots
#' That returns logical.
#' 
#' @param x object
#' @param ext file extension, no leading dot
#' @param project project directory
#' @param nested whether model files are nested in eponymous directories
#' @param ... passed arguments
#' @return character
#' @export
#' @family path
modelpath.character <- function(
  x,
  ext = NULL,
  project = getOption('project', getwd()),
  nested = getOption('nested', TRUE),
  ...
){
  if(!is.logical(nested))nested <- match.fun(nested)(ext, ...)
  stopifnot(is.logical(nested))
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
#' @family path
#' @examples 
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% modelfile('xml')
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
#' @family path
#' @examples 
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% modeldir
modeldir <- function(x, ext, ...)modelpath(x, ext = NULL, ...)

#' Identify Datafile
#' 
#' Identifies datafile.
#' @param x object
#' @param ... dots
#' @export
#' @family path
#' @keywords internal
datafile <- function(x,...)UseMethod('datafile')

#' Identify Datafile from Numeric
#' 
#' Identifies datafile from numeric.
#' @param x object
#' @param ... dots
#' @export
#' @family path
#' @keywords internal
datafile.numeric <- function(x,...)datafile(as.character(x),...)

#' Identify the Datafile for a Model
#' 
#' Identifies the datafile used by a model. 
#' Expresses it relative to current working directory.
#' @param x the model name or path to a control stream
#' @param ... ext can be passed to modelfile, etc.
#' @return character
#' @export
#' @family path
#' @examples
#' library(spec)
#' source <- system.file(package = 'nonmemica','project')
#' target <- tempdir()
#' target <- gsub('\\\\','/',target) # for windows
#' file.copy(source,target,recursive = TRUE)
#' project <- file.path(target,'project','model')
#' options(project = project)
#' library(magrittr)
#' 1001 %>% datafile
#' datafile(1001) %matches% specfile(1001)
#' 1001 %>% specfile
#' 1001 %>% specfile %>% read.spec
datafile.character <- function(
  x,
  ...
){
  ctlfile <- if(file_test('-f',x)) x else modelfile(x,...)
  rundir  <- if(file_test('-f',x)) dirname(x) else modeldir(x,...)
  if(!file_test('-f',x)) x <- modelfile(x, ...)
  control <- read.model(ctlfile,...)
  dname <- getdname(control)
  # datafile <- resolve(dname,rundir)
  datafile <- resolve(dname, dirname(ctlfile)) # 0.9.2
  datafile <- relativizePath(datafile)
  datafile
}

#' Identify Specfile
#' 
#' Identifies specfile.
#' @param x object
#' @param ... dots
#' @export
#' @family path
#' @keywords internal
specfile <- function(x,...)UseMethod('specfile')

#' Identify Specfile from Numeric
#' 
#' Identifies specfile from numeric.
#' @param x object
#' @param ... dots
#' @export
#' @family path
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
#' @family path
#' @examples
#' library(spec)
#' source <- system.file(package = 'nonmemica','project')
#' target <- tempdir()
#' target <- gsub('\\\\','/',target) # for windows
#' file.copy(source,target,recursive = TRUE)
#' project <- file.path(target,'project','model')
#' options(project = project)
#' library(magrittr)
#' 1001 %>% datafile
#' datafile(1001) %matches% specfile(1001)
#' 1001 %>% specfile
#' 1001 %>% specfile %>% read.spec
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

#' Relativize a Path
#' 
#' Relativizes a path. 
#' 
#' x and dir are first normalized, then x is expressed relative to dir.
#' If x and dir are on different drives (i.e. C:/ D:/)  x is returned
#' as an absolute path.
#' @param x a file path
#' @param dir a reference directory
#' @param sep path separator
#' @param ... ignored arguments
relativizePath <- function(x,dir=getwd(),sep='/',...){
  stopifnot(length(x)==1)
  stopifnot(file.info(dir)$isdir)
  y <- normalizePath(x,winslash='/')
  z <- normalizePath(dir,winslash='/')
  # test for different drives
  if(!identical(substr(y,1,1),substr(z,1,1))){
    return(y)
  }
  y <- strsplit(y,sep)[[1]]
  z <- strsplit(z,sep)[[1]]
  count <- 0
  while(length(y) && length(z) && y[[1]] == z[[1]]){
    y <- y[-1]
    z <- z[-1]
  }
  z <- rep('..',length(z))
  y <- c(z,y)
  y <- do.call(file.path,as.list(y))
  y
}

#' Check if File Path is Absolute
#' 
#' Checks if file path is absolute.
#' @param x character (a file path)
#' @return logical; TRUE if x starts with / or .: (e.g. C:)
absolute <- function(x)grepl('^/',x) | grepl('^.:',x)

#' Resolve File Path
#' 
#' Resolves a file path.  Returns the path if absolute.  If
#' relative, concatenates the directory and file.
#' @param file path to a file
#' @param dir reference directory for a relative file path
#' @return character
resolve <-
  function (file, dir) 
    ifelse(
      absolute(file),
      file, 
      file.path(dir, file)
    )

#' PsN Model File is Nested
#' Check whether a particular file extension corresponds to
#' a file that is nested within a subdirectory
#' using default PsN conventions.
#' @param x character, a file extension, without dot.
#' @param ... ignored
#' @export
#' @return logical
#' @examples
#' psn_nested('mod')
psn_nested <- function(x, ...){
  if(is.null(x))return(TRUE)
  if(x %in% c('mod', 'lst')) return(FALSE)
  return(TRUE)
}

#' Set PsN Options
#' Sets PsN-style directory and control stream options.
#' Supports control streams with semicolon-delimited metadata
#' including symbol, unit, transform, and label.
#' Expects model files to be found in nested directory,
#' except for *.mod and *.lst.
#' @param project character, path to project directory
#' @param modex character, extension for model control stream (no dot)
#' @param fields character
#' @param nested logical, or function of file extension returning logical
#' @param ... ignored
#' @export
#' @return used for side-effects (sets options 'fields' and 'nested')
#' @family path
#' @examples
#' \dontrun{
#' psn_options()
#' }
psn_options <- function(
  project = 'NONMEM',
  modex = 'mod',
  fields = c('symbol','unit','transform','label'),
  nested = psn_nested,
  ...
){
  options(modex = modex)
  options(project = project)
  options(fields = fields)
  options(nested = nested)
}


