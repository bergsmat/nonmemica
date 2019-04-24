#' Get the Number of Parameters
#' 
#' Gets the Number of Parameters associated with an object.
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

#' Generate Canonical Names
#' 
#' Generates canonical names.
#' @param x object of dispatch
#' @param ... passed arguments
#' @keywords internal
#' @export
nms_canonical <- function(x,...)UseMethod('nms_canonical')

#' Generate Canonical Names for Numeric
#' 
#' Generates canonical names for numeric by coercing to character.
#' @inheritParams nms_canonical
#' @export
#' @keywords internal
nms_canonical.numeric <- function(x,...)nms_canonical(as.character(x,...),...)

#' Generate Canonical Names for Character
#' 
#' Generates canonical names for character by converting to parsed model.
#' @inheritParams nms_canonical
#' @export
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% nms_canonical
nms_canonical.character <- function(x,...)nms_canonical(as.model(x,parse=TRUE,verbose=FALSE),...)

#' Generate Canonical Names for Model
#' 
#' Generates canonical names for a NONMEM control stream object. Canonical names indicate all and only the declared model parameters in lower-case conventional order (theta, omega row-major, sigma) with underscores and two-digit (or more) indices. E.g. theta_01, theta_02, omega_01_01, omega_02_01, omega_02_02, omega_01_01.
#' 
#' @param x a model designator
#' @param ... passed arguments
#' @return canonical (character)
#' @seealso as.model
#' @export

nms_canonical.model <- function(x,...){
  #y <- as.model(x,parse=TRUE,...)
  comments <- comments(x,tables=FALSE,...)
  res <- comments$item
  class(res) <- union('nms_canonical',class(res))
  res
}

#' Generate PsN-style Names
#' 
#' Generates PsN-style names.
#' @param x object of dispatch
#' @param ... passed arguments
#' @keywords internal
#' @export
nms_psn <- function(x,...)UseMethod('nms_psn')

#' Generate PsN-style Names for Numeric
#' 
#' Generates PsN-style names for numeric by coercing to character.
#' @inheritParams nms_psn
#' @export
#' @keywords internal
nms_psn.numeric <- function(x,...)nms_psn(as.character(x,...),...)

#' Generate PsN-style Names for Character
#' 
#' Generates PsN-style names for numeric by converting to parsed model.
#' @inheritParams nms_psn
#' @export
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% nms_psn
nms_psn.character <- function(x,...)nms_psn(as.model(x,parse=TRUE,verbose=FALSE),...)

#' Generate PsN-style Names for Model
#' 
#' Generates PsN-style names for parameters declared in a NONMEM control stream object. PsN uses NONMEM-style names, substituting a comment, if any: everything after the first semicolon, up to the second semicolon if present, without leading/trailing spaces/tabs.
#' 
#' @param x a model designator
#' @param ... passed arguments
#' @return psn (character)
#' @seealso as.model
#' @export
nms_psn.model <- function(x,...){
  comments <- comments(x,tables=FALSE,...)
  comments <- mutate(comments, item = .canonical2nonmem(item) )
  comments <- mutate(comments, item = if_else(is.na(symbol), item, symbol)) # substitute
  res <- comments$item
  class(res) <- union('nms_psn',class(res))
  res
}

.canonical2nonmem <- function(x,...){
  x <- toupper(x)               # uppercase
  x <- gsub( '_0+', '_',  x ) # unpadded indices
  x <- sub(  '_',   '(',  x ) # first underscore (all)
  x <- sub(  '_',   ',',  x ) # second underscore (ranef)
  x <- sub(  '$',   ')',  x ) # close the interval
  t <- grepl('^THETA',x)
  x[t] <- gsub('\\(|)','',x[t]) # drop parens
  x
}
.nonmem2canonical <- function(x,...){
  t <- sub('(^[a-z]+).*', '\\1', tolower(x))    # lowercase term, isolated
  i <- text2decimal(x)                      # first index
  r <- grepl(',',x)                       # random effects are double-indexed
  j <- text2decimal(sub('[^,]+','',x)) # second index
  j[!r] <- NA                                   # no second index if not ranef
  i <- padded(j,2)
  j <- padded(j,2)
  d <- ifelse(r, paste(i,j,sep='_'), i)         # one index or two
  o <- paste(t,d,sep='_')
  o
}

#' Generate NONMEM-style Names
#' 
#' Generates NONMEM-style names.
#' @param x object of dispatch
#' @param ... passed arguments
#' @keywords internal
#' @family nms_nonmem
#' @export
nms_nonmem <- function(x,...)UseMethod('nms_nonmem')

#' Generate NONMEM-style Names for Numeric
#' 
#' Generates NONMEM-style names for numeric by coercing to character.
#' @inheritParams nms_nonmem
#' @export
#' @family nms_nonmem
#' @keywords internal
nms_nonmem.numeric <- function(x,...)nms_nonmem(as.character(x,...),...)

#' Generate NONMEM-style Names for Character
#' 
#' Generates NONMEM-style names for numeric by converting to parsed model.
#' @inheritParams nms_nonmem
#' @export
#' @family nms_nonmem
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% nms_nonmem
nms_nonmem.character <- function(x,...)nms_nonmem(as.model(x,parse=TRUE,verbose=FALSE),...)

#' Generate NONMEM-style Names for Model
#' 
#' Generates NONMEM-style names for parameters declared in a NONMEM control stream object. PsN uses NONMEM-style names, substituting a comment, if any: everything after the first semicolon, up to the second semicolon if present, without leading/trailing spaces/tabs.
#' 
#' @param x a model designator
#' @param ... passed arguments
#' @return nonmem (character)
#' @family nms_nonmem
#' @seealso as.model
#' @export
nms_nonmem.model <- function(x,...){
  comments <- comments(x,tables=FALSE,...)
  comments <- mutate(comments,item = .canonical2nonmem(item))
  res <- comments$item
  class(res) <- union('nms_nonmem',class(res))
  res
}
