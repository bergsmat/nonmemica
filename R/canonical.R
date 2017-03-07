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
#' @export
as.canonical <- function(x,...)UseMethod('as.canonical')

#' Generate Canonical Names for Numeric
#' 
#' Generates canonical names for numeric by coercing to character.
#' @inheritParams as.canonical
#' @export
as.canonical.numeric <- function(x,...)as.canonical(as.character(x,...),...)

#' Generate Canonical Names for Character
#' 
#' Generates canonical names for numeric by converting to parsed model.
#' @inheritParams as.canonical
#' @export
as.canonical.character <- function(x,...)as.canonical(as.model(x,parse=TRUE,verbose=FALSE),...)


#' Generate Canonical Names for model
#' 
#' Generates canonical names for a NONMEM control stream object. Canonical names indicate all and only the declared model parameters in lower-cae conventional order (theta, omega row-major, sigma) with underscores and two-digit (or more) indices. E.g. theta_01, theta_02, omega_01_01, omega_02_01, omega_02_02, omega_01_01.
#' 
#' @param x a model designator
#' @param ... passed arguments
#' @return canonical (character)
#' @seealso as.model
#' @export

as.canonical.model <- function(x,...){
  #y <- as.model(x,parse=TRUE,...)
  comments <- as.itemComments(x,tables=FALSE,...)
  res <- comments$item
  class(res) <- union('canonical',class(res))
  res
}

#' Generate PsN-style Names
#' 
#' Generates PsN-style names.
#' @param x object of dispatch
#' @param ... passed arguments
#' @export
as.psn <- function(x,...)UseMethod('as.psn')

#' Generate PsN-style Names for Numeric
#' 
#' Generates PsN-style names for numeric by coercing to character.
#' @inheritParams as.psn
#' @export
as.psn.numeric <- function(x,...)as.psn(as.character(x,...),...)

#' Generate PsN-style Names for Character
#' 
#' Generates PsN-style names for numeric by converting to parsed model.
#' @inheritParams as.psn
#' @export
as.psn.character <- function(x,...)as.psn(as.model(x,parse=TRUE,verbose=FALSE),...)

#' Generate PsN-style Names for model
#' 
#' Generates PsN-style names for parameters declared in a NONMEM control stream object. PsN uses NONMEM-style names, substituting a comment, if any: everything after the first semicolon, up to the second semicolon if present, without leading/trailing spaces/tabs.
#' 
#' @param x a model designator
#' @param ... passed arguments
#' @return psn (character)
#' @seealso as.model
#' @export
as.psn.model <- function(x,...){
  comments <- as.itemComments(x,tables=FALSE,...)
  comments %<>% mutate(item = item %>% .canonical2nonmem )
  comments %<>% mutate(item = if_else(symbol %>% is.na, item, symbol)) # substitute
  res <- comments$item
  class(res) <- union('psn',class(res))
  res
}

.canonical2nonmem <- function(x,...){
  x %<>% toupper               # uppercase
  x %<>% gsub( '_0+', '_',  . ) # unpadded indices
  x %<>%  sub( '_',   '(',  . ) # first underscore (all)
  x %<>%  sub( '_',   ',',  . ) # second underscore (ranef)
  x %<>%  sub( '$',   ')',  . ) # close the interval
  t <- grepl('^THETA',x)
  x[t] <- gsub('\\(|)','',x[t]) # drop parens
  x
}
.nonmem2canonical <- function(x,...){
  t <- x %>% tolower %>% sub('(^[a-z]+).*', '\\1', .)    # lowercase term, isolated
  i <- x %>% text2decimal                       # first index
  r <- x %>% grepl(',',.)                       # random effects are double-indexed
  j <- x %>% sub('[^,]+','',.) %>% text2decimal # second index
  j[!r] <- NA                                   # no second index if not ranef
  i %<>% padded(2)
  j %<>% padded(2)
  d <- ifelse(r, paste(i,j,sep='_'), i)         # one index or two
  o <- paste(t,d,sep='_')
  o
}

#' Generate NONMEM-style Names
#' 
#' Generates NONMEM-style names.
#' @param x object of dispatch
#' @param ... passed arguments
#' @export
as.nonmem <- function(x,...)UseMethod('as.nonmem')

#' Generate NONMEM-style Names for Numeric
#' 
#' Generates NONMEM-style names for numeric by coercing to character.
#' @inheritParams as.nonmem
#' @export
as.nonmem.numeric <- function(x,...)as.nonmem(as.character(x,...),...)

#' Generate NONMEM-style Names for Character
#' 
#' Generates NONMEM-style names for numeric by converting to parsed model.
#' @inheritParams as.nonmem
#' @export
as.nonmem.character <- function(x,...)as.nonmem(as.model(x,parse=TRUE,verbose=FALSE),...)

#' Generate NONMEM-style Names for model
#' 
#' Generates NONMEM-style names for parameters declared in a NONMEM control stream object. PsN uses NONMEM-style names, substituting a comment, if any: everything after the first semicolon, up to the second semicolon if present, without leading/trailing spaces/tabs.
#' 
#' @param x a model designator
#' @param ... passed arguments
#' @return nonmem (character)
#' @seealso as.model
#' @export
as.nonmem.model <- function(x,...){
  comments <- as.itemComments(x,tables=FALSE,...)
  comments %<>% mutate(item = item %>% .canonical2nonmem )
  res <- comments$item
  class(res) <- union('nonmem',class(res))
  res
}
