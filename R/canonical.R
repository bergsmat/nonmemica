globalVariables(c('com', 'long', 'name', 'num'))
#' Generate Canonical Names
#' 
#' Generates canonical names.
#' Generic, with method \code{\link{nms_canonical.model}}.
#' @param x object of dispatch
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family nms
nms_canonical <- function(x,...)UseMethod('nms_canonical')

#' Generate Canonical Names for Numeric
#' 
#' Generates canonical names for numeric by coercing to character.
#' @inheritParams nms_canonical
#' @export
#' @family nms
#' @keywords internal
nms_canonical.numeric <- function(x,...)nms_canonical(as.character(x,...),...)

#' Generate Canonical Names for Character
#' 
#' Generates canonical names for character by converting to parsed model.
#' @inheritParams nms_canonical
#' @export
#' @family nms
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% nms_canonical
nms_canonical.character <- function(x,...)nms_canonical(as.model(x,parse=TRUE,verbose=FALSE, ...), ...)

#' Generate Canonical Names for Model
#' 
#' Generates canonical names for a NONMEM control stream object. Canonical names indicate all and only the declared model parameters in lower-case conventional order (theta, omega row-major, sigma) with underscores and two-digit (or more) indices. E.g. theta_01, theta_02, omega_01_01, omega_02_01, omega_02_02, omega_01_01.
#' 
#' @param x a model designator
#' @param ... passed arguments
#' @return nms_canonical (character)
#' @seealso as.model
#' @export
#' @family nms

nms_canonical.model <- function(x,...){
  #y <- as.model(x,parse=TRUE,...)
  comments <- comments(x,tables=FALSE,...)
  res <- comments$item
  class(res) <- 'nms_canonical'
  res
}

#' Generate PsN-style Names
#' 
#' Generates PsN-style names.
#' Generic, with method \code{\link{nms_psn.model}}.
#' @param x object of dispatch
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family nms
nms_psn <- function(x,...)UseMethod('nms_psn')

#' Generate PsN-style Names for Numeric
#' 
#' Generates PsN-style names for numeric by coercing to character.
#' @inheritParams nms_psn
#' @export
#' @family nms
#' @keywords internal
nms_psn.numeric <- function(x,...)nms_psn(as.character(x,...),...)

#' Generate PsN-style Names for Character
#' 
#' Generates PsN-style names for numeric by converting to parsed model.
#' @inheritParams nms_psn
#' @export
#' @family nms
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% nms_psn
nms_psn.character <- function(x,...)nms_psn(as.model(x,parse=TRUE,verbose=FALSE, ...), ...)

#' Generate PsN-style Names for Model
#' 
#' Generates PsN-style names for parameters declared in a NONMEM control stream object. PsN uses NONMEM-style names, substituting a comment, if any: everything after the first semicolon, up to the second semicolon if present, without leading/trailing spaces/tabs.
#' 
#' @param x a model designator
#' @param ... passed arguments
#' @return nms_psn (character)
#' @seealso as.model
#' @export
#' @family nms
nms_psn.model <- function(x,...){
  comments <- comments(x,tables=FALSE,...)
  comments <- mutate(comments, item = nms_nonmem.nms_canonical(item) )
  comments <- mutate(comments, item = if_else(is.na(symbol), as.character(item), symbol)) # substitute
  res <- comments$item
  class(res) <- 'nms_psn'
  res
}


#' Convert Canonical Parameter Names to NONMEM
#' 
#' Converts nms_canonical to nms_nonmem.
#' 
#' @export
#' @family nms
#' @param x nms_canonical
#' @param ... ignored
#' @return nms_nonmem
#' @examples
#' # example code
#' 

nms_nonmem.nms_canonical <- function(x,...){
  x <- toupper(x)               # uppercase
  x <- gsub( '_0+', '_',  x ) # unpadded indices
  x <- sub(  '_',   '(',  x ) # first underscore (all)
  x <- sub(  '_',   ',',  x ) # second underscore (ranef)
  x <- sub(  '$',   ')',  x ) # close the interval
  t <- grepl('^THETA',x)
  x[t] <- gsub('\\(|)','',x[t]) # drop parens
  class(x) <- 'nms_nonmem'
  x
}

#' Convert NONMEM Parameter Names to Canonical
#' 
#' Converts nms_nonmem to nms_canonical.
#' 
#' @export
#' @family nms
#' @param x nms_nonmem
#' @param ... ignored
#' @return nms_canonical

nms_canonical.nms_nonmem <- function(x,...){
  t <- sub('(^[a-z]+).*', '\\1', tolower(x))    # lowercase term, isolated
  suppressWarnings(
  i <- text2decimal(x)                      # first index
  )
  r <- grepl(',',x)                       # random effects are double-indexed
  suppressWarnings(
  j <- text2decimal(sub('[^,]+','',x)) # second index
  )
  j[!r] <- NA                                   # no second index if not ranef
  i <- padded(i,2)
  j <- padded(j,2)
  d <- ifelse(r, paste(i,j,sep='_'), i)         # one index or two
  o <- paste(t,d,sep='_')
  class(o) <- 'nms_canonical'
  o
}

#' Generate NONMEM Names
#' 
#' Generates NONMEM names.
#' @param x object of dispatch
#' @param ... passed arguments
#' @keywords internal
#' @family nms_nonmem
#' @export
#' @family nms
nms_nonmem <- function(x,...)UseMethod('nms_nonmem')

#' Generate NONMEM Names for Numeric
#' 
#' Generates NONMEM names for numeric by coercing to character.
#' @inheritParams nms_nonmem
#' @export
#' @family nms
#' @keywords internal
nms_nonmem.numeric <- function(x,...)nms_nonmem(as.character(x,...),...)

#' Generate NONMEM Names for Character
#' 
#' Generates NONMEM names for numeric by converting to parsed model.
#' @inheritParams nms_nonmem
#' @export
#' @family nms
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% nms_nonmem
nms_nonmem.character <- function(x,...)nms_nonmem(as.model(x,parse=TRUE,verbose=FALSE, ...), ...)

#' Generate NONMEM Names for Model
#' 
#' Generates NONMEM names for parameters declared in a NONMEM control stream object. 
#PsN uses NONMEM-style names, substituting a comment, if any: everything after the first semicolon, up to the second semicolon if present, without leading/trailing spaces/tabs.
#' 
#' @param x a model designator
#' @param ... passed arguments
#' @return nms_nonmem (character)
#' @family nms
#' @seealso as.model
#' @export
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% as.model %>% nms_nonmem
#' 
nms_nonmem.model <- function(x,...){
  comments <- comments(x,tables=FALSE,...)
  comments <- mutate(comments,item = nms_nonmem.nms_canonical(item))
  res <- comments$item
  class(res) <- 'nms_nonmem'
  res
}

#' Convert Parameter Names to pmxTools
#' 
#' Converts parameter names to pmxTools style.
#' Generic, with method \code{\link{nms_pmx.nms_canonical}}.
#' 
#' @export
#' @keywords internal
#' @family nms
#' @param x object of dispatch
#' @param ... passed
nms_pmx <- function(x, ...)UseMethod('nms_pmx')


#' Convert Canonical Names to pmxTools
#' 
#' Converts nms_canonical to nms_pmx.
#' I.e., theta_01 becomes THETA1, and 
#' omega_01_01 becomes OM1,1.
#' 
#' @export
#' @return nms_pmx (character)
#' @param x nms_canonical
#' @param ... ignored
#' @family nms
#' @importFrom magrittr %<>%
#' @importFrom dplyr case_when
#' @examples
#' library(magrittr)
#' c(
#'    'theta_15',
#'    'omega_01_01',
#'    'omega_01_02',
#'    'sigma_01_01',
#'    'sigma_02_01'
#' ) %>%
#' as_nms_canonical %>%
#' nms_pmx
#' 
nms_pmx.nms_canonical <- function(x, ...){
  x <- as.character(x)
  model <- '^(theta|omega|sigma)_(\\d+)(_(\\d+))?$'
  term <- sub(model, '\\1', x)
  ind1 <- sub(model, '\\2', x)
  ind2 <- sub(model, '\\4', x)
  suppressWarnings({
    d <- data.frame(
      term = toupper(term), 
      ind1 = as.character(as.integer(ind1)), 
      ind2 = as.character(as.integer(ind2))
    )
  })
  d %<>% mutate(
    short = case_when(
      term == 'THETA' ~ 'TH',
      term == 'OMEGA' ~ 'OM',
      term == 'SIGMA' ~ 'SG',
      TRUE ~ term
    )
  )
  d %<>% mutate(com = paste(ind1, ind2, sep = ','))
  d %<>% mutate(name = case_when(ind1 != ind2 ~ short, TRUE ~ term))
  d %<>% mutate(num =  case_when(ind1 != ind2 ~ com, TRUE ~ ind1))
  d %<>% mutate(token = paste0(name, num))
  out <- d$token
  class(out) <- 'nms_pmx'
  out
}

#' Convert Parameter Names to NONMEM
#' 
#' Converts parameter names to NONMEM style.
#' @export
#' @family nm
#' @keywords internal
#' @param x object of dispatch
#' @param ... passed



#' Convert pmxTools Names to Canonical
#' 
#' Converts nms_pmx to nms_canonical.
#' I.e., THETA1 becomes theta_01, and 
#' OM1,1 becomes omega_01_01.
#' 
#' @export
#' @family nms
#' @return nms_canonical (character)
#' @param x nms_pmx
#' @param ... ignored
#' @family nm
#' @importFrom magrittr %<>%
#' @importFrom dplyr case_when
#' @examples
#' library(magrittr)
#' c(
#'    'THETA1',
#'    'OM1,1',
#'    'OM1,2',
#'    'SG1,1',
#'    'SG1,2'
#' ) %>%
#' as_nms_pmx %>%
#' nms_canonical
nms_canonical.nms_pmx <- function(x, ...){
  model <- '^(THETA|OM|OMEGA|SG|SIGMA)(\\d+)(,(\\d+))?$'
  term <- sub(model, '\\1', x) %>% as.character
  ind1 <- sub(model, '\\2', x) %>% as.integer
  ind2 <- sub(model, '\\4', x) %>% as.integer
  
  suppressWarnings({
    d <- data.frame(
      term = tolower(term), 
      ind1 = ifelse(ind1 < 10, paste0('0', ind1), paste0(ind1)),
      ind2 = ifelse(ind2 < 10, paste0('0', ind2), paste0(ind2))
    )
  })
  d %<>% mutate(
    long = case_when(
      term == 'th' ~ 'theta',
      term == 'om' ~ 'omega',
      term == 'sg' ~ 'sigma',
      TRUE ~ term
    )
  )
  d %<>% mutate(
    com = case_when(
      long == 'theta' ~ ind1,
      is.na(ind2) ~ paste(ind1, ind1, sep = '_'),
      TRUE ~ paste(ind1, ind2, sep = '_')
    )
  )
  #d %<>% mutate(name = case_when(ind1 != ind2 ~ long, TRUE ~ term))
  #d %<>% mutate(num =  case_when(ind1 != ind2 ~ com, TRUE ~ ind1))
  #d %<>% mutate(token = term)
  d %<>% mutate(token = paste0(long, '_', com))
  out <- d$token
  class(out) <- 'nms_canonical'
  out
}

#' Coerce to 'nms_nonmem'
#' 
#' Coerces character to 'nms_nonmem'.
#' @export
#' @param x character
#' @param ... ignored
#' @family nms
as_nms_nonmem <- function(x, ...){
  stopifnot(inherits(x, 'character'))
  structure(as.character(x), class = 'nms_nonmem')
}

#' Coerce to 'nms_canonical'
#' 
#' Coerces character to 'nms_canonical'.
#' @export
#' @param x character
#' @param ... ignored
#' @family nms
as_nms_canonical <- function(x, ...){
  stopifnot(inherits(x, 'character'))
  structure(as.character(x), class = 'nms_canonical')
}

#' Coerce to 'nms_psn'
#' 
#' Coerces character to 'nms_psn'.
#' @export
#' @param x character
#' @param ... ignored
#' @family nms
as_nms_psn <- function(x, ...){
  stopifnot(inherits(x, 'character'))
  structure(as.character(x), class = 'nms_psn')
}

#' Coerce to 'nms_pmx'
#' 
#' Coerces character to 'nms_pmx'.
#' @export
#' @param x character
#' @param ... ignored
#' @family nms
as_nms_pmx <- function(x, ...){
  stopifnot(inherits(x, 'character'))
  structure(as.character(x), class = 'nms_pmx')
}

#' Convert pmxTools Names to NONMEM
#' 
#' Converts nms_pmx to nms_nonmem.
#' I.e., THETA1 stays THETA1, but 
#' OM1,1 becomes OMEGA(1,1).
#' 
#' @export
#' @family nms
#' @return nms_nonmem (character)
#' @param x nms_pmx
#' @param ... ignored
#' @family nm
#' @importFrom magrittr %<>%
#' @importFrom dplyr case_when
#' @examples
#' library(magrittr)
#' c(
#'    'THETA1',
#'    'OM1,1',
#'    'OM1,2',
#'    'SG1,1',
#'    'SG1,2'
#' ) %>%
#' as_nms_pmx %>%
#' nms_canonical
nms_nonmem.nms_pmx <- function(x, ...){
  y <- x %>% nms_canonical
  z <- y %>% nms_nonmem
  z
}

#' Convert NONMEM Names to pmxTools
#' 
#' Converts nms_nonmem to nms_pmx.
#' I.e., THETA1 stays THETA1, but 
#' OMEGA(1,1) becomes OM1,1.
#' 
#' @export
#' @family nms
#' @return nms_nonmem (character)
#' @param x nms_pmx
#' @param ... ignored
#' @family nm
#' @importFrom magrittr %<>%
#' @importFrom dplyr case_when
#' @examples
#' library(magrittr)
#' c(
#'    'THETA1',
#'    'OMEGA(1,1)',
#'    'OMEGA(1,2)',
#'    'SIGMA(1,1)',
#'    'SIGMA(1,2)'
#' ) %>%
#' as_nms_pmx %>%
#' nms_canonical
nms_pmx.nms_nonmem <- function(x, ...){
  y <- x %>% nms_canonical
  z <- y %>% nms_pmx
  z
}


#' Translate Parameter Names
#' 
#' Translate among various idioms for expressing parameter names
#' @name rosetta
#' @aliases nms
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 
#' # we generate some nonmem parameter names in nonmem format
#' 1001 %>% nms_nonmem -> nonmem
#' nonmem
#'
#' # from these we generate canonical and pmxTools equivalents
#' canonical <- nms_canonical(nonmem)
#' pmxtools <- nms_pmx(nonmem)
#' canonical
#' pmxtools
#'
#' # We demonstrate equivalence
#' stopifnot(identical(nonmem, nms_nonmem(canonical)))
#' stopifnot(identical(nonmem, nms_nonmem(pmxtools)))
#' stopifnot(identical(canonical, nms_canonical(nonmem)))
#' stopifnot(identical(canonical, nms_canonical(pmxtools)))
#' stopifnot(identical(pmxtools, nms_pmx(nonmem)))
#' stopifnot(identical(pmxtools, nms_pmx(canonical)))
#'
#' # on-the-fly conversions
#' nonmem %>% class
#' nonmem %>% nms_pmx
#' nonmem %>% nms_canonical
#' nonmem %>% nms_canonical %>% nms_pmx
#' nonmem %>% nms_canonical %>% nms_pmx %>% nms_nonmem
#'
#' # comparison
#' data.frame(
#'  can = canonical,
#'  non = nonmem,
#'  pmx = pmxtools,
#'  psn = 1001 %>% nms_psn
#' )
NULL

