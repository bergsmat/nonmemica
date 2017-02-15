#' Check Whether Text Contains Pattern
#'
#' Checks whether text contains pattern.
#' 
#' @param pattern regular expression
#' @param text character vector to check
#' @param ... arguments to methods
#' @seealso \code{\link{\%contains\%}}
#' @return logical
#' @examples
#' contains('a',letters)
#' @export

contains <- function(pattern,text,...){
	hits <- regexpr(pattern,text,...)
	hits >=0
}

#' Check Whether x contains y
#'
#' Checks whether x contains y, in natural syntax.
#' 
#' @param x character vector to check
#' @param y pattern
#' @seealso \code{\link{contains}}
#' @return logical
#' @examples
#' letters %contains% 'a'
#' @export

`%contains%` <- function(x,y)contains(y,x)

#' Convert Text to Decimal
#'
#' Scavenge text for first reasonably-inferred numeric value.
#' 
#' @param x character
#' @return numeric
#' @examples
#' text2decimal('30 mg')
#' @export

`text2decimal` <-
function (x) as.numeric(sub("^[^0-9.+-]*([0-9.eE+-]+).*$", "\\1", as.character(x)))

#' Check Whether elements are Defined.
#'
#' Checks whether elements are defined.  Inverse of is.na().
#' 
#' @param x vector
#' @return logical
#' @examples
#' is.defined(c(1,NA,2))
#' @export

is.defined <- function(x)!is.na(x)

#' Compare Sets
#' 
#' Compare sets by evaluating both set differences, and the intersection.
#' @param x vector
#' @param y vector
#' @return list: unique x, unique y, and intersection
#' @examples
#' pool(1:3,2:4)

#' @export
pool <- function(x,y)list(x=setdiff(x,y),y=setdiff(y,x),both=intersect(x,y))

#' Enclose in Parentheses
#' 
#' Enclose in parentheses
#' @param x vector
#' @param ... dots
#' @return character

#' @export
parens <- function(x,...)paste0('(',x,')')
#' Enclose in Arbitrary Characters
#' 
#' Enclose in arbitrary characters
#' @param x vector
#' @param open open string
#' @param close close string
#' @param ... dots
#' @return character

#' @export
enclose <- function(x,open,close,...)paste0(open,x,close)

#' Pad Numeric with Zeros
#' 
#' Pad a numeric vector with leading zeros.
#' @param x numeric
#' @param width desired number of characters
#' @param ... ignored
#' @return character

#' @export
#' 
padded<-function (x, width = 4, ...) 
  sprintf(paste0("%0", width, ".0f"), x)

#' Convert to Best of Numeric or Character
#'
#' Convert to best of numeric or character.
#' 
#' Converts to numeric if doing so creates no new NA; otherwise to character.
#' @param x vector or data.frame
#' @param ... passed to other methods
#' @param prefix character to prepend to values in mixed numeric/character columns
#' @param na.strings strings to treat as NA
#' @export
as.best <- function(x,...)UseMethod('as.best')

#' Convert Dataframe Columns to Best of Numeric or Character
#'
#' Convert data.frame columns to best of numeric or character.
#' 
#' Converts columns to numeric if doing so creates no new NA; otherwise to character. Number-like columns that are nevertheless character are prefixed by default to make this explicit when viewing only a few rows.
#' @inheritParams as.best
#' @return data.frame
#' @describeIn as.best data.frame method
#' @export
as.best.data.frame <- function(x,prefix='#',...){
  for(col in names(x)){
    tryCatch(
      x[[col]] <- as.best(x[[col]],prefix=prefix,...), 
      error = function(e) stop('in column ',col,': ',e$message)
    )
  }
  x
}
#' Convert Vector to Best of Numeric or Character
#'
#' Convert vector to best of numeric or character.
#' 
#' Converts vector to numeric if doing so creates no new NA; otherwise to character. Number-like vectors that are nevertheless character are prefixed by default to make this explicit when viewing only a few rows.
#' @inheritParams as.best
#' @describeIn as.best default method
#' @export
as.best.default <-
  function(x,prefix='',na.strings=c('.','NA',''),...){
    stopifnot(length(prefix)<=1)
    x <- as.character(x)
    x <- sub('^\\s*','',x)
    x <- sub('\\s*$','',x)
    x[x %in% na.strings] <- NA
    y <- suppressWarnings(as.numeric(x))
    newNA <- !is.na(x) & is.na(y)
    if(all(is.na(y)))return(x) # nothing converted to numeric
    if(!any(newNA))return(y) # no loss on conversion to numeric
    if(!length(prefix))stop('character values mixed with numeric, e.g. ', x[newNA][[1]])
    # If we reached here, x has some values coercible to numeric and some not, maybe some NA.
    # Numeric values buried in a character vector are ambiguous
    x[!is.na(y)] <- paste0(prefix,x[!is.na(y)])
    return(x)
  }

#' Find Positions in a Vector that Differ from Previous
#' 
#' Finds postions in a vector that differ from previous.  First position is defined TRUE.
#' @param x vector
#' @return logical
#' @export
`runhead` <-
  function(x){#not like last observation
    n <- x != dplyr::lag(x)
    if(length(n)) n[[1]] <- TRUE
    n
  }

