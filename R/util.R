#' Check Whether Text Contains Pattern
#'
#' Checks whether text contains pattern.
#' 
#' @param pattern regular expression
#' @param text character vector to check
#' @param ... arguments to methods
#' @seealso \code{\link{\%contains\%}}
#' @return logical
# @export
# @keywords internal
# @examples
# contains('a',letters)
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
#' @export
#' @family util
#' @keywords internal
#' @examples
#' letters %contains% 'a'
`%contains%` <- function(x,y)contains(y,x)

#' Convert Text to Decimal
#'
#' Scavenge text for first reasonably-inferred numeric value.
#' 
#' @param x character
#' @return numeric
#' @export
#' @family util
#' @keywords internal
#' @examples
#' text2decimal('30 mg')

`text2decimal` <-
function (x) as.numeric(sub("^[^0-9.+-]*([0-9.eE+-]+).*$", "\\1", as.character(x)))

#' Check Whether Elements are Defined
#'
#' Checks whether elements are defined.  Inverse of is.na().
#' 
#' @param x vector
#' @return logical
#' @examples
#' is.defined(c(1,NA,2))
#' @export
#' @keywords internal

is.defined <- function(x)!is.na(x)

#' Compare Sets
#' 
#' Compare sets by evaluating both set differences, and the intersection.
#' @param x vector
#' @param y vector
#' @return list: unique x, unique y, and intersection
#' @keywords internal
#' @family util
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
#' @keywords internal
#' @export
#' @family util
parens <- function(x,...)paste0('(',x,')')

#' Enclose in Arbitrary Characters
#' 
#' Enclose in arbitrary characters
#' @param x vector
#' @param open open string
#' @param close close string
#' @param ... dots
#' @return character
#' @keywords internal
#' @export
#' @family util
enclose <- function(x,open,close,...)paste0(open,x,close)

#' Pad Numeric with Zeros
#' 
#' Pad a numeric vector with leading zeros.
#' @param x numeric
#' @param width desired number of characters
#' @param ... ignored
#' @return character
#' @keywords internal
#' @export
#' @family util
#' 
padded <- function (x, width = 4, ...) 
  sprintf(paste0("%0", width, ".0f"), x)

#' Convert to Best of Numeric or Character
#'
#' Convert to best of numeric or character.
#' 
#' Converts to numeric if doing so creates no new NA; otherwise to character.
#' @param x vector or data.frame
#' @param ... passed to other methods
# @param na.strings strings to treat as NA
#' @keywords internal
#' @export
#' @family util
as.best <- function(x,...)UseMethod('as.best')

#' Convert Dataframe Columns to Best of Numeric or Character
#'
#' Convert data.frame columns to best of numeric or character.
#' 
#' Converts columns to numeric if doing so creates no new NA; otherwise to character. Number-like columns that are nevertheless character are prefixed by default to make this explicit when viewing only a few rows.
#' @param x data.frame
#' @param prefix character to prepend to values in mixed numeric/character columns
#' @param ... passed arguments
#' @return data.frame
#' @describeIn as.best data.frame method
#' @export
#' @family util
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
#' @param x default
#' @param ... ignored
#' @param prefix prefix for viewing numerics
#' @param na.strings strings that should be treated as NA
#' @export
#' @family util
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
#' @keywords internal
#' @export
#' @family util
`runhead` <-
  function(x){#not like last observation
    n <- x != dplyr::lag(x)
    if(length(n)) n[[1]] <- TRUE
    n
  }

#' Impute Missing Vector Values
#' 
#' \code{locf()} implements 'last observation carried forward': \code{NA}'s are imputed with the most recent non-\code{NA} value. \code{nocb()} is the complement: 'next observation carried backward': \code{NA}'s are imputed with the next non-NA value. \code{forbak()} first executes \code{locf()}, then \code{nocb()}, so that even leading NAs are imputed. If even one non-NA value is present, \code{forbak()} should not return any \code{NA}'s. \code{bakfor()} does the reverse.
#' 
#' @param x a vector possibly with some missing values and some non-missing values
#' @return a vector like \code{x}
#' @keywords internal
#' @export 
#' @family util
#' @examples
#' locf(c(NA,1,2,NA,NA,3,NA,4,NA))
#' nocb(c(NA,1,2,NA,NA,3,NA,4,NA))
#' forbak(c(NA,1,2,NA,NA,3,NA,4,NA))
`locf` <-
  function(x){
    good <- !is.na(x)
    positions <- seq(length(x))
    good.positions <- good * positions
    last.good.position <- cummax(good.positions)
    last.good.position[last.good.position==0] <- NA
    x[last.good.position]
  }

#' @describeIn locf forbak locf followed by nocb
#' @export
#' @family util
`forbak` <-
  function(x)nocb(locf(x))

#' @describeIn locf bakfor nocb followed by locf
#' @export
#' @family util
`bakfor` <-
  function(x)locf(nocb(x))

#' @describeIn locf nocb next observation carried backward
#' @export
#' @family util
`nocb` <-
  function(x)rev(locf(rev(x)))
