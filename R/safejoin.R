#' Join Data Safely
#'
#' Joins data safely.  Generic, with method for data.frame.
#' @param x object of dispatch
#' @param ... arguments to methods
#' @seealso \code{\link{safe_join.data.frame}}
#' @family safe_join
#' @export
safe_join <- function(x, ...)UseMethod('safe_join')

#' Join Data Frames Safely
#' 
#' Joins data frames safely. I.e., a left join that 
#' cannot alter row order or number.  Supports the
#' case where you only intend to augment existing
#' rows with additional columns and are expecting
#' singular matches. Gives an error if row order
#' or number would have been altered by a left join.
#' @param x data.frame
#' @param y data.frame
#' @param ... passed to dplyr::left_join
#' @family safe_join
#' @export
#' @importFrom dplyr left_join
#' @examples 
#' library(magrittr)
#' x <- data.frame(code = c('a','b','c'), value = c(1:3))
#' y <- data.frame(code = c('a','b','c'), roman = c('I','II','III'))
#' x %>% safe_join(y)
#' try(
#' x %>% safe_join(rbind(y,y))
#' )

safe_join.data.frame <- function(x, y, ...){
  x$safe_join <- 1:nrow(x)
  before <- x$safe_join
  z <- left_join(x, y, ...)
  after <- z$safe_join
  stopifnot(identical(length(before), length(after)))
  stopifnot(all(before == after))
  z$safe_join <- NULL
  z
}
