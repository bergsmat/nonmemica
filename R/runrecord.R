#' Parse the Run Record
#' 
#' Parses the run record portion of a problem statement in a NONMEM model.
#' stores it as an attribute of the problem statement.
#' See \url{https://github.com/UUPharmacometrics/PsN/releases/download/4.9.0/runrecord_userguide.pdf}.
#' 
#' @param x character
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family problem
#' @examples 
#' x <- 
#' '$PROB Simpraz - full data set
#' ;; 1. Based on: 1
#' ;; 2. Description:
#' ;;    Added an OMEGA BLOCK(2) for CL and V
#' ;; 3. Label:
#' ;;    Basic model
#' ;; 4. Structural model:
#' ;;    One compartment linear model
#' ;; 5. Covariate model:
#' ;;    No covariates
#' ;; 6. Interindividual variability:
#' ;;    CL, V and KA. BLOCK(2) for CL and V
#' ;; 7. Interoccasion variability:
#' ;; 8. Residual variability:
#' ;;    Proportional
#' ;; 9. Estimation:
#' ;;    FO'
#' con <- textConnection(x)
#' y <- readLines(con)
#' close(con)
#' z <- as.problem(y)
#' at <- attr(z,'problem')
#' names(at)
#' z

as.problem <- function(x, ...){
  y <- x[grepl('^;;',x)]
  x <- x[!grepl('^;;',x)]
  y <- sub('^;;\\s*','',y)
  key <- c(
    'Based on',
    'Description',
    'Label',
    'Structural model',
    'Covariate model',
    'Interindividual variability',
    'Interoccasion variability',
    'Residual variability',
    'Estimation'
  )
  pattern <- paste0(key,':')
  pattern <- paste(pattern, collapse = '|')
  hits <- grepl(pattern,y)
  hit <- cumsum(hits)
  h <- suppressWarnings(max(hit))
  h <- max(h,0)
  if(h > 9)warning('found more than 9 runrecord elements')
  at <- list()
  if(h > 0){
    for(i in seq_len(h)){
      at <- c(at, as.element(y[hit == i]))
    }
  }
  if(any(duplicated(names(at))))warning('found duplicate runrecord element names')
  attr(x,'runrecord') <- at
  class(x) <- union('problem',class(x))
  x
}

as.element <- function(x){
  stopifnot(length(x) >= 1)
  label <- x[[1]]
  value <- x[-1]
  label <- sub('^[0-9]*[.]\\s*','',label)
  trail <- sub('[^:]+:\\s*','',label)
  trail <- sub('\\s*$','',trail)
  label <- sub(':.*','',label)
  if(label == 'Based on'){
    value <- trail
    if(!nchar(trail))warning('found no value for Based on')
  }else{
    if(nchar(trail))warning('found trailing runrecord element text: ',trail)
  }
  out <- list(value)
  names(out) <- label
  out
}

