#' Calculate Time of Most Recent Dose
#'
#' Calculates time of most recent dose.
#' @param x a numeric vector of event times
#' @param ref length x vector of reference dose times
#' @param addl length x integer: number of additional doses
#' @param ii length x numeric: interdose interval for addl
#' @param pre assume that simultaneous sample precedes implied dose
#' @param ... ignored
#' @export
#' @return numeric
#' @seealso \code{\link{tad}}

tod <- function( 
  x,
  ref,
  addl,
  ii,
  pre = T,
  ...
  
){ 
  z <- ref
  hi <- max(addl, na.rm = TRUE)
  if (is.na(hi) | is.nan(hi) | is.infinite(hi)) return(z)
  for (trial in 0:hi) {
    dosetime <- ref + trial * ii
    update <- 
      !is.na(dosetime) & 
      !is.na(x) & 
      !is.na(addl) & 
      trial <= addl &
      if(pre) dosetime < x else dosetime <= x
    z[update] <- dosetime[update]
  }
  z
}

#' Calculate time since most recent dose.
#'
#' Calculate time since most recent dose.
#' @param x a numeric vector of event times
#' @param dose length x logical indicating which of x are dose times
#' @param addl length x integer: number of additional doses
#' @param ii length x numeric: interdose interval for addl
#' @param index length x factor (optional) indicating subgroups to evaluate
#' @param pre assume that simultaneous sample precedes implied dose
#' @param ... passed to tod()
#' @export
#' @return numeric
#' @seealso \code{\link{tod}}
#' @examples
#' data(tad1)
#' x <- tad1
#' head(x)
#' x$tad <- tad(
#'    x = x$TIME,
#'    dose = x$EVID %in% c(1,4) & is.na(x$C),
#'    addl = x$ADDL,
#'    ii = x$II,
#'    index = x$ID
#'  )
#' head(x)

tad <- function(
  x,
  dose=rep(FALSE,length(x)),
  addl=rep(0,length(x)),
  ii= rep(0,length(x)),
  index = rep(1,length(x)),
  pre = TRUE,
  ...
){
  stopifnot(
    length(dose) == length(x),
    length(addl) == length(x),
    length(ii) == length(x),
    length(index) == length(x),
    all(addl %% 1 == 0,na.rm=T)
  )
  pos <- seq_along(x)
  dose <- as.logical(dose)
  dose[is.na(dose)] <- FALSE
  valid <- ifelse(dose,pos,NA)
  r <- stats::ave(valid,index,cumsum(dose),FUN=function(i)i[1])
  t <-  tod(
    x = x, 
    ref  =    x[r], 
    addl = addl[r], 
    ii   =   ii[r],
    pre = pre,
    ...
  )
  tad <- x - t
  tad
}

#' A NONMEM-like dataset
#'
#' A dataset showing dose and and observation records for several subjects.
#' Doses are duplicated across compartments 1 and 2 as for mixed absorption modeling.
#' C,ID,TIME,EVID,CMT,AMT,RATE,ADDL,II,DV
#'
#' \itemize{
#'   \item C. An exclusion flag, NA by default, or 'C'.
#'   \item ID. Integer subject identifier.
#'   \item TIME. Numeric event time (h).
#'   \item EVID. Event type identifier: observation (0) or dose (1).
#'   \item CMT. Event compartment: dose (1), central (2) or peripheral (4).
#'   \item AMT. Amount of dose (mg).
#'   \item RATE. NONMEM RATE item.
#'   \item ADDL. Number of additional doses, or NA for observations.
#'   \item II. Interdose interval for additional doses, or NA for observations.
#'   \item DV. Observation placeholder.
#' }
#' @docType data
#' @name tad1
#' @usage data(tad1)
#' 
NULL