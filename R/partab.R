globalVariables(c('symbol','value'))

#' Create Parameter Table
#'
#' Creates a parameter table.
#' 
#' x can be numeric or character model name, assuming project is identified by argument or option.
#' @param x object of dispatch
#' @param ... arguments to methods
#' @seealso \code{\link{partab.character}}
#' @export
#' @family partab
partab <- function(x,...)UseMethod('partab')
#' Create Model Parameter Table from partab
#'
#' Creates a model parameter table from a partab object.
#' 
#' Just returns the object unmodified.
#' @param x partab
#' @param ... ignored
#' @describeIn partab partab method
#' @export
#' @family partab
partab.partab <- function(x,...)x
#' Create Model Parameter Table from Number.
#'
#' Creates a model parameter table from a number.
#' 
#' Just coerces to character and calls partab again.
#' @param x numeric
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family partab
partab.numeric <- function(x,...)partab(as.character(x),...)

#' Create Model Parameter Table from Xml_document
#'
#' Creates a model parameter table from xml_document.
#' 
#' The function evaluates the xpath expression, 
#' supplying arbitrary names for parameter and moment of parameter.
#' @param x xml_document
#' @param xpath character
#' @param param character
#' @param moment character
#' @param ... passed arguments
#' @return data.frame
#' @export
#' @family val_name
#' @keywords internal
val_name <- function(x, xpath, param, moment,...){
  tokenpath <- paste0('//',xpath,'/val')
  valpath   <- paste0(tokenpath,'/@name')
  dat <- data.frame(
    stringsAsFactors=FALSE,
    parameter =  padded(xpath(x,valpath),2),
    x =  as.numeric(xpath(x,tokenpath))
  )
  dat <- mutate(dat,parameter = paste(sep='_',param,parameter))
  names(dat)[names(dat) == 'x'] <- moment
  dat
}

#' Create Indexed Model Parameter Table from Xml_document
#'
#' Creates a model parameter table from xml_document.
#' 
#' The function evaluates the xpath expression, supplying parameter 
#' and moment labels, and documenting indices of rows and columns.
#' @param x xml_document
#' @param xpath character
#' @param param character
#' @param moment character
#' @param ... passed arguments
#' @return data.frame
#' @export
#' @family row_col
#' @keywords internal
row_col <- function(x, xpath, param, moment,...){
  tokenpath <- paste0('//',xpath,'/row/col')
  dat <- as.data.frame(as.halfmatrix(xpath(x,tokenpath)))
  dat <- mutate(dat, parameter = paste(sep='_',param,padded(row,2),padded(col,2)))
  dat <- mutate(dat, offdiag = as.integer(row != col))
  dat <- select(dat, parameter,x, offdiag)
  names(dat)[names(dat) == 'x'] <- moment
  dat
}

#' Create a Parameter Table from Model Name
#'
#' Creates a parameter table from a model name. Pass the project argument or set 
#' the project option.  
#' 
#' Normally you can just call the generic.  Suitable defaults are supplied, but much customization is supported by means of arguments documented here and in called functions.
#' 
#' Metadata can be added to the parameter table two ways: as markup in the control stream, and as a *.def file in the model directory.  See vignette('parameter-table') for details.
#' 
#' @import magrittr
#' @import dplyr
#' @param x a model name (numeric or character)
#' @param verbose set FALSE to suppress messages
#' @param lo the PsN bootstrap lower confidence limit (\%)
#' @param hi the PsN bootstrap upper confidence limit (\%)
#' @param metafile optional metadata for parameter table (see also: fields)
#' @param xmlfile path to xml file
#' @param ctlfile path to control stream
#' @param bootcsv path to PsN bootstrap_results.csv
#' @param strip.namespace whether to strip e.g. nm: from xml elements for easier xpath syntax
#' @param skip number of lines to skip in bootstrap_results.csv
#' @param check.names passed to bootstrap reader
#' @param digits limits numerics to significant digits (use NULL to suppress)
#' @param ci combine bootstrap lo and hi into an enclosed interval
#' @param sep separator for bootstrap interval
#' @param open first character for bootstrap interval
#' @param close last character for bootstrap interval
#' @param format format numerics as character
#' @param fields metadata fields to read from control stream.  See details.
#' @param relative transform standard errors to relative standard errors: rse replaces se
#' @param percent if relative is true, express as percent (else ignore): prse replaces se
#' @param nonzero limit random effects to those with nonzero estimates
#' @param shrinkage whether to include percent shrinkage on random effects
#' @param correlation whether to include correlation of random effects (as percent if \code{percent} is true)
#' @param ... passed to other functions
#' @seealso \code{\link{as.xml_document.character}}
#' @seealso \code{\link{as.bootstrap.character}}
#' @seealso \code{\link{as.model.character}}
#' @seealso \code{\link[csv]{as.csv}}
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% partab
#' 1001 %>% partab(shrinkage = TRUE, correlation = TRUE)
#' @return object of class partab, data.frame
#' @export
#' @family partab
partab.character <- function(
  x,
  verbose=FALSE,
  lo='5',
  hi='95',
  metafile = modelpath(x,'def',...),
  xmlfile = modelpath(x,'xml',...),
  ctlfile = modelfile(x,...),
  bootcsv,
  strip.namespace=TRUE,
  skip=28,
  check.names=FALSE,  
  digits = 3,
  ci = TRUE,
  open = '(',
  close = ')',
  sep = ', ',
  format = TRUE,
  fields = getOption('fields', default = c('symbol','label','unit')),
  relative = TRUE,
  percent=relative,
  nonzero = TRUE,
  shrinkage = FALSE,
  correlation = FALSE,
  ...
){
  # SCAVENGE XML
  y <- as.xml_document(xmlfile, strip.namespace=strip.namespace,...)
  # SCAVENGE BOOTSTRAPS
  args <- list(x = x, skip=skip,check.names=check.names,lo=lo,hi=hi,verbose=verbose)
  if(!missing(bootcsv)) args <- c(args,list(bootcsv=bootcsv))
  args <- c(args,list(...))
  z <- tryCatch(do.call(as.bootstrap,args),error = function(e) if (verbose) e)
  theta   <- val_name(y, 'theta',  'theta','estimate')
  thetase <- val_name(y, 'thetase','theta','se')
  sigma   <- row_col(y, 'sigma',   'sigma','estimate')
  sigmase <- row_col(y, 'sigmase', 'sigma','se')
  omega   <- row_col(y, 'omega',   'omega','estimate')
  omegase <- row_col(y, 'omegase', 'omega','se')
  theta <- left_join(theta, thetase,by='parameter')
  omega <- left_join(omega, omegase,by=c('parameter','offdiag'))
  sigma <- left_join(sigma, sigmase,by=c('parameter','offdiag'))
  theta <- mutate(theta, offdiag = 0)
  etashrink <- xpath(y,'//etashrink/row/col')
  if(length(etashrink) == 0) etashrink <- xpath(y,'//etashrinksd/row/col')
  epsshrink <- xpath(y,'//epsshrink/row/col')
  if(length(epsshrink) == 0) epsshrink <- xpath(y,'//epsshrinksd/row/col')
  etacor <- row_col(y, 'omegac', 'omega','correlation')
  epscor <- row_col(y, 'sigmac','sigma','correlation')
  # 2023-02-06 TTB if nrow(epscor) == 0, epscor$correlation is character(0) (incompatible in bind_rows)
  # implement consistent typing
  theta <- mutate(theta, across(.cols = -c(parameter), as.numeric))
  omega <- mutate(omega, across(.cols = -c(parameter), as.numeric))
  sigma <- mutate(sigma, across(.cols = -c(parameter), as.numeric))
  etacor <- mutate(etacor, across(.cols = -c(parameter), as.numeric))
  epscor <- mutate(epscor, across(.cols = -c(parameter), as.numeric))
  cor <- suppressWarnings(bind_rows(etacor, epscor))
  if(shrinkage && length(etashrink) == sum(omega$offdiag == 0)) omega$shrinkage[omega$offdiag == 0] <- etashrink
  if(shrinkage && length(epsshrink) == sum(sigma$offdiag == 0)) sigma$shrinkage[sigma$offdiag == 0] <- epsshrink
  param <- suppressWarnings(bind_rows(theta, omega, sigma))
  if(correlation && nrow(cor)) param %<>% left_join(cor)
  if(inherits(z,'data.frame')){
    z <- z[-1,] # drop ofv
    need <- nrow(param) - sum(param$offdiag)
    if(nrow(z) < need){
      message('not as many bootstrap estimates as parameters')
    }else{
      z <- z[1:need,]
      names(z) <- c('lo','hi')
      if(verbose)message(
        'matching:\n',
        paste(
          paste(
            sep=':',
            filter(param,offdiag==0)$parameter,
            row.names(z)
          ),
          '\n'
        )
      )
      i <- param$offdiag==0
      param$lo[i] <- z$lo
      param$hi[i] <- z$hi
    }
  }else{
    param$lo <- rep(NA_real_, nrow(param))
    param$hi <- rep(NA_real_, nrow(param))
  }
  param <- select(param, -offdiag)
  if(nonzero){
    param <- filter(param, !(estimate == 0 & parameter %contains% 'omega|sigma'))
  }
  if(relative){
    param <- mutate(param, se = abs(as.numeric(se) / estimate)) # rename rse below
    if(percent){
    param <- mutate(param, se = se * 100) # rename prse below
    }
  }
  if(percent){
    if('correlation' %in% names(param)){
      param <- mutate(param, correlation = correlation * 100)
    }
  }
  if(length(digits)){
    param <- mutate(param, estimate = signif(estimate, digits))
    param <- mutate(param, se =  signif(se,digits))
    param <- mutate(param, lo =  signif(lo,digits))
    param <- mutate(param, hi =  signif(hi,digits))
    if('correlation' %in% names(param)) param <- mutate(param, correlation = signif(correlation,digits))
    if('shrinkage' %in% names(param)) param <- mutate(param, shrinkage = signif(shrinkage,digits))
  }
  if(format){
    param <- mutate(param, estimate = as.character(estimate))
    param <- mutate(param, se = as.character(se))
    param <- mutate(param, lo = as.character(lo))
    param <- mutate(param, hi = as.character(hi))
  }
  if(all(is.na(param$lo)) && all(is.na(param$hi))) param <- select(param,-lo,-hi)
  if(ci && 'lo' %in% names(param)){
    blank <- is.na(param$lo) & is.na(param$hi)
    param <- mutate(param, ci = enclose(paste(sep=sep, lo, hi),open,close))
    param <- select(param, -lo, -hi)
    param$ci[blank] <- ''
  }
  if(relative && percent) param <- rename(param,prse = se)
  if(relative && !percent) param <- rename(param,rse = se)
  meta <- definitions(x, ctlfile=ctlfile, metafile=metafile, fields = fields, ...)
  meta <- rename(meta, parameter = item)
  param <- left_join(param, meta,by='parameter')
  class(param) <- union('partab', class(param))
  param
}
