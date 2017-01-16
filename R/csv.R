#' Read or Write Standardized CSV

#' Reads or writes CSV files in a standard way.
#' @param x object
#' @param ... passed arguments

as.csv <- function(x,...)UseMethod('as.csv')

#' Treat Character as CSV
#' 
#' Treat a character string as a csv filename.
#' 
#' If x is character, is length one, and is a path to a file, an attempt is made to read the file.  
#' @inheritParams as.csv
#' @param as.is passed to read.csv
#' @param na.strings passed to read.csv
#' @param strip.white passed to read.csv
#' @return data.frame

as.csv.character <- function(x,as.is=TRUE,na.strings=c('',' ','.'),strip.white=T,...){
  stopifnot(length(x)==1)
  stopifnot(file.exists(x))
  y <- utils::read.csv(x,as.is=as.is,na.strings=na.strings,strip.white=strip.white,...)
  y
}

#' Save a Data Frame as CSV
#' 
#' Saves a data.frame as csv.
#' 
#' @inheritParams as.csv
#' @param file passed to write.csv
#' @param na passed to write.csv
#' @param quote passed to write.csv
#' @param row.names passed to write.csv
#' @return invisible data.frame (x)

as.csv.data.frame <- function(x, file, na='.',quote=FALSE,row.names=FALSE,...){
    comma <- sapply(x,function(col)any(col %contains% ','))
  nms <- names(comma)[comma]
  if(length(nms) & quote)warning(
    'quote is false but found comma(s) in ',
    paste(nms,collapse=', ')
  )
  dup <- x[duplicated(x),]
  if(nrow(dup))warning(
    'found duplicate(s) e.g.:\n',
    dup[1,] %>% t %>% paste(collapse=', ')
  )
  utils::write.csv(x,file=file,na=na,quote=quote,row.names=row.names,...)
  invisible(x)
}

