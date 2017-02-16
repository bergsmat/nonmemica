globalVariables(c('item','.','parameter','estimate','se'))

#' Coerce to NONMEM Control Object
#' 
#' Coerces to NONMEM control stream object.
#' @param x object of dispatch
#' @param ... dots
#' @return nmctl
#' @export
#' @keywords internal
as.nmctl <-
function(x,...)UseMethod('as.nmctl')

#' Coerce NONMEM Control Object to character
#' 
#' Coerces NONMEM control stream object to character.
#' @param x object of dispatch
#' @param ... dots
#' @return nmctl
#' @export
#' @keywords internal
as.character.nmctl <-
function(x,...){
	if(length(x)==0) return(character(0))
	x[] <- lapply(x,as.character) # to accommodate novel underlying object types
	order <- sapply(x,length)
	recnums <- 1:length(x)
	record <- rep(recnums,order)
	flag <- runhead(record)
	content <- as.character(unlist(x))
	nms <- toupper(names(x))
	content[flag] <- paste(paste0('$',nms),content[flag])
	content[flag] <- sub(' $','',content[flag])
	content
}

#' Coerce nmctl to list
#' 
#' Coerces nmctl to list.
#' @param x nmctl
#' @param ... dots
#' @return list
#' @export
#' @keywords internal
as.list.nmctl <-
function(x,...)unclass(x)

#' Coerce character to nmctl
#' Coerces chacter to nmctl.
#' @inheritParams as.nmctl
#' @param pattern pattern to identify record declarations
#' @param head subpattern to identify declaration type
#' @param tail subpattern remaining
#' @param parse whether to convert thetas omegas and sigmas to initList and tables to itemList
#' @return list
#' @describeIn as.nmctl character method
#' @export
as.nmctl.character <-
function(
	x,
	pattern='^\\s*\\$(\\S+)(\\s.*)?$',
	head='\\1',
	tail='\\2',
  parse=FALSE,
	...
){
  if(length(x) == 1){
    class(x) <- if(file.exists(x)) 'filename' else 'modelname'
      return(as.nmctl(x,parse=parse,...))
  }
	flag <- grepl(pattern,x)
	nms <- sub(pattern,head,x)
	nms <- nms[flag]
	nms <- tolower(nms)
	content <- sub(pattern,tail,x)
	content[flag] <- sub('^ ','',content[flag])
	content <- split(content,cumsum(flag))
	content[['0']] <- NULL	
	names(content) <- nms
	class(content) <- c('nmctl',class(content))
	thetas <- names(content)=='theta'
	omegas <- names(content)=='omega'
	sigmas <- names(content)=='sigma'
	tables <- names(content)=='table'
	if(parse)content[thetas] <- lapply(content[thetas],as.initList)
	if(parse)content[omegas] <- lapply(content[omegas],as.initList)
	if(parse)content[sigmas] <- lapply(content[sigmas],as.initList)
	if(parse)content[tables] <- lapply(content[tables],as.itemList)
	content
}

#' Format nmctl
#' 
#' Format nmctl.
#' 
#' Coerces to character.
#' @param x nmctl
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
format.nmctl <-
function(x,...)as.character(x,...)

#' Print nmctl
#' 
#' Print nmctl.
#' 
#' Formats and prints.
#' @param x nmctl
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
print.nmctl <-
function(x,...)print(format(x,...))

#' Read nmctl
#' 
#' Read nmctl.
#' 
#' Reads nmctl from a connection.
#' @param con nmctl connection
#' @param parse whether to convert thetas to initList objects
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
read.nmctl <-
function(con,parse=FALSE,...)as.nmctl(readLines(con),parse=parse,...)

#' Write nmctl
#' 
#' Write nmctl.
#' 
#' writes (formatted) nmctl to file.
#' @param x nmctl
#' @param file passed to write()
#' @param ncolumns passed to write() 
#' @param append passed to write()
#' @param sep passed to write()
#' @param ... dots
#' @return used for side effects
#' @export
#' @keywords internal

write.nmctl <-
function(x, file='data',ncolumns=1,append=FALSE, sep=" ",...){
	out <- format(x)
	write(
		out,
		file=file,
		ncolumns=ncolumns,
		append=append, 
		sep=sep,
		...
	)
}

#' Subset nmctl
#' 
#' Subsets nmctl.
#' @param x nmctl
#' @param ... dots
#' @param drop passed to subset
#' @return nmctl
#' @export
#' @keywords internal
`[.nmctl` <- function (x, ..., drop = TRUE){
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}
#' Select nmctl Element
#' 
#' Selects nmctl element.
#' @param x nmctl
#' @param ... dots
#' @param drop passed to element select
#' @return element
#' @export
#' @keywords internal

`[[.nmctl` <- function (x, ..., drop = TRUE)NextMethod("[[")

#' Convert Filename to nmctl
#' 
#' Converts filename to nmctl.
#' 
#' @inheritParams as.nmctl
#' @param parse convert thetas to initList
#' @return nmctl
#' @describeIn as.nmctl filename method
#' @export 

as.nmctl.filename <- function(x, parse=FALSE, ...)read.nmctl(con=x,parse=parse,...)

#' Convert Modelname to nmctl
#' 
#' Converts modelname to nmctl.
#' 
#' @inheritParams as.nmctl
#' @param verbose whether to display messages
#' @param project path to project directory
#' @param opt alternative specification of project
#' @param rundir model specific run directory
#' @param ctlfile path to model control stream
#' @param ext extension (with dot) for control stream
#' @param parse convert thetas to initList
#' @return nmctl
#' @describeIn as.nmctl modelname method
#' @export 
as.nmctl.modelname <- function(
  x,
  verbose=TRUE,
  project = if(is.null(opt)) getwd() else opt, 
  opt = getOption('project'),
  rundir = file.path(project,x), 
  ctlfile = file.path(rundir,paste0(x,ext)),
  ext = '.ctl',
  parse = TRUE,
  ...
){
  if(verbose)message('converting ',ctlfile)
  read.nmctl(ctlfile,parse=parse,...)
}

#' Extract nmctl record type
#' 
#' Extracts nmctl record type.
#' 
#'@param x nmctl
#'@param ... dots
#'@return nmctltype (list)
#'@export
#'@keywords internal
as.nmctlType <- function(x,...)UseMethod('as.nmctlType')

#' Extract nmctl record type from nmctl
#' 
#' Extracts nmctl record type from nmctl.
#' 
#'@inheritParams as.nmctlType
#'@param type theta omega or sigma
#'@return nmctlType (list)
#'@describeIn as.nmctlType nmctl method
#'@export
as.nmctlType.nmctl <- function(x,type,...){
  y <- x[names(x) %in% type ]
  attr(y,'type') <- type
  class(y) <- 'nmctlType'
  y
}

#' Coerce to itemComments
#' 
#' Coerces to itemComments
#' 
#' @param x object of dispatch
#' @param ... dots
#' @export
#' @keywords internal
as.itemComments <- function(x,...)UseMethod('as.itemComments')

#' Convert nmctlType to itemComments
#' 
#' Converts nmctlType to itemComments
#' 
#' @inheritParams as.itemComments
#' @return data.frame
#' @describeIn as.itemComments nmctlType method
#' @export
#' 
as.itemComments.nmctlType <- function(x,...){
  type <- attr(x,'type')
  y <- list()
  prior <- 0
  for(i in seq_along(x)){
    this <- x[[i]]
    y[[i]] <- as.itemComments(this,type=type, prior=prior)
    prior <- prior + ord(this)
  }
  y <- do.call(rbind,y)
  class(y) <- union('itemComments',class(y))
  y
}

#' Convert nmctl to itemComments
#' 
#' Converts nmctl to itemComments
#' 
#' @inheritParams as.itemComments
#' @param fields data items to scavenge from control stream comments
#' @param expected parameters known from NONMEM output
#' @param na string to use for NA values when writing default metafile
#' @return data.frame
#' @describeIn as.itemComments nmctl method
#' @export
#' 
as.itemComments.nmctl <- function(x,fields=c('symbol','unit','label'),expected=character(0),na=NA_character_,tables=TRUE, ...){
  t <- x %>% as.nmctlType('theta') %>% as.itemComments
  o <- x %>% as.nmctlType('omega') %>% as.itemComments
  s <- x %>% as.nmctlType('sigma') %>% as.itemComments
  b <- x %>% as.nmctlType('table') %>% as.itemComments
  y <- rbind(t,o,s)
  if(tables) y <- rbind(y,b)
  y <- cbind(y[,'item',drop=F], .renderComments(
    y$comment,fields=fields, na=na, ...))
  if(length(expected)) y <- data.frame(stringsAsFactors=F,item=expected) %>% left_join(y,by='item')
  class(y) <- union('itemComments',class(y))
  y
}

.renderComments <- function(x, fields, cumulative = NULL,na, ...){
  if(length(fields) < 1) return(cumulative)
  col <- fields[[1]]
  dat <- sub('^([^;]*);?(.*)$','\\1',x)
  rem <- sub('^([^;]*);?(.*)$','\\2',x)
  dat <- sub('^\\s+','',dat)
  dat <- sub('\\s+$','',dat)
  out <- data.frame(stringsAsFactors=F, col = dat)
  out$col[is.defined(out) & out == ''] <- na
  names(out)[names(out) == 'col'] <- col
  cum <- if(is.null(cumulative)) out else cbind(cumulative,out)
  .renderComments(x=rem,fields=fields[-1],cumulative=cum, na=na)
}

#' Convert to itemList
#' 
#' Converts to itemList.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
as.itemList <- function(x,...)UseMethod('as.itemList')

#' Convert to itemList from Character
#' 
#' Converts to itemlist from character
#' @inheritParams as.itemList
#' @return itemList
#' @export
as.itemList.character <- function(x,...){
  txt <- x
  # for nonmem table items.  'BY' not supported
  x <- sub('FILE *= *[^ ]+','',x) # filename must not contain space
  reserved  <- c(
    'NOPRINT','PRINT','NOHEADER','ONEHEADER',
    'FIRSTONLY','NOFORWARD','FORWARD',
    'NOAPPEND','APPEND',
    'UNCONDITIONAL','CONDITIONAL','OMITTED'
  )
  for(i in reserved) x <- sub(i,'',x) # remove reserved words
  x <- gsub(' +',' ',x) # remove double spaces
  x %<>% sub('^ *','',.) # rm leading spaces
  x %<>% sub(' *$','',.) # rm trailing spaces
  x <- x[!grepl('^;',x)] # rm pure comments
  x <- x[x!=''] # remove blank lines
  # each line is now a set of items followed by an optional comment that applies to the last item
  sets <- sub(' *;.*','',x) # rm first semicolon, any preceding spaces, and all following
  comment <- sub('^[^;]*;','',x) # select only material following the first semicolon
  comment[comment == x] <- '' # if pattern not found
  stopifnot(length(sets) == length(comment)) # one comment per set, even if blank
  sets <- strsplit(sets,c(' ',',')) # sets is now a list of character vectors, possibly length one
  sets <- lapply(sets,as.list) # sets is now a list of lists of character vectors
  for(i in seq_along(sets)){ # for each list of lists of character vectors
    com <- comment[[i]]     # the relevant comment
    len <- length(sets[[i]])# the element on which to place the comment
    for(j in seq_along(sets[[i]])){ # assign each element of each set
      attr(sets[[i]][[j]],'comment') <- if(j == len) com else '' # blank, or comment for last element
    }
  }
  sets <- do.call(c,sets)
  class(sets) <- c('itemList','list')
  attr(sets,'text') <- txt
  sets
}

#' Coerce itemList to Character
#' 
#' Coerces itemList to character.
#' @param x itemList
#' @param ... dots
#' @return character
#' @export
as.character.itemList <- function(x,...){
  attr(x,'text')
}

#' Format itemList
#' 
#' Formats itemList.
#' @param x itemList
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
format.itemList <-function(x,...)as.character(x,...)

#' Print itemList
#' 
#' Prints itemList.
#' @param x itemList
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
print.itemList <-function(x,...)print(format(x,...))

#' Convert itemList to itemComments
#' 
#' Converts itemList to itemComments
#' 
#' @inheritParams as.itemComments
#' @param type item type: table
#' @return data.frame
#' @describeIn as.itemComments initList method
#' @export
#' 

as.itemComments.itemList <- function(x, type, prior, ...){
  item <- sapply(x,as.character)
  comment <- sapply(x,function(i)attr(i,'comment'))
  dex <- cbind(item,comment)
  class(dex) <- union('itemComments',class(dex))
  dex
}


#' Convert initList to itemComments
#' 
#' Converts initList to itemComments
#' 
#' @inheritParams as.itemComments
#' @param type item type: theta, omega, sigma, or table
#' @param prior number of prior items of this type (maybe imporant for numbering)
#' @return data.frame
#' @describeIn as.itemComments initList method
#' @export
#' 

as.itemComments.initList <- function(x, type, prior,...){
  block <- attr(x,'block')
  com <- lapply(x,function(i)attr(i,'comment'))
  com <- sapply(com, function(i){ # ensure single string
    if(length(i) == 0) return('')
    i[[1]]
  })
  stopifnot(length(com) == length(x))
  if(block > 0) stopifnot(block == ord(as.halfmatrix(seq_along(x))))
  block <- block > 0
  dex <- if(block)as.data.frame(as.halfmatrix(com)) else data.frame(
    row = seq_along(com), col=seq_along(com), x=com
  )
  dex$row <- padded(dex$row + prior,2)
  dex$col <- padded(dex$col + prior,2)
  dex$item <- type
  dex$item <- paste(sep='_',dex$item,dex$row)
  if(type %in% c('omega','sigma'))dex$item <- paste(sep='_', dex$item, dex$col)
  dex %<>% rename(comment = x)
  dex %<>% select(item,comment)
  class(dex) <- union('itemComments',class(dex))
  dex
}

#' Identify the order of an initList
#' 
#' Identifies the order of an initList.
#' 
#' Essentially the length of the list, or the length of the diagonal of a matrix (if BLOCK was defined).
#' @param x initList
#' @param ... dots
#' @return numeric
#' @export
#' @keywords internal

ord.initList <- function(x,...){
  block <- attr(x,'block')
  len <- length(x)
  if(is.null(block)) return(len)
  if(block == 0) return(len)
  return(block)
}

#' Identify the order of an itemList
#' 
#' Identifies the order of an itemList.
#' 
#' Essentially the length of the list
#' @param x itemList
#' @param ... dots
#' @return numeric
#' @export
#' @keywords internal

ord.itemList <- function(x,...)length(x)


#' Identify Indices of Initial Estimates
#' 
#' Identifies indices of initial Estimates.
#' @param x object of dispatch
#' @param ... dots
#' @export
#' @keywords internal
initDex <- function(x,...)UseMethod('initDex')

#' Identify Indices of Initial Estimates in nmctl
#' 
#' Identifies record indices of initial estimates for an object of class nmctl. If nmctl has not been parsed, the result is integer(0).  Otherwise, the result is the record numbers for the canonical order of all init objects among theta, omega, and sigma element types, regardless of the number and order of such types. If a block(2) omega is specified between two thetas and one sigma follows, the results could be c(6L, 8L, 7L, 7L, 7L, 9L).

#' @param x nmctl
#' @param ... dots
#' @return integer
#' @export
#' @keywords internal
#' 
initDex.nmctl <- function(x,...){
  i <- seq_along(x)
  t <- i[names(x) == 'theta']
  o <- i[names(x) == 'omega']
  s <- i[names(x) == 'sigma']
  c <- c(t,o,s)
  y <- x[c]
  l <- sapply(y,length)
  parsed <- all(sapply(y,inherits,'initList'))
  if(!parsed)return(integer(0))
  z <- rep(c,times=l)
  z
}

#' Identify Subscripts
#' 
#' Identifies subscripts.
#' @param x object of dispatch
#' @param ... dots
#' @export
#' @keywords internal
initSubscripts <- function(x,...)UseMethod('initSubscripts')

#' Identify Subscripts of Initial Estimates in nmctl
#' 
#' Identifies subscripts of record indices of initial estimates for an object of class nmctl. If nmctl has not been parsed, the result is integer(0).  Otherwise, the result is the element number for each init object within each initList in x (canonical order).

#' @param x nmctl
#' @param ... dots
#' @return integer
#' @export
#' @keywords internal
#' 
initSubscripts.nmctl <- function(x,...){
  i <- seq_along(x)
  t <- i[names(x) == 'theta']
  o <- i[names(x) == 'omega']
  s <- i[names(x) == 'sigma']
  c <- c(t,o,s)
  y <- x[c]
  l <- sapply(y,length)
  parsed <- all(sapply(y,inherits,'initList'))
  if(!parsed)return(integer(0))
  z <- do.call('c',lapply(l,seq_len))
  z <- as.integer(z)
  z
}

#' Create the Updated Version of Something
#' 
#' Creates the updated version of something. Don't confuse with stats::update.
#' 
#' @param x object of dispatch
#' @param ... dots
#' @export
#' @keywords internal
updated <- function(x,...)UseMethod('updated')

#' Create the Updated Version of Numeric
#' 
#' Creates the updated version of numeric by coercing to character.
#' @inheritParams updated
#' @export
updated.numeric <- function(x,...)updated(as.character(x),...)

#' Create the Updated Version of Character
#' 
#' Creates the updated version of character by treating as a modelname. Parses the associated control stream and ammends the initial estimates to reflect model results (as per xml file).
#' 
#' @param x character
#' @param initial values to use for initial estimates (numeric)
#' @param parse whether to parse the initial estimates, etc.
#' @param verbose extended messaging
#' @param ... dots
#' @return nmctl
#' @export
#' @keywords internal
updated.character <- function(x, initial = estimates(x,...), parse= TRUE,verbose=FALSE, ...){
  y <- x %>% as.nmctl(parse=TRUE,verbose=verbose,...)
  initial(y) <- initial
  y
}

#' Coerce to List of Matrices
#' 
#' Coerces to list of matrices.
#' @param x object of dispatch
#' @param ... dots
#' @export
#' @keywords internal
as.matrixList <- function(x,...)UseMethod('as.matrixList')

#' Coerce to List of Matrices from nmctlType
#' 
#' Coerces to list of matrices from nmctlType
#' @param x object of dispatch
#' @param ... dots
#' @export
#' @keywords internal
as.matrixList.nmctlType <- function(x,...){
  lapply(x,as.matrix)
}

#' Coerce to matrix from initList
#' 
#' Coerces to matrix from initList.
#'
#' @param x object of dispatch
#' @param ... dots
#' @return matrix
#' @export
#' @keywords internal
as.matrix.initList <- function(x,...){
  block <- attr(x,'block')
  y <- sapply(x, `[[`, 'init')
  stopifnot(length(y) >= 1)
  if(block != 0){
    y %<>% as.halfmatrix %>% as.matrix
  }else{
    if(length(y) == 1) {
      y %<>% as.matrix
    }else{
      y %<>% diag
    }
  }
  y
}

  