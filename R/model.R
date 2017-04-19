globalVariables(c('item','.','parameter','estimate','se'))

#' Coerce to NONMEM Control Object
#' 
#' Coerces to NONMEM control stream object.
#' @param x object of dispatch
#' @param ... dots
#' @return model
#' @export
#' @keywords internal
as.model <-
function(x,...)UseMethod('as.model')

#' Coerce NONMEM Control Object to character
#' 
#' Coerces NONMEM control stream object to character.
#' @param x object of dispatch
#' @param ... dots
#' @return model
#' @export
#' @keywords internal
as.character.model <- function(x,...){
	if(length(x)==0) return(character(0))
  meta <- x[sapply(x,inherits,'items') | sapply(x,inherits,'inits')]
  meta <- lapply(meta, comwidth)
  widths <- maxWidths(meta)
	x[] <- lapply(x,as.character,widths = widths) # to accommodate novel underlying object types
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

#' Coerce model to list
#' 
#' Coerces model to list.
#' @param x model
#' @param ... dots
#' @return list
#' @export
#' @keywords internal
as.list.model <-
function(x,...)unclass(x)

#' Coerce to Model from Numeric
#' 
#' Coerces to model from numeric by coercing to character.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
as.model.numeric <- function(x,...)as.model(as.character(x),...)

#' Coerce character to model
#' Coerces chacter to model.
#' @inheritParams as.model
#' @param pattern pattern to identify record declarations
#' @param head subpattern to identify declaration type
#' @param tail subpattern remaining
#' @param parse whether to convert thetas omegas and sigmas to inits and tables to items
#' @return list
#' @describeIn as.model character method
#' @export
as.model.character <-
function(
	x,
	pattern='^\\s*\\$(\\S+)(\\s.*)?$',
	head='\\1',
	tail='\\2',
  parse=TRUE,
	...
){
  if(length(x) == 1){
    if(!file.exists(x))x <- modelfile(x,...)
    if(!file.exists(x))stop(x, ' does not exist as a file')
    x <- readLines(x)
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
	class(content) <- c('model',class(content))
	thetas <- names(content)=='theta'
	omegas <- names(content)=='omega'
	sigmas <- names(content)=='sigma'
	tables <- names(content)=='table'
	if(parse)content[thetas] <- lapply(content[thetas],as.inits)
	if(parse)content[omegas] <- lapply(content[omegas],as.inits)
	if(parse)content[sigmas] <- lapply(content[sigmas],as.inits)
	if(parse)content[tables] <- lapply(content[tables],as.items)
	content
}

#' Format model
#' 
#' Format model.
#' 
#' Coerces to character.
#' @param x model
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
format.model <-
function(x,...)as.character(x,...)

#' Print model
#' 
#' Print model.
#' 
#' Formats and prints.
#' @param x model
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
print.model <-
function(x,...)print(format(x,...))

#' Read model
#' 
#' Read model.
#' 
#' Reads model from a connection.
#' @param con model connection
#' @param parse whether to convert thetas to inits objects
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
read.model <-
function(con,parse=TRUE,...)as.model(readLines(con),parse=parse,...)

#' Write model
#' 
#' Write model.
#' 
#' writes (formatted) model to file.
#' @param x model
#' @param file passed to write()
#' @param ncolumns passed to write() 
#' @param append passed to write()
#' @param sep passed to write()
#' @param ... dots
#' @return used for side effects
#' @export
#' @keywords internal

write.model <-
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

#' Subset model
#' 
#' Subsets model.
#' @param x model
#' @param ... dots
#' @param drop passed to subset
#' @return model
#' @export
#' @keywords internal
`[.model` <- function (x, ..., drop = TRUE){
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}
#' Select model Element
#' 
#' Selects model element.
#' @param x model
#' @param ... dots
#' @param drop passed to element select
#' @return element
#' @export
#' @keywords internal

`[[.model` <- function (x, ..., drop = TRUE)NextMethod("[[")


#' Extract Thetas
#' 
#' Extracts thetas.
#' 
#'@param x object
#'@param ... passed arguments
#'@export
#'@keywords internal
as.theta <- function(x,...)UseMethod('as.theta')

#' Extract Thetas from Model
#' 
#' Extracts thetas from model.
#' 
#'@param x model
#'@param ... passed arguments
#'@return theta (subset of model)
#'@export
#'@keywords internal
as.theta.model <- function(x,...){
  y <- x[names(x) %in% 'theta' ]
  class(y) <- union(c('theta','records'), class(y))
  y
}
#' Extract Omegas
#' 
#' Extracts omegas.
#' 
#'@param x object
#'@param ... passed arguments
#'@export
#'@keywords internal
as.omega <- function(x,...)UseMethod('as.omega')

#' Extract Omegas from Model
#' 
#' Extracts omegas from model.
#' 
#'@param x model
#'@param ... passed arguments
#'@return omega (subset of model)
#'@export
#'@keywords internal
as.omega.model <- function(x,...){
  y <- x[names(x) %in% 'omega' ]
  class(y) <- union(c('omega','records'), class(y))
  y
}
#' Extract Sigmas
#' 
#' Extracts sigmas.
#' 
#'@param x object
#'@param ... passed arguments
#'@export
#'@keywords internal
as.sigma <- function(x,...)UseMethod('as.sigma')

#' Extract Sigmas from Model
#' 
#' Extracts sigmas from model.
#' 
#'@param x model
#'@param ... passed arguments
#'@return sigma (subset of model)
#'@export
#'@keywords internal
as.sigma.model <- function(x,...){
  y <- x[names(x) %in% 'sigma' ]
  class(y) <- union(c('sigma','records'), class(y))
  y
}
#' Extract Tables
#' 
#' Extracts tables.
#' 
#'@param x object
#'@param ... passed arguments
#'@export
#'@keywords internal
as.tab <- function(x,...)UseMethod('as.tab')

#' Extract Tables from Model
#' 
#' Extracts tables from model.
#' 
#'@param x model
#'@param ... passed arguments
#'@return tab (subset of model)
#'@export
#'@keywords internal
as.tab.model <- function(x,...){
  y <- x[names(x) %in% 'table' ]
  class(y) <- union(c('tab','records'), class(y))
  y
}

#' Extract Comments
#' 
#' Extracts comments.
#' 
#' @param x object of dispatch
#' @param ... dots
#' @export
#' @keywords internal
comments <- function(x,...)UseMethod('comments')

#' Extract Comments from Records
#' 
#' Extracts comments from records.
#' 
#' @inheritParams comments
#' @return data.frame
#' @describeIn comments record method
#' @export
#'@keywords internal
#' 
comments.records <- function(x,...){
  y <- list()
  prior <- 0
  type = class(x)[[1]]
  for(i in seq_along(x)){
    this <- x[[i]]
    y[[i]] <- comments(this, type=type, prior=prior)
    prior <- prior + ord(this)
  }
  y <- do.call(rbind,y)
  class(y) <- union('comments',class(y))
  y
}

#' Extract Comments from Model
#' 
#' Extracts comments from model.
#' 
#' @param x model
#' @param ... passed arguments
#' @param fields data items to scavenge from control stream comments
#' @param expected parameters known from NONMEM output
#' @param na string to use for NA values when writing default metafile
#' @param tables whether to include table comments
#' @return data.frame
#' @describeIn comments model method
#' @export
#' 
comments.model <- function(
  x,
  fields=c('symbol','unit','label'),
  expected=character(0),
  na=NA_character_,
  tables=TRUE, 
  ...
){
  t <-  comments(as.theta(x))
  o <-  comments(as.omega(x))
  s <-  comments(as.sigma(x))
  b <-  comments(as.tab(x))
  y <- rbind(t,o,s)
  if(tables) y <- rbind(y,b)
  y <- cbind(y[,'item',drop=F], .renderComments(
    y$comment,fields=fields, na=na, ...))
  if(length(expected)) y <- left_join(data.frame(stringsAsFactors=F,item=expected), y, by='item')
  class(y) <- union('comments',class(y))
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

#' Convert to Items
#' 
#' Converts to items.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
as.items <- function(x,...)UseMethod('as.items')

#' Convert to Items from Character
#' 
#' Converts to items from character
#' @inheritParams as.items
#' @return items
#' @export
#' @keywords internal
as.items.character <- function(x,...){
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
  x <- sub('^ *','',x) # rm leading spaces
  x <- sub(' *$','',x) # rm trailing spaces
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
  class(sets) <- c('items','list')
  attr(sets,'text') <- txt
  sets
}

#' Format Items
#' 
#' Formats items.
#' @param x items
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
format.items <-function(x,...)as.character(x,...)

#' Print Items
#' 
#' Prints items.
#' @param x items
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
print.items <-function(x,...)print(format(x,...))

#' Extract Comments from Items
#' 
#' Extracts comments from items.
#' 
#' @inheritParams comments
#' @return data.frame
#' @describeIn comments items method
#' @export
#' 

comments.items <- function(x, ...){
  item <- sapply(x,as.character)
  comment <- sapply(x,function(i)attr(i,'comment'))
  dex <- cbind(item,comment)
  class(dex) <- union('comments',class(dex))
  dex
}


#' Extract Comments from Inits
#' 
#' Extracts comments from inits.
#' 
#' @inheritParams comments
#' @param type item type: theta, omega, sigma (tables give items not inits)
#' @param prior number of prior items of this type (maybe imporant for numbering)
#' @return data.frame
#' @describeIn comments inits method
#' @export
#' 
comments.inits <- function(x, type, prior,...){
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
  dex <- rename(dex,comment = x)
  dex <- select(dex,item,comment)
  class(dex) <- union('comments',class(dex))
  dex
}

#' Identify the order of an inits
#' 
#' Identifies the order of an inits.
#' 
#' Essentially the length of the list, or the length of the diagonal of a matrix (if BLOCK was defined).
#' @param x inits
#' @param ... dots
#' @return numeric
#' @export
#' @keywords internal

ord.inits <- function(x,...){
  block <- attr(x,'block')
  len <- length(x)
  if(is.null(block)) return(len)
  if(block == 0) return(len)
  return(block)
}

#' Identify the Order of an Items Object
#' 
#' Identifies the order of an items object.
#' 
#' Essentially the length of the list
#' @param x items
#' @param ... dots
#' @return numeric
#' @export
#' @keywords internal

ord.items <- function(x,...)length(x)


#' Identify Indices of Initial Estimates
#' 
#' Identifies indices of initial Estimates.
#' @param x object of dispatch
#' @param ... dots
#' @export
#' @keywords internal
initDex <- function(x,...)UseMethod('initDex')

#' Identify Indices of Initial Estimates in model
#' 
#' Identifies record indices of initial estimates for an object of class model. If model has not been parsed, the result is integer(0).  Otherwise, the result is the record numbers for the canonical order of all init objects among theta, omega, and sigma element types, regardless of the number and order of such types. If a block(2) omega is specified between two thetas and one sigma follows, the results could be c(6L, 8L, 7L, 7L, 7L, 9L).

#' @param x model
#' @param ... dots
#' @return integer
#' @export
#' @keywords internal
#' 
initDex.model <- function(x,...){
  i <- seq_along(x)
  t <- i[names(x) == 'theta']
  o <- i[names(x) == 'omega']
  s <- i[names(x) == 'sigma']
  c <- c(t,o,s)
  y <- x[c]
  l <- sapply(y,length)
  parsed <- all(sapply(y,inherits,'inits'))
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

#' Identify Subscripts of Initial Estimates in model
#' 
#' Identifies subscripts of record indices of initial estimates for an object of class model. If model has not been parsed, the result is integer(0).  Otherwise, the result is the element number for each init object within each inits in x (canonical order).

#' @param x model
#' @param ... dots
#' @return integer
#' @export
#' @keywords internal
#' 
initSubscripts.model <- function(x,...){
  i <- seq_along(x)
  t <- i[names(x) == 'theta']
  o <- i[names(x) == 'omega']
  s <- i[names(x) == 'sigma']
  c <- c(t,o,s)
  y <- x[c]
  l <- sapply(y,length)
  parsed <- all(sapply(y,inherits,'inits'))
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
#' @keywords internal
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
#' @return model
#' @export
updated.character <- function(x, initial = estimates(x,...), parse= TRUE,verbose=FALSE, ...){
  y <- as.model(x, parse=TRUE,verbose=verbose,...)
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
as.matrices <- function(x,...)UseMethod('as.matrices')

#' Coerce to List of Matrices from Records
#' 
#' Coerces to list of matrices from Records
#' @param x object of dispatch
#' @param ... dots
#' @export
#' @keywords internal
as.matrices.records <- function(x,...){
  y <- lapply(x,as.matrices)
  z <- do.call(c,y)
  z
}

#' Coerce to Matrices from Inits
#' 
#' Coerces to matrices from inits. Non-block inits is expanded into list of matrices.
#'
#' @param x object of dispatch
#' @param ... dots
#' @return matrices
#' @export
#' @keywords internal
as.matrices.inits <- function(x,...){
  block <- attr(x,'block')
  y <- sapply(x, `[[`, 'init')
  stopifnot(length(y) >= 1)
  if(block != 0) return(list(as.matrix(as.halfmatrix(y))))
  return(lapply(y,as.matrix))
}

  