# We need two object types:  theta list, and individual list members.
# Class 'theta' will be a list of theta members, possibly with attributes.
# Theta members will be vectors: low init up, with a fixed attribute.
# Need get/set functions for individual members, and possibly for list as whole.
# Need to support subset and element select on list.
# Need to support conversion to character.
# Need to support get/set fixed status.

#' Coerce to init
#' 
#' Coerces to init
#'@param x object
#'@param ... dots
#'@export
#'@keywords internal
as.init <- function(x,...)UseMethod('as.init')
#' Coerce init to init
#' 
#' Coerces init to init
#' @inheritParams as.init
#' @param fixed logical
#' @param comment character
#' @param ... dots
#' @return init
#' @describeIn as.init init method
#' @export
as.init.init <- function(x=numeric(0),fixed=FALSE,comment=character(0),...)as.init.numeric(x=x,fixed=fixed,comment=comment,...)

#' Coerces numeric to init
#' @inheritParams as.init
#' @param fixed logical
#' @param comment character
#' @param ... dots
#' @return init
#' @describeIn as.init numeric method
#' @export
as.init.numeric <- function(x=numeric(0),fixed=FALSE,comment=character(0),...){
    #x may be one,two, or three values
	#init, low/init, or low/init/up
	stopifnot(length(x)<=3,is.logical(fixed),inherits(comment,'character'))
	y <- c(-Inf,NA,Inf)
	names(y) <- c('low','init','up')
	class(y) <- c('init',class(y))
	if(length(x)==1)y['init'] <- x
	if(length(x)==2)y[c('low','init')] <- x
	if(length(x)==3)y[c('low','init','up')] <- x
	if(is.na(y['low']))y['low'] <- -Inf
	if(is.na(y['up']))y['up'] <- Inf
  if(y['low'] > y['up']) stop('lower bound must not be greater than upper bound')
  if(!is.na(y['init']))stopifnot(y['low'] <= y['init'],y['init'] <= y['up'])
  if(fixed & is.na(y['init']))stop('initial cannot be fixed if missing')
  if(!fixed & !is.na(y['init']) & y['init']==0)stop('initial cannot be fixed to zero')
	if(fixed) y[c('low','init','up')] <- y['init']
  if(length(comment))comment(y) <- comment
	y
}

#' Coerce init to character
#' 
#' Coerces init to character.
#' @inheritParams as.init
#' @return character
#' @export
#' @keywords internal
as.character.init <- function(x,...){
  fixed <- x['low']==x['init'] & x['init']==x['up']
  com <- comment(x)
  if(is.na(fixed))fixed <- FALSE
  if(!is.na(x['init']) & all(is.infinite(x[c('low','up')]))) x <- x['init']
  if(!is.na(x['init']) & is.infinite(x['up'])) x <- x[c('low','init')]
  x[] <- sapply(x,toupper)
  x[is.na(x)] <- ''
  if(fixed) x <- x['init']
  len <- length(x)
  y <- paste(x,collapse=',')
  if(len>1) y <- parens(y)
  if(fixed) y <- paste(y,'FIXED')
  if(!is.null(com)) {
    com <- as.character(com)
    if(length(com)==1)y <- paste(y,com, sep='; ')
    if(length(com)>1){
      com <- paste('; ',com)
      y <- c(y,com)
    }
  }
  y
}
#' Format init
#' 
#' Formats init.
#' @param x init
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
format.init <-function(x,...)as.character(x,...)
#' Print init
#' 
#' Prints init.
#' @param x init
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
print.init <-function(x,...)print(format(x,...))

#' Check if something is fixed
#' 
#' Checks if something is fixed
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
fixed <- function(x,...)UseMethod('fixed')

#' Set value of fixed attribute
#' 
#' Sets value of fixed attribute.
#' @param x object
#' @param value value to set
#' @export
#' @keywords internal
`fixed<-` <- function(x,value)UseMethod('fixed<-')

#' Check if init is fixed
#' 
#' Checks if init is fixed.
#' @inheritParams fixed
#' @return logical
#' @describeIn fixed init method
#' @export
fixed.init <- function(x,...)!any(is.na(x)) & length(unique(x)) == 1

#' Set fixed init value
#' 
#' Sets fixed init value.
#' @param x object
#' @param value value to
#' @return init
#' @export
#' @keywords internal
`fixed<-.init` <- function(x,value){
  stopifnot(is.logical(value))
  if(is.na(value))stop('NA found where logical required')
  if(fixed(x)==value)return(x)
  if(is.na(x['init']))stop("cannot alter 'fixed' for a missing value")
  if(value)x[c('low','init','up')] <- x['init']
  else{
    #user has requested 'not fixed' on something that is 'fixed'
    x['low'] <- -Inf
    x['up'] <- Inf
  }
  x
}
#' Coerce to initList
#' 
#' Coerces to initList.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
as.initList <- function(x,...)UseMethod('as.initList')

#' Coerce list to initList
#' 
#' Coerces list to initlist.
#' @inheritParams as.initList
#' @param comment character
#' @return initList
#' @describeIn as.initList list method
#' @export
as.initList.list <- function(x,comment=character(0),...){
  stopifnot(length(x)>0,is.character(comment))
  is.init <- sapply(x,inherits,'init')
  class(x) <- c('initList',class(x))
   comment(x) <- if(length(comment))comment else NA_character_
  x
}
#' Coerce initList to character
#' 
#' Coerces initList to character.
#' @param x initList
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
as.character.initList <- function(x,...){
  com <- comment(x)
  if(is.null(com))com <- ''
  else com <- c('',paste(';',com))
  y <- c(com,unlist(lapply(x,as.character)))
  y
}

#' Format initList
#' 
#' Formats initList.
#' @param x initList
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
format.initList <-function(x,...)as.character(x,...)

#' Print initList
#' 
#' Prints initList.
#' @param x initList
#' @param ... dots
#' @return character
#' @export
#' @keywords internal
print.initList <-function(x,...)print(format(x,...))
.comments <- function(x)sub('^[^;]*;?(.*)$','\\1',x)
.space2comma <- function(x){
  if(!length(x))return(x)
  x[[1]] <- gsub(' ',',',x[[1]])
  x
}
.explicit <- function(x){
    halves <- strsplit(x,')',fixed=TRUE)
    halves <- lapply(halves,.space2comma)
}

#' Subset initList
#' 
#' Subsets initList.
#' @param x initList
#' @param ... dots
#' @param drop logical
#' @return initList
#' @export
#' @keywords internal
`[.initList` <- function (x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  val
}
#' Coerce numeric to initList
#' 
#' Coerces numeric to initList
#' @inheritParams as.initList
#' @param fixed logical
#' @param comment character
#' @return initList
#' @describeIn as.initList numeric method
#' @export
as.initList.numeric <- function(x,fixed=FALSE,comment=character(0),...){
  stopifnot(is.logical(fixed),is.character(comment))
  fixed <- rep(fixed,length.out=length(x))
  y <- lapply(
    seq_along(x),
    function(i)as.init(x[[i]],fixed=fixed[[i]])
  )
  y <- as.initList(y)
  if(length(comment)) comment(y) <- comment
  y
}
#' Select init element
#' 
#' Selects init element.
#' @param x init
#' @param name character
#' @return numeric
#' @export
#' @keywords internal
`$.init` <- function(x,name)x[[name]]
#' Set init element
#' 
#' Sets init element.
#' @param x init
#' @param name character
#' @param value numeric
#' @return init
#' @export
#' @keywords internal
`$<-.init` <- function(x,name,value){
  if(!name %in% c('low','init','up'))stop('attempt to set an invalid init element')
  if(is.null(value))stop('attempt to delete a required init element')
  x[name] <- value
  x
}

#' Check if initList is fixed
#' 
#' Checks if initList is fixed.
#' @inheritParams fixed
#' @return logical
#' @describeIn fixed initList method
#' @export
fixed.initList <- function(x,...)sapply(x,fixed)
#' Set fixed attribute of initList
#' 
#' Sets fixed attribute of initList.
#' @param x initList
#' @param value logical
#' @return initList
#' @export
#' @keywords internal
`fixed<-.initList` <- function(x,value){
	stopifnot(is.logical(value))
	value <- rep(value,length.out=length(x))
	for(i in seq_along(value))fixed(x[[i]]) <- value[[i]]
	x
}
.initcomments <- function(x)sub('^[^;]*;?(.*)$','\\1',x)
.initdata <- function(x)sub(';.*','',x)
.initEstimateNum <- function(x){
  # an estimate is initiated by a parenthesis, or by a qualifying character.
  # a qualifying character is a numeric or reserved char.
  # if initiated by parenthesis, it is closed by parenthesis, else by next
  # qualifying character
  # only 0123456789.-+eI( can initiate an estimate. For (, only ) can close it.
  # For the others, the first space or newline ends the numeric region.
  y <- numeric(0)
  openers <- c('0','1','2','3','4','5','6','7','8','9','.','-','+','(')
  closers <- c(' ','\n','\t') #or end of vector
  region <- 0
  parenthetical <- FALSE
  active <- FALSE
  for(i in x){
    if(!active){
      if(i %in% openers){
        region <- region + 1
        active <- TRUE
      }
    }else{
      if(parenthetical){
        if(i==')') active <- FALSE
      }else{
        if(i %in% closers) active <- FALSE
      }
    }
    if(i=='(')parenthetical <- TRUE
    if(i==')')parenthetical <- FALSE
    y <- append(y,region)
  }
  y
}

.initLineNum <- function(x){
  newline <- x=='\n'
  #I am the first character in a line if I follow a newline, or if I am the veryfirst character.
  frst <- lag(newline)
  frst[is.na(frst)] <- TRUE
  linenum <- cumsum(frst)
  linenum
}
#' Coerce character to initList
#' 
#' Coerces character to initList.
#' @inheritParams as.initList
#' @return initList
#' @describeIn as.initList character method
#' @export
as.initList.character <- function(x,...){
  x <- .parseBlock(x)
  block <- attr(x,'block')
  stopifnot(length(x)>0)
  comments <- .initcomments(x)
  data <- .initdata(x)
  data <- paste(data,collapse='\n')
  z <- strsplit(data,NULL)[[1]]
  #classify each character in z as belonging to a particular line and particular estimate.
  est <- .initEstimateNum(z)
  line <- .initLineNum(z)
  inits <- split(z,est)
  inits <- lapply(inits,paste,collapse='')
  globalcom <- comments[unique(line[est==0])]
  if(0 %in% est) inits <- inits[-1]
  inits <- lapply(
    seq_along(inits),
    function(i){
      x <- inits[[i]]
      com <- comments[.initRelComment(i,est,line)]
      com <- com[com!='']
      comment(x) <- com
      x
    }
  )
  inits <- lapply(
    seq_along(inits),
    function(i){
      com <- comment(inits[[i]])
      if(is.null(com))com <- character(0)
      out <- .as.init.character(inits[[i]],comment=com)
      out
    }
  )
  out <- as.initList(inits,comment=globalcom)
  attr(out,'block') <- block
  out
}
.parseBlock <- function(x){
  y <- paste(x,collapse='\n')
  block <- 0
  if(y %contains% '^ *BLOCK'){
    block <- text2decimal(y)
    y <- sub('BLOCK *\\([^)]+\\)','',y)
  }
  y <- strsplit(y,'\n')[[1]]
    attr(y,'block') <- block
  y
}
.initRelComment <- function(i,est,line){
  #need numbers for comment lines for any comment on my line, plus any trailing
  #comments, i.e. those after this line but before a new est is started
  #use comment groups: where runhead est coincides with runhead line
  stopifnot(length(est)==length(line))
  groupstart <- runhead(est) & runhead(line)
  group <- cumsum(groupstart)
  mygroup <- unique(group[est==i])#probably only 1
  mylines <- unique(line[group==mygroup])
  mylines
}
.as.init.character <- function(x,comment=character(0),...){#limited utility
  stopifnot(length(x)==1)
  fixed = FALSE
  if(x %contains% 'FIX|FIXED') fixed <- TRUE
  x <- gsub('FIXED','',x)
  x <- gsub('FIX','',x)
  x <- gsub('\n','',x)
  x <- sub('(','',x,fixed=TRUE)
  x <- sub(')','',x,fixed=TRUE)
  x <- gsub(' +',' ',x)
  x <- sub('^ ','',x)
  x <- sub(' $','',x)
  x <- gsub(',? ',',',x)
  x <- strsplit(x,',')[[1]]
  x <- as.numeric(x)
  x <- as.init(x,fixed=fixed,comment=comment,...)
  x
}
#' Coerce initList to initList
#' 
#' Coerces initList to initList
#' @inheritParams as.initList
#' @return initList
#' @describeIn as.initList initList method
#' @export
as.initList.initList <- function(x,...)x

#' Tweak something
#' 
#' Tweaks something.
#' @param x object
#' @param ... dots
#' @export
#' @family tweak
tweak <- function(x,...)UseMethod('tweak')

#' Tweak init
#' 
#' Tweaks init.
#' @inheritParams tweak
#' @param sd numeric
#' @param digits integer
#' @return init
#' @export
#' @family tweak
tweak.init <- function(x,sd=0.13,digits=3,...){
	scale <- stats::rnorm(1,mean=1,sd=sd)
	y <- x$init * scale
	y <- signif(y,digits=digits)
	if(y < x$low | y > x$up) return(tweak.init(x,sd=sd,digits=digits,...))
	x$init <- y
	x
}
#' Tweak initList
#' 
#' Tweaks initList.
#' @inheritParams tweak
#' @param sd numeric
#' @param digits integer
#' @return initList
#' @export
#' @family tweak
tweak.initList <- function(x,sd=0.13,digits=3,...){
	x[] <- lapply(x,tweak,sd=sd,digits=digits,...)
	x
}
#' Tweak nmctl
#' 
#' Tweaks nmctl.
#' @inheritParams tweak
#' @param sd numeric
#' @param digits integer
#' @return nmctl
#' @export
#' @family tweak
tweak.nmctl <- function(x,sd=0.13,digits=3,...){
	stopifnot('theta' %in% names(x))
	x$theta <- as.initList(x$theta)
	x$theta <- tweak(x$theta,sd=sd,digits=digits,...)
	x
}
	


