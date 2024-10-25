#' Coerce to init
#' 
#' Coerces to init
#' @param x object
#' @param ... dots
#' @export
#' @family as.init
#' @keywords internal
as.init <- function(x,...)UseMethod('as.init')
#' Coerce init to init
#' 
#' Coerces init to init
#' @param x init
#' @param fixed logical
#' @param comment character
#' @param ... passed arguments
#' @return init
#' @family as.init
#' @export
as.init.init <- function(x=numeric(0),fixed=FALSE,comment=character(0),...)as.init.numeric(x=x,fixed=fixed,comment=comment,...)

#' Coerces numeric to init
#' @param x numeric
#' @param fixed logical
#' @param comment character
#' @param ... ignored
#' @return init
#' @keywords internal
#' @export
#' @family as.init
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
  # if(!fixed & !is.na(y['init']) & y['init'] == 0)warning('initial should be fixed if zero\n')
	if(fixed) y[c('low','init','up')] <- y['init']
  if(length(comment))comment(y) <- comment
	y
}

#' Coerce init to character
#' 
#' Coerces init to character.
#' @param x init
#' @param ... ignored
#' @return character
#' @export
#' @family as.character
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
#' @param ... passed arguments
#' @return character
#' @export
#' @family format
#' @keywords internal
format.init <-function(x,...)as.character(x,...)
#' Print init
#' 
#' Prints init.
#' @param x init
#' @param ... passed arguments
#' @return character
#' @export
#' @family print
#' @keywords internal
print.init <-function(x,...)print(format(x,...))

#' Check if something is fixed
#' 
#' Checks if something is fixed
#' @param x object
#' @param ... passed arguments
#' @export
#' @family fixed
#' @keywords internal
fixed <- function(x,...)UseMethod('fixed')

#' Set value of fixed attribute
#' 
#' Sets value of fixed attribute.
#' @param x object
#' @param value value to set
#' @export
#' @family fixed
#' @keywords internal
`fixed<-` <- function(x,value)UseMethod('fixed<-')

#' Check if init is fixed
#' 
#' Checks if init is fixed.
#' @param x init
#' @return logical
#' @param ... ignored
#' @export
#' @family fixed
fixed.init <- function(x,...)!any(is.na(x)) & length(unique(x)) == 1

#' Set fixed init value
#' 
#' Sets fixed init value.
#' @param x object
#' @param value value to
#' @return init
#' @export
#' @family fixed
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
#' Coerce to inits
#' 
#' Coerces to inits.
#' @param x object
#' @param ... dots
#' @export
#' @family as.inits
#' @keywords internal
as.inits <- function(x,...)UseMethod('as.inits')

#' Coerce list to inits
#' 
#' Coerces list to inits.
#' @param x list
#' @param ... passed arguments
#' @param comment character
#' @return inits
#' @export
#' @family as.inits
as.inits.list <- function(x,comment=character(0),...){
  stopifnot(length(x)>0,is.character(comment))
  is.init <- sapply(x,inherits,'init') ## test?
  class(x) <- c('inits',class(x))
   comment(x) <- if(length(comment))comment else '' # NA_character_
  x
}
#' Coerce inits to character
#' 
#' Coerces inits to character.
#' @param x inits
#' @param ... passed arguments
#' @return character
#' @export
#' @family as.character
#' @keywords internal
as.character.inits <- function(x,pretty = TRUE, sep = ';', delim = ' ; ', widths = comwidth(x), ...){
  block <- attr(x, 'block')
  com <- comment(x)
  padded <- attr(x,'padded')
  if(is.null(padded)) padded <- FALSE
  if(!is.null(com))if(!is.na(com))if(com != '') com <- paste(sep,com)
  y <- c(com,unlist(lapply(x,as.character)))
  if(padded) y <- c(y,rep('',padded))
  if(!is.null(block))if(block) y[[1]] <- paste(y[[1]], paste0('BLOCK',parens(block)))
  if(pretty) y <- sapply(y,prettycom, widths, sep = sep, delim = delim, ...)
  y
}

#' Coerce Items to Character
#' 
#' Coerces items to character.
#' @param x items
#' @param pretty logical
#' @param sep input delimiter
#' @param delim output delimiter
#' @param widths desired widths
#' @param ... passed arguments
#' @return character
#' @keywords internal
#' @export
#' @family as.character
as.character.items <- function(x,pretty = TRUE, sep = ';', delim = ' ; ', widths = comwidth(x), ...){
  y <- attr(x,'text')
  if(pretty) y <- sapply(y,prettycom, widths, sep = sep, delim = delim, ...)
  y
}

#' Calculate Comment Widths for Items
#' 
#' Calculates comment widths for items
#' @param x object
#' @param ... passed arguments
#' @export
#' @family comwidth
#' @keywords internal
comwidth.items <- function(x, ...){
  y <- attr(x,'text')
  z <- comwidth(y)
  z
}

#' Calculate Comment Widths for Inits
#' 
#' Calculates comment widths for inits
#' @param x object
#' @param ... passed arguments
#' @export
#' @family comwidth
#' @keywords internal
comwidth.inits <- function(x, ...){
  com <- comment(x)
  y <- c(com,unlist(lapply(x,as.character)))
  z <- comwidth(y)
  z
}

#' Calculate Comment Widths
#' Calculates comment widths.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family comwidth
#' @keywords internal
comwidth <- function(x,...)UseMethod('comwidth')

#' Calculate Comment Widths for Character
#' 
#' Calculates comment widths for character.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family comwidth
#' @keywords internal
comwidth.character <- function(x,...){
  y <- lapply(x,comwidthOne)
  w <- maxWidths(y,...)
  w
}

#' Calculate Maximum Widths
#' 
#' Calculates maximum widths
#' @param x object
#' @param ... passed arguments
#' @export
#' @family maxWidths
#' @keywords internal
maxWidths <- function(x,...)UseMethod('maxWidths')

#' Calculate Maximum Widths for List
#' 
#' Calculates maximum widths for list
#' @param x object
#' @param ... passed arguments
#' @export
#' @family maxWidths
#' @keywords internal
maxWidths.list <- function(x,...){
  if(length(x) == 0) return(0)
  maxlen <- max(sapply(x,length))
  for(i in seq_along(x)){
    len <- length(x[[i]])
    for(j in seq_len(maxlen - len)){
      x[[i]] <- c(x[[i]], 0)
    }
  }
  w <- do.call(cbind,x)
  w <- apply(w,1,max)
  w
}

#' Calculate Comment Widths for One Element
#' 
#' Calculates comment widths for one element.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family comwidthOne
#' @keywords internal
comwidthOne <- function(x,...)UseMethod('comwidthOne')

#' Calculate Comment Widths for One Element of Character
#' 
#' Calculates Comment Widths for one element of character
#' @param x object
#' @param ... passed arguments
#' @param split comment separator
#' @export
#' @family comwidthOne
#' @keywords internal
comwidthOne.character <- function(x,split = ';', ...){
  if(!grepl(split, x)) return(0)
  y <- strsplit(x, split = split)[[1]]
  y <- sub('^\\s+', '', y)
  y <- sub('\\s+$', '', y)
  z <- sapply(y, nchar)
  z <- as.integer(z)
  z
}

#' Pretty-print a Comment
#' 
#' Pretty-prints a comment.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family prettycom
#' @keywords internal
prettycom <- function(x, ...)UseMethod('prettycom')

#' Pretty-print a Comment for Character
#' 
#' Pretty-prints a comment for character.
#' @param x ... object
#' @param widths integer
#' @param sep input separator
#' @param delim output separator
#' @param ... passed arguments
#' @export
#' @family prettycom
#' @keywords internal
prettycom.character <- function(x, widths, sep, delim, ...){
  stopifnot(length(x) == 1)
  y <- strsplit(x, sep)[[1]]
  y <- sub('^\\s+', '', y)
  y <- sub('\\s+$', '', y)
  stopifnot(length(widths) >= length(y))
  for(i in seq_along(y)){
    have <- nchar(y[[i]])
    need <- widths[[i]]
    pad <- need - have
    pad <- max(pad, 0)
    tail <- rep(' ',pad)
    tail <- paste(tail, collapse = '')
    y[[i]] <- paste0(y[[i]], tail)
  }
  z <- paste(y, collapse = delim)
  z
}

#' Format inits
#' 
#' Formats inits.
#' @param x inits
#' @param ... passed arguments
#' @return character
#' @export
#' @family format
#' @keywords internal
format.inits <-function(x,...)as.character(x,...)

#' Print inits
#' 
#' Prints inits.
#' @param x inits
#' @param ... passed arguments
#' @return character
#' @export
#' @family print
#' @keywords internal
print.inits <-function(x,...)print(format(x,...))
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

#' Subset inits
#' 
#' Subsets inits.
#' @param x inits
#' @param ... passed arguments
#' @param drop logical
#' @return inits
#' @export
#' @family inits
#' @keywords internal
`[.inits` <- function (x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  val
}
#' Coerce numeric to inits
#' 
#' Coerces numeric to inits
#' @param x numeric
#' @param fixed logical
#' @param comment character
#' @return inits
#' @keywords internal
#' @export
#' @family as.inits
as.inits.numeric <- function(x,fixed=FALSE,comment=character(0),...){
  stopifnot(is.logical(fixed),is.character(comment))
  fixed <- rep(fixed,length.out=length(x))
  y <- lapply(
    seq_along(x),
    function(i)as.init(x[[i]],fixed=fixed[[i]])
  )
  y <- as.inits(y)
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
#' @family init
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
#' @family init
#' @keywords internal
`$<-.init` <- function(x,name,value){
  if(!name %in% c('low','init','up'))stop('attempt to set an invalid init element')
  if(is.null(value))stop('attempt to delete a required init element')
  x[name] <- value
  x
}

#' Check if inits is fixed
#' 
#' Checks if inits is fixed.
#' @param x inits
#' @param ... ignored
#' @return logical
#' @describeIn fixed inits method
#' @export
#' @family fixed
fixed.inits <- function(x,...)sapply(x,fixed)

#' Set fixed attribute of inits
#' 
#' Sets fixed attribute of inits.
#' @param x inits
#' @param value logical
#' @return inits
#' @export
#' @family fixed
#' @keywords internal
`fixed<-.inits` <- function(x,value){
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
#' Coerce character to inits
#' 
#' Coerces character to inits.
#' @param x character
#' @param ... ignored
#' @return inits
#' @describeIn as.inits character method
#' @export
#' @family as.inits
as.inits.character <- function(x,...){
  # final empty line is scrubbed by collapse=\n
  # chomp all line endings, count and store final empties
  x <- sub('\\s+$','',x)
  y <- rev(x)
  z <- y == ''
  count = match(FALSE,z)
  padded <- count - 1
  x <- .parseBlock(x)
  block <- attr(x,'block')
  stopifnot(length(x)>0)
  comments <- .initcomments(x)
  data <- .initdata(x)
  data <- sub('SAME','1',data)
  data <- paste(data,collapse='\n')
  z <- strsplit(data,NULL)[[1]]
  #classify each character in z as belonging to a particular line and particular estimate.
  est <- .initEstimateNum(z)
  line <- .initLineNum(z)
  inits <- split(z,est)
  inits <- lapply(inits,paste,collapse='')
  # those comments on lines before the start of estimates
  # globalcom <- comments[unique(line[est==0])]
  globalcom <- comments[setdiff(line,line[est > 0])]
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
  out <- as.inits(inits,comment=globalcom)
  attr(out,'block') <- block
  attr(out,'padded') <- padded
  out
}
.parseBlock <- function(x){
  y <- c(x,'_terminus_')
  y <- paste(y,collapse='\n')
  block <- 0
  if(y %contains% '^ *BLOCK'){
    block <- text2decimal(y)
    y <- sub('BLOCK *\\([^)]+\\)','',y)
  }
  y <- strsplit(y,'\n')[[1]]
  y <- y[y != '_terminus_']
    attr(y,'block') <- block
  y
}
.initRelComment <- function(i,est,line){
  #need numbers for comment lines for any comment on my line, plus any trailing
  #comments, i.e. those after this line but before a new est is started
  #use comment groups: where runhead est coincides with runhead line
  #but if line starts with space, runhead est does not coincide with runhead line
  stopifnot(length(est)==length(line))
  myLines <- line[est == i]
  theirLines <- line[ est > i] # note: not est == i because leading space "belongs" to i - 1
  myLines <- setdiff(myLines, theirLines)
  # groupstart <- runhead(est) & runhead(line)
  # group <- cumsum(groupstart)
  # mygroup <- unique(group[est==i])#probably only 1
  # mylines <- unique(line[group==mygroup])
  myLines
}
.as.init.character <- function(x,comment=character(0),...){#limited utility
  stopifnot(length(x)==1)
  fixed = FALSE
  if(x %contains% 'FIX|FIXED') fixed <- TRUE
  x <- gsub('FIXED','',x)
  x <- gsub('FIX','',x)
  x <- gsub('\n','',x)
  x <- gsub('\t','',x)
  x <- sub('(','',x,fixed=TRUE)
  x <- sub(')','',x,fixed=TRUE)
  x <- gsub(' +',' ',x)
  x <- sub('^ ','',x)
  x <- sub(' $','',x)
  # x <- gsub(',? ',',',x) fails: "1,2 ,3" -> "1,2,,3"
  x <- gsub(' ,', ',', x) # @1.0.9
  x <- gsub(', ', ',', x) # @1.0.9
  x <- strsplit(x,',')[[1]]
  x <- as.numeric(x)
  x <- as.init(x,fixed=fixed,comment=comment,...)
  x
}
#' Coerce Inits to Inits
#' 
#' Coerces inits to inits
#' @param x inits
#' @param ... ignored
#' @return inits
#' @describeIn as.inits inits method
#' @export
#' @family as.inits
as.inits.inits <- function(x,...)x

#' Tweak Something
#' 
#' Tweaks something.
#' @param x object
#' @param ... dots
#' @export
#' @family tweak
#' @keywords internal
tweak <- function(x,...)UseMethod('tweak')

#' Tweak Init
#' 
#' Tweaks init.
#' @param x init
#' @param ... passed arguments
#' @param sd numeric
#' @param digits integer
#' @return init
#' @export
#' @keywords internal
#' @family tweak
tweak.init <- function(x,sd=0.13,digits=3,...){
	scale <- stats::rnorm(1,mean=1,sd=sd)
	y <- x$init * scale
	y <- signif(y,digits=digits)
	if(y < x$low | y > x$up) return(tweak.init(x,sd=sd,digits=digits,...))
	x$init <- y
	x
}
#' Tweak inits
#' 
#' Tweaks inits.
#' @param x inits
#' @param ... passed arguments
#' @param sd numeric
#' @param digits integer
#' @return inits
#' @export
#' @keywords internal
#' @family tweak
tweak.inits <- function(x,sd=0.13,digits=3,...){
	x[!fixed(x)] <- lapply(x[!fixed(x)],tweak,sd=sd,digits=digits,...)
	x
}
#' Tweak Model
#' 
#' Tweaks model.
#' @param x model
#' @param ... passed arguments
#' @param sd numeric
#' @param digits integer
#' @return model
#' @export
#' @family tweak
#' @examples
#' # Create a working project.
#' source <- system.file(package = 'nonmemica','project')
#' target <- tempdir()
#' target <- gsub('\\\\','/',target) # for windows
#' source
#' target
#' file.copy(source,target,recursive = TRUE)
#' project <- file.path(target,'project','model')
#' 
#' # Point project option at working project
#' options(project = project)
#' library(magrittr)
#' 
#' # Make ten new models with slightly different initial estimates.
#' 1001 %>% tweak
tweak.model <- function(x,sd=0.13,digits=3,...){
	stopifnot('theta' %in% names(x))
	# x$theta <- as.inits(x$theta)
  x[names(x) == 'theta'] <- lapply(x[names(x) == 'theta'], as.inits)
	# x$theta <- tweak(x$theta,sd=sd,digits=digits,...)
  x[names(x) == 'theta'] <- lapply(x[names(x) == 'theta'], tweak, sd = sd, digits = digits,...)
	x
}
	


