#' Create Specification for Model Inputs and Outputs
#'
#' Create a specification for the result of superset().  
#' @param x object
#' @param ... passed arguments
#' @seealso \code{\link{superspec.character}}
#' @export
#' @family superset
superspec <- function(x, ...)UseMethod('superspec')

#' Create Specification for Model Inputs and Outputs From Numeric
#'
#' Create a specification for the result of superset() from numeric by coercing to character.  
#' @param x numeric
#' @param ... passed arguments
#' @export
#' @family superset
superspec.numeric <- function(x, ...){
  y <- as.character(x)
  superspec(y, ...)
}

#' Create Specification for Model Inputs and Outputs From Character
#'
#' Create a specification for the result of superset() from character by treating as a model name. By default, gives a spec template for superset(x). Tries to supplement with labels and units from parent specification, if it exists.  Tries to supplement with any additional labels and units in definitions(x).  Defers to actual data if provided. Specify \code{exclusive}, \code{visible}, and \code{after} as for \code{superset}.
#' @param x character
#' @param include column names in output to consider adding
#' @param exclude column names in output to reject 
#' @param rename logical: whether to keep and rename columns with re-used names
#' @param visible a name for the flag column indicating visibility
#' @param after place new columns after this column; at end by default (NULL); TRUE places them after 
#' @param data an alternative dataset on which to model the specification
#' @param ... passed arguments
#' @export
#' @family superset
superspec.character <- function(
  x, 
  #exclusive = NULL,
  include = character(0),
  exclude = character(0),
  rename = NULL,
  visible = 'VISIBLE',
  after = NULL,
  data = NULL, 
  ...
){
  if(is.null(data)) data <- superset(
    x, 
    #exclusive = exclusive, 
    include = include,
    exclude = exclude,
    rename = rename,
    visible = visible, 
    after = after, 
    ...
  )
  ospec <- try(x %>% specfile %>% read.spec)
  def <- definitions(x)
  spec <- specification(data)
  if(ospec %>% inherits('spec')){
    spec$label <- ospec$label[match(spec$column, ospec$column)]
    spec$guide <- ospec$guide[match(spec$column, ospec$column)]
  }
  if('label' %in% names(def))spec$label <- ifelse(is.defined(spec$label), spec$label, def$label[match(spec$column, def$item)])
  if('unit' %in% names(def))spec$guide <- ifelse(is.defined(spec$guide), spec$guide, def$unit[match(spec$column, def$item)])
  if(visible %in% spec$column){
    spec$label[spec$column == visible] <- 'visibility of record during modeling'
    spec$guide[spec$column == visible] <- '//0/not visible//1/visible//'
  }
  spec
}

#' Move the Columns of a Data Frame Relative to Each Other
#' 
#' Move the columns of a data.frame relative to each other.
#' @param x data.frame
#' @param who a character vector of column names to move, or a logical vector of length names(x), or a vector of indices
#' @param after column after which to put who: may be character, integer, NA, or NULL
#' @param ... ignored
#' @export
#' @family superset
#' @return data.frame
#' 
shuffle <- function (x, who, after = NA, ...) 
{
  names(x) <- make.unique(names(x))
  who <- names(x[, who, drop = FALSE])
  nms <- names(x)[!names(x) %in% who]
  if (is.null(after)) 
    after <- length(nms)
  if (is.na(after)) 
    after <- 0
  if (length(after) == 0) 
    after <- length(nms)
  if (is.character(after)) 
    after <- match(after, nms, nomatch = 0)
  if (after < 0) 
    after <- length(nms)
  if (after > length(nms)) 
    after <- length(nms)
  nms <- append(nms, who, after = after)
  x[nms]
}

#' Calculate Number of Inputs
#' 
#' Calculates number of inputs.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family superset
ninput <- function(x, ...)UseMethod('ninput')
#' Calculate Number of Inputs for Numeric
#' 
#' Calculates number of inputs for numeric by coercing to character.
#' @param x numeric
#' @param ... passed arguments
#' @export
#' @family superset
ninput.numeric <- function(x,...){
  y <- as.character(x)
  ninput(y, ...)
}
#' Calculate Number of Inputs for Character
#' 
#' Calculates number of inputs for character by treating as a model name.
#' @param x character
#' @param ... passed arguments
#' @export
#' @family superset
#' @return integer
ninput.character <- function(x,...){
  y <- as.model(x, parse = FALSE)
  y <- y$input
  y <- sub(';.*','',y)
  y <- paste(y, collapse = ' ')
  y <- gsub('\\s*=\\s*','=',y)
  y <- strsplit(y,'[ ,]+')
  y <- y[[1]]
  y <- y[y != '']
  length(y)
}

#' Coerce to Superset
#' 
#' Coerces to superset.
#' 
#' Generic, with methods for numeric and character.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @family superset
#' @keywords internal
#' @seealso \code{\link{superset.numeric}} \code{\link{superset.character}}
superset <- function(x,...)UseMethod('superset')

#' Coerce to Superset from Numeric
#' 
#' Coerces to superset from numeric.
#' 
#' Converts number to character and revisits generic.
#' 
#' @inheritParams superset
#' @export
#' @family superset
#' @keywords internal
#' @seealso \code{\link{superset.character}}
superset.numeric <- function(x,...){
  y <- as.character(x)
  superset(y, ...)
}

#' Coerce to Superset from Character
#' 
#' Coerces to superset from character, treating \code{x} as a model name.
#' 
#' Given a model name, (\code{project} passed or set as global option) superset() figures out the run directory and location of a NONMEM control stream. It reads the control stream to identify the run-time location of input and output files, as well as the "ignore" (and/or "accept") criteria that relate extent of input records to extent of output records. `read.input` and `read.output` are lists consisting of functions and arguments appropriate for reading input and output file formats, respectively. The ignore criteria will be reconstructed per row so that output can be mapped unambiguously to input. A column named VISIBLE is bound to the input data, showing 1 where a record was visible to NONMEM, and 0 otherwise.

#' During integration, naming convention of the input is retained, and output column names are mapped by position, using the control stream input criteria. Output tables are restored to input dimensions using the "ignore" criteria, then checked for length: currently, superset ignores output tables having fewer rows than the input, as well as output tables whose row count is not a multiple of input row count.

#' Output tables may contain versions of input columns. Disposition depends on the values of \code{include}, \code{exclude}, and \code{rename}. If \code{include} has length, other columns are excluded.  Then, if \code{exclude} has length, these columns are excluded. Then, if \code{rename} is FALSE all remaining columns with re-used names will be dropped. If TRUE, such columns will be renamed (*.n, where n is table number). If NULL, only informative columns will be retained and renamed. A column is informative if any element is informative. An element is informative if it is newly generated (not NA and not zero, but original is NA) or if it is an alteration (non-NA, and different from non-NA original). If the column pair can be interpreted as numeric, "different" is determined using only the first \code{digits} digits.

#' Only the first instance of any column among successive output tables is retained.
 
#' In the control stream, avoid use of FIRSTONLY, as this alters the number of rows. 
 
# Tables created using FIRSTONLY can be summarized by superset if key is provided. Note that when key is provided, innocuous warnings result (e.g. 'nothing to merge') if items are tabled that are already present in the original data set.
#' 
#' @inheritParams superset
# @param project parent directory of model directories
#  @param key the object model for the input data: vector of key column names in hierarchical order (e.g. USUBJID, TIME, CMT)
#' @param read.input a methodology for acquiring the input
#' @param read.output a methodology for acquiring the output
#' @param include column names in output to consider adding
#' @param exclude column names in output to reject 
#' @param rename logical: whether to keep and rename columns with re-used names
#' @param digits significant digits for assessing informativeness when exclusive=NULL
#' @param visible a name for the flag column indicating visibility
#' @param after place new columns after this column; at end by default (NULL); TRUE places them after last model-visible column (see input statement) 
#' @param groups character vector of groupings within which any imputations will be performed
#' @param imputation a list of functions (or arguments to match.fun()) to perform imputations within cells defined by groups: e.g. generalize, forbak, etc (to be tried in succession for new columns only). 
#' @return superset: a data.frame  where row count is a multiple of (typically equal to) input row count.
#' @import utils
#' @export
#' @family superset
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(wrangle)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% superset %>% head
#' 1001 %>% superset %>% filter(VISIBLE == 1) %>% group_by(ID,TIME) %>% status
#' 1001 %>% ignored %>% table
superset.character <- function(
  x,
  read.input = list(read.csv,header=TRUE,as.is=TRUE), # na.strings = c("", "\\s", ".", "NA")
  read.output = list(read.table,header=TRUE,as.is=TRUE,skip=1,comment.char='',check.names=FALSE,na.strings = c("", "\\s", ".", "NA")),
  include = character(0),
  exclude = character(0),
  rename = NULL,
  digits = 5,
  visible = 'VISIBLE',
  after = NULL,
  groups = character(0),
  imputation = generalize,
  ...
){
  if(!is.list(imputation))if(length(imputation) == 1) imputation <- list(imputation)
  if(is.logical(after))if(after)after <- ninput(x)
  if(length(x) != 1)stop('character is understood as a model name and must have length one')
  #run <- x
  ctlfile <- modelfile(x,...)
  dropped <- ignored(x,read.input=read.input,...)
  control <- read.model(ctlfile,...)
  datafile <- datafile(x,...)
  if (!file.exists(datafile))stop(datafile, " not found ", call. = FALSE)
  outputdomain <- names(control) %contains% "tab"
  tables <- control[outputdomain]
  paths <- .tablePaths(tables,modeldir(x,...))
  labels <- .nminput(control)
  input <- .read.any(file=datafile,args=read.input)
  stopifnot(nrow(input)==length(dropped))
  if(!length(paths))return(input)
  if(length(labels) > ncol(input))stop('more nonmem aliases than data columns')
  input[x] <- as.integer(!dropped)
  output <- lapply(paths, .read.any, args=read.output)
  analogs <- names(input)[seq_along(labels)]
  output <- lapply(output,.revert,labels=labels,analogs=analogs)
  #Now all the tables have corresponding column names.
  expected <- nrow(input) - sum(dropped)
  #lapply(output,.agree,expected)
  #if(length(key)) return(.markup(lst=c(list(input),output),key=key))
  output <- .distill(output)#drop repeat columns
  output <- lapply(output,.restore,dropped=dropped)#expand
  output <- lapply(output, .select, include = include, exclude = exclude)
  res <- .superbind(
    c(list(input),output),
    rename = rename,
    digits = digits
  )
  rownames(res) <- NULL
  node <- match(x, names(res))
  new <- character(0)
  if(ncol(res) > node) new <- names(res)[(node+1):ncol(res)]
  if(length(groups))if(length(imputation)){
    res <- group_by(res, UQS(lapply(groups, as.symbol)))
    res <- mutate_at(res, .vars = new, .funs = do.call(funs, imputation))
    res <- ungroup(res)
  } 
  if(!is.null(after))res <- shuffle(res, who = new, after = after)
  if(length(visible) == 1) names(res)[names(res) == x] <- visible else res[[x]] <- NULL
  class(res) <- union('superset',class(res))
  res
}

#' Generalize a Nonmissing Value
#' 
#' #Generalize a nonmissing value.  If there is only one such among zero or more NA, impute that value for all NA.
#' @param x vector
#' @param ... ignored
#' @export
#' @family superset
generalize <- function(x,...){
  y <- unique(x[!is.na(x)])
  if(length(y) == 1) x[is.na(x)] <- y
  x
}

.select <- function(x, include = character(0), exclude = character(0), ...){ # limit to columns of interest
  nms <- names(x)
  if(length(include)) nms <- intersect(nms, include)
  nms <- setdiff(nms, exclude)
  x[,nms,drop = FALSE]
}

.restore <- function(x,dropped,...){ # add records wherever dropped is TRUE
  stopifnot(is.data.frame(x),is.logical(dropped))
  if(any(is.na(dropped)))stop('dropped must not contain NA')
  #if(sum(!dropped)!=nrow(x))warning('row count does not match sum of non-dropped')
  if(nrow(x) %% sum(!dropped)!=0)warning('row count not a multiple of non-dropped')
  scale <- nrow(x) %/% sum(!dropped)
  dropped <- rep(dropped,scale)
  index <- rep(NA,length(dropped))
  index[!dropped] <- seq_len(nrow(x))
  x <- x[index,,drop=FALSE]
}
.distill <- function(x,known=character(0),...){ # strike repeated columns within and among data frames
    stopifnot(is.list(x))
    if(!length(x))return(x)
    y <- x[[1]]
    stopifnot(is.data.frame(y))
    y <- y[unique(names(y))] # drop internal duplicates 
    y <- y[!names(y) %in% known] # drop external duplicates
    known <- c(known,names(y))
    c(list(y),.distill(x[-1],known=known,...)) # recursion
}
.informative <- function(x,y,digits=5){
      stopifnot(length(x)==length(y))
      # convert x and y to canonical form
      x <- as.best(x)
      y <- as.best(y)
      if(is.numeric(x))x <- signif(x,digits=digits)
      if(is.numeric(y))y <- signif(y,digits=digits)
      # detect informative elements of y
      generated <- is.na(x) & !is.na(y) & y!=0 #y:0 is probably just NONMEM substitute for NA
      degenerated <- !is.na(x) & is.na(y)
      altered <- !is.na(x) & !is.na(y) & x != y
      any(generated | altered)
}
.superbind <- function(
  lst,
  i=0,
  # exclusive=NULL,
  # include = character(0),
  # exclude = character(0),
  rename = NULL,
  digits=5,
  # node = 'visible',
  x=data.frame(),
  ...
){ # recursively cbind with auto-rename and exclusivity options
    if(!length(lst))return(x)
    stopifnot(is.list(lst),is.data.frame(lst[[1]]))
    stopifnot(is.null(rename) || is.logical(rename))
    # if(length(include)) include <- c(include,node)
    # if(length(exclude)) exclude <- setdiff(exclude,node)
    y <- lst[[1]]
    lst <- lst[-1]
    # if(length(include)) y <- y[,names(y) %in% include, drop = FALSE]
    # if(length(exclude)) y <- y[,!names(y) %in% exclude, drop = FALSE]
    # #if(is.character(exclusive)) y <- y[,names(y)[!names(y) %in% exclusive],drop=FALSE]
    if(nrow(x)==0) x <- data.frame(row.names=1:nrow(y))
    if(
    	(nrow(y) %% nrow(x) != 0) || (nrow(y) == 0)
    ){
      message('ignoring table ',i,': expected ', nrow(x),' rows but found ',nrow(y))
      y <- data.frame(row.names=1:nrow(x))
    }else{
      x <- x[rep(seq_len(nrow(x)),nrow(y) %/% nrow(x)),]
    }
    stopifnot(nrow(y)==nrow(x))
    analogs <- intersect(names(x),names(y))
    # implicitly, y cols with new names are informative.
    index <- sapply(analogs, function(col).informative(x[[col]],y[[col]],digits=digits))
    goodDups <- character(0)
    if(length(analogs))goodDups <- analogs[index]
    badDups <- setdiff(analogs,goodDups)
    # every analog y column will be renamed or dropped (or both).
    if(is.null(rename)) y <- y[,!names(y) %in% badDups,drop=FALSE]
    else if(rename == FALSE) y <- y[,!names(y) %in% analogs,drop=FALSE]
    fix <- names(y) %in% analogs
    if(any(fix)) names(y)[fix] <- map(names(y)[fix],from=analogs,to=paste0(analogs,'.',i))
    x <- cbind(x,y)
    rownames(x) <- NULL
    .superbind(
      lst=lst, 
      i=i + 1, 
      #exclusive=exclusive,
      # include = include,
      # exclude = exclude,
      rename = rename,
      digits=digits,
      x=x,
      ...
    )
}
map <- function (x, from, to, strict = TRUE, ...) 
{
  stopifnot(length(to) == length(from))
  res <- to[match(x, table = from)]
  if (!strict) 
    res[!(x %in% from)] <- x[!(x %in% from)]
  res
}
.read.any <- function(file,args){ # read a file according to a protocol (first arg is function ref.) 
    fun <- match.fun(args[[1]])
    args <- args[-1]
    args <- c(args,file=file)
    do.call(fun,args)
  }

#' Extract Index for Ignored Records
#' 
#' Extracts index for ignored records, given a NONMEM control stream.
#' 
#' @export
#' @family superset
#' @param x length-one character: a model name
#' @param read.input list of arguments representing a methodology for acquiring the data of interest: the first argument is not named and will be passed to match.fun(); the other arguments will be passed to the result, and must include an argument named 'header'.
#' @param ext model file extension, e.g. 'mod' or 'ctl'
#' @param project project directory (can be expression)
#' @param nested whether model files are nested in eponymous directories
#' @param ... passed to \code{\link{modelfile}} and \code{link{datafile}}
#' @return logical
#' 
#' 
ignored <- function(
  x,
  read.input=list(read.csv,header=TRUE,as.is=TRUE),
  ext = getOption("modex", "ctl"),
  project = getOption('project', getwd()),
  nested = getOption('nested', TRUE),
  ...
){
  project <- eval(project)
  stopifnot('header' %in% names(read.input))
  ctlfile <- modelfile(x, ext = ext, project = project, nested = nested, ...)
  control <- read.model(ctlfile)
  datafile <- datafile(x, ext = ext, project = project, nested = nested, ...)
  if (!file.exists(datafile))stop(datafile, " not found ", call. = FALSE)
  dropped <- .nmdropped(
  	data=.read.any(file=datafile,args=read.input),
  	lines=readLines(datafile),
  	test=.nmignore(control),
  	labels=.nminput(control)
  )
  return(dropped)
}
.revert <- function(x,labels,analogs){#change column names from their labels to their analogs
  fix <- names(x) %in% labels
  names(x)[fix] <- map(names(x)[fix],from=labels,to=analogs)
  x
}
.tablePaths <- function(tables,rundir)sapply(#try to extract paths to tables from control stream fragments
  seq_along(tables), 
  function(recnum)tryCatch(
    extfile(
      as.character(tables[[recnum]]),
      dir=rundir,
      extreg='FILE'
    ),
    error=function(e)warning('in table ',recnum,': ',e,call.=FALSE,immediate.=TRUE)
  )
)
#warn if nrow does not match expected
.agree <- function(x,expected)if(nrow(x)!=expected)warning('expected ',expected,' rows but found ',nrow(x))

filename <-
function (dir, run = NULL, ext = NULL) 
  file.path(dir, paste0(run, ext))

#lossless integration of NONMEM run inputs and outputs

.nmdropped <- function(data,lines,test,labels,...){#determine which records from data/lines are dropped, given test
  #data is the original data set as a data frame
  #lines is original data set, with any header, as character
  #test is a list corresponding to the INPUT options named ignore or accept
  #labels is the column names as described to NONMEM in the INPUT statement
  #lines may include header; we limit to no longer than data to discard header; 
  #it certainly would have been discarded (if present) by ignore logic.
  keep <- seq(length.out=nrow(data))
  lines <- rev(rev(lines)[keep])
  c1 <- substr(lines,1,1)
  cn <- sub(' *','',lines)
  cn[nzchar(cn)] <- substr(cn[nzchar(cn)],1,1)
  test <- lapply(test, .nmconditional, data=data, c1=c1, cn=cn,labels=labels,...)
  for(i in seq_along(test))if(names(test)[[i]]=='accept')test[[i]] <- !test[[i]] # convert accept to ignore
  .or(test)
}

.evalcond <- function(x,data,labels,...){#evaluate condition-type ignore criteria
    label <- x['label']
    op <- x['operator']
    val <- x['value']
    if(is.na(label))stop('no label')
    if(is.na(val))stop('no value')
    if(is.na(op))op <- 'EQ' # case should not occur; see .ignorecondition
    pos <- match(label,labels)
    vec <- data[,pos]
    if(any(is.na(vec))){
    	    warning('imputing NA as "."')
    	    vec[is.na(vec)] <- '.'
    }
    if(!op %in% c('EQ','NE')){ # EQ and NE compared as character (EQN and NEN compared as numeric)
      vec[vec == '.'] <- 0 # NONMEM help for ignore:  under this condition, values are coerced to numeric.  Presumably '.' treated as 0.
      val <- as.numeric(val)
      vec <- as.numeric(vec)
    }
    op <- map(
      op, 
      from=c('EQ','NE','GT','GE','LT','LE','EQN','NEN'),
      to=  c('==','!=', '>','>=','<' ,'<=','==' ,'!=' )
    )
    fun <- match.fun(op)
    fun(vec,val)
  }
.evalchar <- function(x,c1,cn,...){#evaluate character-type ignore criteria
    if(x=='@')return(cn %in% c(letters,LETTERS,'@'))
    else return(c1==x)
  }
.evalEither <- function(x,data,c1,cn,labels,...){#evaluate either conditional or character ignore criteria
    if(length(x)==1).evalchar(x['value'],data=data,c1=c1,cn=cn,...)
    else .evalcond(x,data=data,labels=labels,...)
  }
.nmconditional <- function(x,data,c1,cn,labels,...){#evaluate ignore criteria from NONMEM control stream
  result <- lapply(x,.evalEither,data=data,c1=c1,cn=cn,labels=labels,...)
  .or(result)
}
.or <- function(x,na.rm=FALSE,...){#multicolumn compounded "or"
  x <- as.matrix(as.data.frame(x,...),...)
  apply(x,MARGIN=1,any,na.rm=na.rm)
}
.and <- function(x,na.rm=FALSE,...){#multicolumn compounded "and"
  x <- as.matrix(as.data.frame(x,...),...)
  apply(x,MARGIN=1,all,na.rm=FALSE)
}

.nminput <- function(x,...)UseMethod('.nminput')
.nminput.default <- function(x,...){#assume x is a filepath for control stream
  if (!file.exists(x)) stop(x, " not found", call. = FALSE)
  control <- read.model(x)
  .nminput(control)
}
.nminput.model <- function(x,...){#extract input labels from control stream, with values like output tables but order like input table
  if(! 'input' %in% names(x))stop('no input record found in ',x,call.=FALSE)
  x <- x$input
  x <- x[x != '']
  x <- sub('^\\s+','',x)
  x <- sub(';.*','',x)
  x <- x[x != '']
  x <- paste(x,collapse=' ')
  x <- gsub(',',' ',x)
  x <- gsub(' +',' ',x)
  reserved=c(
    'ID','L1','L2','DV','MDV','RAW_','MRG_','RPT_',
    'TIME','DATE','DAT1','DAT2','DAT3','DROP','SKIP',
    'EVID','AMT','RATE','SS','II','ADDL','CMT','PCMT','CALL','CONT',
    'XVID1','XVID2','XVID3','XVID4','XVID5'
  )
  x <- gsub('DROP *= *','',x)
  x <- gsub('SKIP *= *','',x)
  x <- gsub(' *= *DROP','',x)
  x <- gsub(' *= *SKIP','',x)
  labels <- strsplit(x,' ')[[1]] # x is length one, so we keep just the first list
  labels <- lapply(labels,strsplit,'=')
 labels <- lapply(labels,unlist)
 labels <- sapply(
    labels,
    function(pair){
      if(length(pair)==1) return(pair[[1]])
      if(length(pair)>2)stop('too many synonyms')
      if(pair[[2]] %in% reserved) return(pair[[1]])#At least one must be reserved, per NONMEM help
      if(pair[[1]] %in% reserved) return(pair[[2]])#Prefer the first if both reserved (maybe does not occur if SKIP/DROP are removed).
      stop('unrecognized label syntax')
    }
  )
  #We now have a vector of labels as they will be used (if at all) in NONMEM output.
  #It is as long as the number of columns read from the input data set.
  labels
}

.nmignore <- function (x,...)UseMethod('.nmignore')
.nmignore.default <- function(x,...){#assume x is path to control stream
  if (!file.exists(x)) stop(x, " not found", call. = FALSE)
  control <- read.model(x)
  .nmignore(control)
}
.nmignore.model <- function(x,...){#extract ignore criteria from control stream
  opt <- .nmdataoptions(x,...) # named character
  if(!length(opt))opt  <- c(ignore="#") # the nonmem default
  test <- opt[names(opt) %in% c('ignore','accept')]
  #Only accept list or ignore list should occur, but not both. Ignore character may occur regardless.
  #logic <- TRUE
  #if('accept' %in% names(opt)) logic <- !logic
  #The next character is (, @, or some other character.
  #We need to reduce the lists to canonical sets of exclusions.
  #Exclusions are either character or conditional.
  test <- lapply(test,.ignorecanonical)
  test # a list
}
.ignorecanonical <- function(x,...){ # convert x (scalar character) to canonical form
  x  <- sub('^\\(','',x)
  x <- sub('\\)$','',x)
  #x <- strsplit(x,',',fixed=TRUE)[[1]] #prior to 5.24, did not strip bounding whitespace
  x <- strsplit(x,'[\n\t ]*,[\n\t ]*')[[1]] #only need the first, since x was scalar
  #now x is a chararcter vector of conditions (possibly scalar)
  #We now have character tests, or comparisons in the form 
  #label=value or label="value" or label='value' or label.op.value or label value 
  #x <- sub('=','.',x)
  x <- lapply(x,.ignorecondition)
  x
}

.ignorecondition <- function(x,...){ # convert conditional tests to canonical form
  # x is a single condition of the form value or label=value or label.op.value or possibly (version 5.52) label > value or label value
  #x <- gsub('\\s','',x) # strip all white space
  x <- sub('^\\s','',x) #strip leading white
  x <- sub('\\s$','',x) # strip trailing white
  # x is a single condition of the form value or label.value or label.op.value or possibly label>value or label=value with value possibly y.z
  # no leading or trailing space.  No space in label or value.  
  # collapse cases:  Fortran 95 supports == /= < > <= >=  nm720.pdf p. 9.
  x <- sub( '=','.EQ.',x) # actually, NONMEM case (not fortran 95)
  x <- sub('==','.EQ.',x)
  x <- sub('/=','.NE.',x)
  x <- sub('>=','.GE.',x)
  x <- sub('<=','.LE.',x)
  x <- sub('>' ,'.GT.',x)
  x <- sub('<' ,'.LT.',x)
  # No translations for .EQN. or .NEN. (NONMEM 73)
  # Now op, if present, is one of above.  We collapse all space around dots.
  x <- gsub(' *\\. *','.',x)
  # Now space, if present, occurs in a string without an operator, and therefore represents the equality operator.
  x <- sub(' ','.EQ.',x)
  # x is a single condition of the form value or label.op.value with value possibly y.z with y or z missing.
  # value does not contain space or comma.
  # Per NONMEM help for INPUT, a label must begin with a letter A-Z.
  # To distinguish between decimal dot and separator dot, we substitute all separator dot with comma
  x <- sub('\\.(EQ|NE|GE|LE|GT|LT|EQN|NEN)\\.',',\\1,',x)
  # Now all comma is element delimiter.
  x <- strsplit(x,',',fixed=TRUE)[[1]]
  stopifnot(length(x) %in% c(1,3))
  #label the list members
  if(length(x)==1)names(x) <- 'value'
  #if(length(x)==2)names(x) <- c('label','value')
  if(length(x)==3)names(x) <- c('label','operator','value')
  #remove quotes from values
  x['value'] <- sub('^"','',x['value'])
  x['value'] <- sub('"$','',x['value'])
  x['value'] <- sub("^'",'',x['value'])
  x['value'] <- sub("'$",'',x['value'])
  x
}
.nmdataoptions <- function(x,...)UseMethod('.nmdataoptions')
.nmdataoptions.default <- function(x,...){#assume x is path to control stream
  if (!file.exists(x)) stop(x, " not found", call. = FALSE)
  control <- read.model(x)
  .nmdataoptions(control)
}
.nmdataoptions.model <- function(x,...){# extract data options from control stream (especially IGNORE/ACCEPT)
  if (!"data" %in% names(x))stop("data record not found in control stream")
  rec <- x$data
  rec <- sub(';.*','',rec)
  rec <- paste(rec,collapse=' ') # now scalar
  rec <- gsub('[[:space:]]+',' ',rec)
  reserved <- c(
    'IGNORE','NULL','ACCEPT','NOWIDE','WIDE','CHECKOUT',
    'RECORDS','LREC','NOREWIND','REWIND','NOOPEN','LAST20',
    'TRANSLATE','BLANKOK'
  )
  for(word in reserved) rec <- gsub(word,paste0('$',word),rec,fixed=TRUE)
  splits <- strsplit(rec,'$',fixed=TRUE)[[1]] # rec is scalar; interested in the first (only) element
  names(splits) <- sapply(splits,function(split)tolower(sub('^([a-zA-Z0-9]+).*$','\\1',split)))
  splits <- sapply(splits,sub,pattern='^[a-zA-Z0-9]+ *=? *',replacement='')
  names(splits)[!names(splits) %in% tolower(reserved)] <- NA
  splits <- sub('^[[:space:]]+','',splits)
  splits <- sub('[[:space:]]+$','',splits)
  splits # a character vector
}
getdname <-function (x, ...) UseMethod("getdname")
getdname.default <- 
function (x, ...) {
  if (!file.exists(x)) 
    stop(x, " not found", call. = FALSE)
  control <- read.model(x)
  getdname(control)
}

getdname.model <- 
function (x, ...) 
{
  if (!"data" %in% names(x)) 
    stop("data record not found in control stream")
  x$data <- sub("^ +", "", x$data)
  x$data <- x$data[!x$data == ""]
  sub("^([^ ]+).*$", "\\1", x$data[[1]])
}

extfile <- 
function (ctlfile, dir, extreg, ...) 
{
  x <- scavenge(extreg, ctlfile)
  x <- extractPath(x)
  x <- resolve(x, dir)
  x
}
scavenge <- 
function (expr, lines) 
{
  x <- lines[grep(expr, lines, ignore.case = TRUE, perl = TRUE)]
  if (!length(x)) 
    stop("expression ", expr, " not found", call. = FALSE)
  x[[1]]
}
extractPath <-
function (x) 
  sub("(^.*(MSFO?|FILE) *= *)([^ ]*)(.*$)", "\\3", x, ignore.case = TRUE)

#' Retrieve Model Outputs with Metadata
#' 
#' Retrieves model outputs with metadata.
#' 
#' @param x model name
#' @param groups vector of key column names in superset, e.g. USUBJID, TIME
#' @param meta metadata with column 'item' and possibly attributes such as 'label' and 'guide'
#' @param subset length-one character: a condition for filtering results, e.g. 'EVID == 0'
#' @param ... passed arguments
#' @return data.frame
#' @importFrom tidyr gather
#' @importFrom tidyr gather_
#' @export
#' @family superset
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% metasuperset(c('ID','TIME')) %>% head
metasuperset <- function(
  x,
  groups, # = c('USUBJID','DATETIME'),
  meta = match.fun('meta')(x,...),
  subset = getOption('metasuperset_subset',NULL),
  ...
){
  stopifnot(length(x)==1)
  y <- superset(x,...)
  y <- as.best(y, '')
  #y <- filter(y, VISIBLE==1)
  if(!missing(subset)){
    if(!is.null(subset)){
    subset <- as.logical(eval(parse(text=subset), envir=y))
    y <- y[subset,,drop = FALSE] 
  }}
  targets <- intersect(names(y), meta$item)
  attrs <- setdiff(names(meta), 'item')
  for(col in targets){
    for(at in attrs){
      a <- meta[[at]][meta$item == col]
      if(!is.null(a))if(!is.na(a)) attr(y[[col]], at) <- a
    }
  }
  y
}

#' Get Metadata
#' 
#' Gets metadata.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @family superset
#' @keywords internal
meta <- function(x,...)UseMethod('meta')

#' Get Metadata for Numeric
#' 
#' Gets metadata for numeric by coercing to character.
#' 
#' @inheritParams meta
#' @export
#' @family superset
#' @keywords internal
meta.numeric <- function(x,...)meta(as.character(x),...)

#' Get Metadata for Character
#' 
#' Gets metadata for character, treating it as a model name. Blends metadata from specfile with metadata from control stream, removing both exact duplicates as well as redefined values (with warning).  
#' @inheritParams meta
#' @param simplify logical: remove range information from guide text
#' @export
#' @import spec
#' @return data.frame
#' @family superset
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% meta
meta.character <- function(x, simplify = TRUE, ...){
  y <- data.frame()
  z <- data.frame()
  try(y <- read.spec(specfile(x)))
  try(z <- definitions(x))
  e <- encoded(y)
  if(simplify)y$guide[!e] <- guidetext(y)[!e]
  y <- y[,c('column','label','guide'),drop = FALSE]
  names(y)[names(y) == 'column'] <- 'item'
  names(z)[names(z) == 'unit'] <- 'guide'
  res <- bind_rows(y,z)
  res <-  unique(res)
  if(any(duplicated(res$item))){
    warning('removing duplicate definitions')
    res <- res[!duplicated(res$item),,drop = FALSE]
  }
  # keys <- res[,c('VARIABLE','META')]
  # dups <- keys[duplicated(keys),]
  # dups <- unique(dups)
  # if(nrow(dups)){
  #   tags <- paste(dups$VARIABLE, dups$META, sep = '_')
  #   warning('removing conflicting metadata for ',paste(tags,collapse=', '))
  #   res <- res[!duplicated(keys),]
  # }
  res
}


#' Metaplot Numeric
#'
#' Plots numeric by coercing first to character.
#' @param x object
#' @param ... passed arguments
#' @import metaplot
#' @export
#' @family superset
#' @keywords internal
metaplot.numeric <- function(x, ...)metaplot(as.character(x),...)

#' Metaplot Character, Standard Evaluation
#'
#' Plots character by treating as model name.  A dataset
#' is constructed by combining the model input with a
#' the model output and calling metaplot with the result.
#'
#' @param x object
#' @param ... passed arguments
#' @param groups  columns by which to group the dataset
#' @param meta metadata; meta(x) by default
#' @param subset a condition for filtering data
#' @param var variables to plot
#' @import metaplot
#' @importFrom rlang UQS
#' @importFrom rlang syms
#' @family superset
metaplot_character <- function(
  x,
  groups,
  meta = NULL,
  subset,
  var,
  ...
){
  if(is.null(meta)) meta <- meta(x)
  z <- metasuperset(x,UQS(groups), meta = meta, subset = subset)
  #z %<>% pack
  metaplot(z, !!!var, ...)
}
#' Metaplot Character
#'
#' Plots character by treating as model name.  A dataset
#' is constructed by combining the meta version of the model input with a
#' meta version of the model output and calling metaplot with the result.
#'
#' @param x object
#' @param ... unquoted names of variables to plot, or other named arguments (passed)
#' @param groups  columns by which to group the dataset
#' @param meta metadata; meta(x) by default
#' @param subset a condition for filtering data
#' @import metaplot
#' @importFrom rlang quos
#' @import lazyeval
#' @export
#' @family superset
#' @examples
#' library(magrittr)
#' library(metaplot)
#' options(project = system.file('project/model',package='nonmemica'))
#' \dontrun{
#' 1001 %>% metaplot(
#'  CWRESI, TAD, SEX, 
#'  groups = c('ID','TIME'), 
#'  subset = 'MDV == 0',
#'  yref = 0, 
#'  ysmooth = TRUE
#' )
#' }
metaplot.character <- function(
  x,
  ...,
  groups,
  meta = match.fun('meta')(x),
  subset
){
  args <- quos(...)
  args <- lapply(args,f_rhs)
  var  <- args[names(args) == '']
  other<- args[names(args) != '']
  var <- sapply(var,as.character)
  val <- list(
    x = x,
    var = var,
    groups = groups,
    meta = meta
  )
  if(!missing(subset)) val <- c(val,list(subset = subset))
  val <- c(val, other)
  do.call(metaplot_character, val)
}

