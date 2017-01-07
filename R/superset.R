#' Coerce to superset
#' 
#' Coerces to superset.
#' 
#' Generic, with methods for numeric and character.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
as.superset <- function(x,...)UseMethod('as.superset')

#' Coerce to superset from numeric
#' 
#' Coerces to superset from numeric.
#' 
#' Converts number to character and revisits generic.
#' 
#' @inheritParams as.superset
#' @export
as.superset.numeric <- function(x,...){
  y <- as.character(x)
  as.superset(y)
}

#' Coerce to superset from character
#' 
#' Coerces to superset from character.
#' 
#' Treats character as a modelname.
#' 
#' Given a model run run name (x) and project directory, `superset` figures out the run directory and location of a NONMEM control stream. It reads the control stream to identify the run-time location of input and output files, as well as the "ignore" (and/or "accept") criteria that relate extent of input records to extent of output records. `read.input` and `read.output` are lists consisting of functions and arguments appropriate for reading input and output file formats, respectively. The ignore criteria will be reconstructed per row so that output can be mapped unambiguously to input. A column named VISIBLE is bound to the input data, showing 1 where a record was visible to NONMEM, and 0 otherwise.

# Normally, `superset` tries to bind output columns directly to input. Alternatively, if key is provided, it is used as an object model to allow an inferential left join of output onto input; this approach is riskier, but can back fill NA cells with values that are otherwise constant within left-subsets of the key.

#' During integration, naming convention of the input is retained, and output column names are mapped by position, using the control stream input criteria. Output tables are restored to input dimensions using the "ignore" criteria, then checked for length: currently, superset ignores output tables having fewer rows than the input, as well as output tables whose row count is not a multiple of input row count.

#' Output tables may contain versions of input columns. Disposition depends on the value of `exclusive`. If a character vector, it lists columns to exclude from output. If TRUE, all columns with re-used names will be dropped. If FALSE, such columns will be renamed (*.n, where n is table number). If NULL, only informative columns will be retained and renamed. A column is informative if any element is informative. An element is informative if it is newly generated (not NA and not zero, but original is NA) or if it is an alteration (non-NA, and different from non-NA original). If the column pair can be interpreted as numeric, "different" is determined using only the first `digits` digits.

#' Only the first instance of any column among successive output tables is retained.
 
#' In the control stream, avoid use of FIRSTONLY, as this alters the number of rows. 
 
# Tables created using FIRSTONLY can be summarized by superset if key is provided. Note that when key is provided, innocuous warnings result (e.g. 'nothing to merge') if items are tabled that are already present in the original data set.
#' 
#' @inheritParams as.superset
#' @param project parent directory of model directories
#' @param opt alternative argument for setting project
#' @param rundir specific model directory
#' @param ctlfile path to control stream
#  @param key the object model for the input data: vector of key column names in hierarchical order (e.g. USUBJID, TIME, CMT)
#' @param read.input a methodology for acquiring the input
#' @param read.output a methodology for acquiring the output
#' @param exclusive character vector of output column names to exclude; or logical: whether to keep columns with re-used names
#' @param digits significant digits for assessing informativeness when exclusive=NULL
#' @param visible a name for the flag column indicating visibility
#' @return superset: a data.frame  where row count is a multiple of (typically equal to) input row count.
#' @export

as.superset.character <- function(
  x,
  project = if (is.null(opt)) getwd() else opt, 
  opt = getOption("project"),
  rundir = filename(project,run), 
  ctlfile = filename(rundir, run, ".ctl"),
# key=character(0),
  read.input=list(read.csv,header=TRUE,as.is=TRUE),
  read.output=list(read.table,header=TRUE,as.is=TRUE,skip=1,comment.char='',check.names=FALSE),
  exclusive=NULL,
  digits=5,
  visible='VISIBLE',
  ...
){
  if(length(x) != 1)stop('character is understood as a model name and must have length one')
  run <- x
  #stopifnot('header' %in% names(read.input))
  if(!missing(run))run <- as.character(run)
  if(!missing(rundir))rundir <- as.character(rundir)
  if(missing(run) & missing(rundir) & missing(ctlfile))stop('one of run, rundir, or ctlfile must be supplied')
  if(missing(run) & missing(ctlfile)) run <- basename(rundir)  	  
  if(missing(run) & missing(rundir))run <- sub('[.][^.]+$','',basename(ctlfile))
  if(missing(project) & !missing(rundir))project <- dirname(rundir)
  if(missing(project) & missing(rundir) & !missing(ctlfile))project <- dirname(dirname(ctlfile))
  dropped <- ignored(run=run,project=project,rundir=rundir,ctlfile=ctlfile,read.input=read.input,...)
  control <- read.nmctl(ctlfile)
  dname <- getdname(control)
  datafile <- resolve(dname,rundir)
  if (!file.exists(datafile))stop(dname, " not visible from ", rundir, call. = FALSE)
  outputdomain <- names(control) %contains% "tab"
  tables <- control[outputdomain]
  paths <- .tablePaths(tables,rundir)
  labels <- .nminput(control)
  input <- .read.any(file=datafile,args=read.input)
  stopifnot(nrow(input)==length(dropped))
  input[as.character(run)] <- as.integer(!dropped)
  if(!length(paths))return(input)
  if(length(labels)>ncol(input))stop('more nonmem aliases than data columns')
  output <- lapply(paths, .read.any, args=read.output)
  analogs <- names(input)[seq_along(labels)]
  output <- lapply(output,.revert,labels=labels,analogs=analogs)
  #Now all the tables have corresponding column names.
  expected <- nrow(input) - sum(dropped)
  #lapply(output,.agree,expected)
  #if(length(key)) return(.markup(lst=c(list(input),output),key=key))
  output <- .distill(output)#drop repeat columns
  output <- lapply(output,.restore,dropped=dropped)#expand
  res <- .superbind(c(list(input),output),exclusive=exclusive,digits=digits)
  rownames(res) <- NULL
  if(length(visible) == 1) names(res)[names(res) == run] <- visible
  class(res) <- union('superset',class(res))
  res
}

.restore <- function(x,dropped,...){ # add records wherever dropped is TRUE
  stopifnot(is.data.frame(x),is.logical(dropped))
  if(any(is.na(dropped)))stop('dropped must not contain NA')
  #if(sum(!dropped)!=nrow(x))warning('row count does not match sum of non-dropped')
  if(nrow(x)%%sum(!dropped)!=0)warning('row count not a multiple of non-dropped')
  scale <- nrow(x)%/%sum(!dropped)
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
# .markup <- function(lst,key,...){ # recursively amend using raised.keyed
#     stopifnot(is.list(lst),!!length(lst),is.data.frame(lst[[1]]))
#     x <- lst[[1]]
#     lst <- lst[-1]
#     if(!length(lst))return(x)
#     y <- lst[[1]]
#     lst <- lst[-1]
#     ind <- key[key %in% names(y)]
#     y <- as.keyed(y,ind)
#     x$metrumrg.markup <- 1:nrow(x)
#     x <- as.keyed(x,'metrumrg.markup')
#     z <- x^y # the magic
#     if(any(is.na(z$metrumrg.markup)))stop('pseudo rows introduced')
#     z <- sort(as.keyed(z,'metrumrg.markup'))
#     z$metrumrg.markup <- NULL
#     z <- as.data.frame(z,...)
#     .markup(c(list(z),lst),key=key,...)
# }
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
.superbind <- function(lst,i=0,exclusive=NULL,digits=5,x=data.frame(),...){ # recursively cbind with auto-rename and exclusivity options
    if(!length(lst))return(x)
    stopifnot(is.list(lst),is.data.frame(lst[[1]]))
    y <- lst[[1]]
    lst <- lst[-1]
    if(is.character(exclusive)) y <- y[,names(y)[!names(y) %in% exclusive],drop=FALSE]
    if(nrow(x)==0) x <- data.frame(row.names=1:nrow(y))
    if(
    	(nrow(y) %% nrow(x) != 0) ||
	(nrow(y) == 0)
    ){
      message('ignoring table ',i,': expected ', nrow(x),' rows but found ',nrow(y))
      y <- data.frame(row.names=1:nrow(x))
    }else{
      x <- x[rep(seq_len(nrow(x)),nrow(y)%/% nrow(x)),]
    }
    stopifnot(nrow(y)==nrow(x))
    analogs <- intersect(names(x),names(y))
    #(implicitly, y cols with new names are informative.)
    index <- sapply(analogs, function(col).informative(x[[col]],y[[col]],digits=digits))
    goodDups <- character(0)
    if(length(analogs))goodDups <- analogs[index]
    badDups <- setdiff(analogs,goodDups)
    #every analog y column will be renamed or dropped (or both).
    if(is.null(exclusive)) y <- y[,!names(y) %in% badDups,drop=FALSE]
    else if(is.logical(exclusive))if(exclusive==TRUE) y <- y[,!names(y) %in% analogs,drop=FALSE]
    fix <- names(y) %in% analogs
    if(any(fix))names(y)[fix] <- map(names(y)[fix],from=analogs,to=paste0(analogs,'.',i))
    x <- cbind(x,y)
    rownames(x) <- NULL
    .superbind(lst=lst, i=i+1, exclusive=exclusive,digits=digits,x=x,...)
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
# given a nonmem control stream, give an index to dropped input rows.
ignored <- function(
  run,
  project=getwd(),
  rundir = filename(project,run), 
  ctlfile = filename(rundir, run, ".ctl"),
  read.input=list(read.csv,header=TRUE,as.is=TRUE),
  ...
){
  stopifnot('header' %in% names(read.input))
  if(!missing(run))run <- as.character(run)
  if(!missing(rundir))rundir <- as.character(rundir)
  if(missing(run) & missing(rundir) & missing(ctlfile))stop('one of run, rundir, or ctlfile must be supplied')
  if(missing(run) & missing(ctlfile)) run <- basename(rundir)  	  
  if(missing(run) & missing(rundir))run <- sub('[.][^.]+$','',basename(ctlfile))
  if(missing(project) & !missing(rundir))project <- dirname(rundir)
  if(missing(project) & missing(rundir) & !missing(ctlfile))project <- dirname(dirname(ctlfile))
  if(normalizePath(rundir)!=normalizePath(dirname(ctlfile)))warning('rundir does not specify parent of ctlfile')
  control <- read.nmctl(ctlfile)
  dname <- getdname(control)
  datafile <- resolve(dname,rundir)
  if (!file.exists(datafile))stop(dname, " not visible from ", rundir, call. = FALSE)
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
      tables[[recnum]],
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
  control <- read.nmctl(x)
  .nminput(control)
}
.nminput.nmctl <- function(x,...){#extract input labels from control stream, with values like output tables but order like input table
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
  control <- read.nmctl(x)
  .nmignore(control)
}
.nmignore.nmctl <- function(x,...){#extract ignore criteria from control stream
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
  control <- read.nmctl(x)
  .nmdataoptions(control)
}
.nmdataoptions.nmctl <- function(x,...){# extract data options from control stream (especially IGNORE/ACCEPT)
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
  control <- read.nmctl(x)
  getdname(control)
}
runhead <-
function (x) 
{
  n <- x != prev(x)
  if (length(n)) 
    n[[1]] <- TRUE
  n
}
prev <-
function (x) 
{
  s <- seq_along(x)
  s <- c(length(s), s[-length(s)])
  x <- x[s]
  if (length(x)) 
    x[[1]] <- NA
  x
}
getdname.nmctl <- 
function (x, ...) 
{
  if (!"data" %in% names(x)) 
    stop("data record not found in control stream")
  x$data <- sub("^ +", "", x$data)
  x$data <- x$data[!x$data == ""]
  sub("^([^ ]+).*$", "\\1", x$data[[1]])
}
absolute <- function(x)grepl('^/',x) | grepl('^.:',x)
resolve <-
function (file, dir) 
  ifelse(
    absolute(file),
    file, 
    file.path(dir, file)
  )

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