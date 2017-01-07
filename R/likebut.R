.problem <- function(x,...)UseMethod('.problem')
.problem.character <- function(
  x,
  opt = getOption("project"), 
  project = if (is.null(opt)) getwd() else opt, 
  rundir = file.path(project, x),
  ctlext = '.ctl',
  ctlfile = paste0(x,ctlext),
  ctlpath = file.path(rundir, ctlfile),
  ...
){
  y <- read.nmctl(ctlpath)
  p <- y$prob
  p <- sub('^ +','',p)
  p <- sub(' +$','',p)
  p
}

#' Identify the Model Problem Statement
#' 
#' Identifies the model problem statement.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
problem <- function(x,...)UseMethod('problem')

#' Identify the Model Problem Statement for Numeric.
#' 
#' Identifies the model problem statement for numeric. Coerces to character.
#' 
#' @inheritParams problem
#' @export
problem.numeric <- function(x,...)problem(as.character(x))


#' Identify the Model Problem Statement for Character
#' 
#' Identifies the model problem statement for character. Treats x as a vector of modelnames.
#' 
#' @inheritParams problem
#' @param opt alternative specification of project directory
#' @param project direct specification of project directory
#' @param rundir model run directory
#' @param ctlext model file extension
#' @param ctlfile name of model file
#' @param ctlpath path to model file
#' @return character
#' @export
problem.character <- function(
  x,
  opt = getOption("project"), 
  project = if (is.null(opt)) getwd() else opt, 
  rundir = file.path(project, x),
  ctlext = '.ctl',
  ctlfile = paste0(x,ctlext),
  ctlpath = file.path(rundir, ctlfile),
  ...
)sapply(x,.problem,opt=opt,project=project,...)

#' Identify What Something is Like
#' 
#' Identifies what something is like.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
like <- function(x,...)UseMethod('like')

#' Identify the Relevant Reference Model
#' 
#' Identifies the relevant reference model, i.e. the 'parent' of x.
#' @inheritParams like
#' @param opt alternative specification of project directory
#' @param project direct specification of project directory
#' @return character
#' @export
like.default <- function(
  x,
  opt = getOption('project'),
  project = if(is.null(opt)) getwd() else opt,
  ...
){
  m <- data.frame(
    stringsAsFactors=F,
    run=x,
    value=problem(x,project=project,...)
  )
  v <- m$value[match(x,m$run)]
  c <- codes(v,simplify=FALSE)
  d <- decodes(v,simplify=FALSE)
  l <- sapply(c, function(i)match('like',i))
  s <- seq_along(l)
  f <- function(i){
    where = l[[i]]
    what = d[[i]]
    return(what[where])
  }
  ans <- sapply(s,f)
  ans
}

#' Identify a Distinctive Feature
#' 
#' Identifies a distinctive feature of an object.
#' @param x object
#' @param ... passed arguments
#' @export
but <- function(x,...)UseMethod('but')

#' Identify the Distinctive Feature of a Model
#' 
#' Identifies the distinctive feature of a model, i.e. how it differs from its parent (the reference model). x can be a vector.
#' @inheritParams but
#' @param opt alternative specification of project directory
#' @param project direct specification of project directory
#' @return character
#' @export
but.default <- function(
  x,
  opt=getOption('project'),
  project = if(is.null(opt))getwd() else opt,
  ...
){
  m <- data.frame(
    stringsAsFactors=F,
    run=x,
    value=problem(x,project=project,...)
  )
  #m %<>% filter(parameter=='prob')
  #stopifnot(all(m$run == x))
  v <- m$value[match(x,m$run)]
  c <- codes(v,simplify=FALSE)
  d <- decodes(v,simplify=FALSE)
  l <- sapply(c, function(i)match('but',i))
  s <- seq_along(l)
  f <- function(i){
    where = l[[i]]
    what = d[[i]]
    return(what[where])
  }
  ans <- sapply(s,f)
  ans
}

#' Identify What Something Depends On
#' 
#' Identfies that on which something depends.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
depends <- function(x,...)UseMethod('depends')

dependsOne <- function(x,...){
  stopifnot(length(x) == 1)
  cascade(x,...)
}
cascade <- function(x,...){
  prepend <- function(x,values,after=0)append(x=x,values=values,after=after)
  arg <- x[[1]]
  res <- like(arg,...)
  if(is.na(res))return(x)
  if(res == arg)return(x)
  x <- prepend(x,res)
  cascade(x,...)
}

#' Identify Model Dependencies
#' 
#' Identify those models in the lineage of models in x.
#' @inheritParams depends
#' @param opt alternative specification of project directory
#' @param project direct specification of project directory
#' @return character
depends.default <- function(
  x,
  opt=getOption('project'),
  project = if(is.null(opt))getwd() else opt,
  ...
){
  res <- lapply(x,dependsOne,project=project,append=append,...)
  unionRollUp(res)
}
unionRollUp <- function(x,...)UseMethod('unionRollUp') 
unionRollUp.list <- function(x,...){
  if(length(x)==1) return(x[[1]])
  if(length(x)==0) return(x)
  # x has at least two positions
  #x <- rev(x)
  x[[2]] <- union(x[[1]],x[[2]])
  x <- x[-1]
  #x <- rev(x)
  unionRollUp(x)
}
  
.runlog <- function(x,...){
  stopifnot(length(x) == 1)
  p <- parameters(x,...)
  p %<>% filter(symbol %in% c('min','cov','like','feature','ofv'))
  p %<>% spread_ ('symbol',x)
  p %<>% mutate(run = x)
  p %<>% select(run,like,feature,min,cov,ofv)
  p
}

#' Create a Runlog
#' 
#' Creates a runlog.
#' @param x object
#' @param ... passed arguments
#' @export
runlog <- function(x,...)UseMethod('runlog')

#' Create a Runlog for Numeric
#' 
#' Creates a runlog for numeric by coercing to character.
#' @inheritParams runlog
#' @export
runlog.numeric <- function(x,...)runlog(as.character(x),...)

#' Create a Runlog for Character
#' 
#' Creates a Runlog for character by treating x as modelname(s).  
#' @inheritParams runlog
#' @param dependencies whether to log runs in lineage(s) as well
#' @param opt alternative specification of project directory
#' @param project direct specification of project directory
#' @return data.frame
#' @export
runlog.character <- function(
  x, 
  dependencies=F,
  opt=getOption('project'),
  project = if(is.null(opt)) getwd() else opt,
  ...
){
  dirs <- file.path(project,x)
  x <- x[file.exists(dirs)]
  stopifnot(length(x) > 0)
  if(dependencies) x %<>% depends
  r <- lapply(x,.runlog,project=project,...)
  b <- do.call(bind_rows,r)
  b %<>% mutate(delta = as.numeric(ofv) - as.numeric(ofv)[match(like,run)])
  b
}

#' Tweak a Model
#' 
#' Tweaks a model by jittering initial estimates. Creates a new model directory
#' in project context and places the model there. Copies helper files as well.
#' Expects that x is a number.
#' @inheritParams partab::tweak
#' @param opt alternative specification of project directory
#' @param project direct specification of project directory
#' @param start a number to use as the first modelname
#' @param n the number of variants to generate (named start:n)
#' @param extension extension for the model file
#' @param include regular expressions for files to copy to new directory
#' @return character: vector of names for models created
#' @export
tweak.default <- function(
  x, 
  opt=getOption('project'),
  project = if(is.null(opt)) getwd() else opt,
  start=NULL, 
  n=10,
  extension='.ctl',
  include = '.def',
  ...
){
  stopifnot(length(x) == 1) # a run number
  path <- file.path(project,x,paste0(x,extenstion))
  ctl <- read.nmctl(path)
  ctl$problem <- paste0('   ',encode(c('like','but'),c(x,'tweaked initials')))
  #set.seed(1)
  if(is.null(start)) start <- dir(project) %>% as.numeric %>% max(na.rm=TRUE) %>% `+`(1)
  for(i in seq(from=start,length.out=n)) dir.create(file.path(project,i))
  for(i in seq(from=start,length.out=n)) ctl %>% tweak %>% write.nmctl(file.path(project,i,paste0(i,'.ctl')))
  out <- seq(from=start,length.out=n)
  for(i in out) 
    for(p in include){
      srcdir <- file.path(project,x)
      target <- file.path(project,i)
      files <- dir(srcdir,pattern = p)
      as <- sub(x,i,files)
      file.copy(
        file.path(srcdir,files),
        file.path(target,as)
      )
    }
  out
}

#' Modify a Model
#' 
#' Makes a copy of a model in a corresponding directory.  Problem statement is
#' updated to reflect that the model is LIKE the reference model BUT different
#' in some fundamental way. 
#' @param x a model name, presumably interpretable as numeric
#' @param but a short description of the characteristic difference from x
#' @param y optional name for model to be created
#' @param opt alternative specification of project directory
#' @param project direct specification of project directory
#' @param overwrite whether to overwrite y if it exists
#' @param ... passed arguments
#' @return the value of y
#' @export
 
likebut <- function(
  x,
  but='better',
  y=NULL,
  opt=getOption('project'),
  project = if(is.null(opt)) getwd() else opt,
  overwrite=FALSE,
  ...
){
  d <- dir(project)
  suppressWarnings(d %<>% as.numeric)
  d <- d[is.defined(d)]
  d <- d %<>% as.numeric %>% max %>% `+`(1)
  d <- padded(d)
  if(is.null(y)) y <- d
  if(file.exists(file.path(project,y)))if(!overwrite)stop(y,' already exists')
  dir.create(file.path(project,y))
  p <- file.path(project,x,paste0(x,'.par'))
  t <- file.path(project,x,paste0(x,'.tbl'))
  #t <- file.path(project,x,'tab.spec')
  if(!file.exists(p))stop('could not find ',p)
  if(!file.exists(t))stop('could not find ',t)
  file.copy(p,file.path(project,y,paste0(y,'.par')))
  file.copy(t,file.path(project,y,paste0(y,'.tbl')))
  #file.copy(w,file.path(project,y,paste0(y,'.wiki')))
  #file.copy(t,file.path(project,y,'tab.spec'))
  c <- read.nmctl(file.path(project,x,paste0(x,'.ctl')))
  like <- x
  c$problem <- encode(c('like','but'),c(like,but),...)
  if(nchar(c$problem) > 58)warning('problem statement more than 40 chars')
  write.nmctl(c, file.path(project,y,paste0(y,'.ctl')))
  y
}
padded <- function (x, width = 4, ...)sprintf(glue("%0", width, ".0f"), x)
relativizePath <- function(x,dir=getwd(),sep='/',...){
  stopifnot(length(x)==1)
  stopifnot(file.info(dir)$isdir)
  y <- normalizePath(x,winslash='/')
  z <- normalizePath(dir,winslash='/')
  y <- strsplit(y,sep)[[1]]
  z <- strsplit(z,sep)[[1]]
  count <- 0
  while(length(y) && length(z) && y[[1]] == z[[1]]){
    y <- y[-1]
    z <- z[-1]
  }
  z <- rep('..',length(z))
  y <- c(z,y)
  y <- do.call(file.path,as.list(y))
  y
}

#' Identify the Datafile for a Model
#' 
#' Identifies the datafile used by a model.
#' 
#' @param x the model name
#' @param opt alternative specification of project directory
#' @param project direct specification of project directory
#' @param ... passed arguments
#' @return character
#' @export
datafile <- function(
  x,
  opt=getOption('project'),
  project = if(is.null(opt))getwd() else opt,
  ...
){
  rundir <- file.path(project,x)
  ctlfile <- paste0(x,'.ctl')
  ctlpath <- file.path(rundir,ctlfile)
  control <- read.nmctl(ctlpath)
  dname <- getdname(control)
  datafile <- resolve(dname,rundir)
  datafile <- relativizePath(datafile)
  datafile
}

.parameters <- function(x,...){
  stopifnot(length(x) == 1)
  p <- x %>% as.partab(verbose=F,digits=3)
  parfile <- paste(sep='/',getOption('project'),x,paste0(x,'.par'))
  if(!'symbol' %in% names(p))stop('provide symbols for parameters in ',parfile)
  need <- p %>% filter(symbol %>% is.na) %$% parameter
  if(length(need))warning('in ',parfile,', symbols undefined for ',need %>% paste(collapse=', '))
  p %<>% select(symbol,estimate)
  p$estimate <- as.character(p$estimate)
  p %<>% rename(value = estimate)
  min <- x %>% xpath('//termination_status')
  if(length(min) == 0) min <- NA
  cov <- x %>% xpath('//covariance_status/@error')
  if(length(cov) == 0) cov <- NA
  ofv <- x %>% xpath('//final_objective_function') %>% round
  if(length(ofv) == 0) ofv <- NA
  dat <- x %>% datafile
  if(length(datafile) == 0) datafile <- NA
  like <- like(x,...)
  but <- but(x,...)
  met <- data.frame(
    stringsAsFactors = F,
    symbol = c('min','cov','like','ofv','dat','feature'),
    value = c(min,cov,like,ofv,dat,but)
  )
  p %<>% bind_rows(met)
  p
}

#' Get Parameters
#' 
#' Gets parameters.
#' @param x object
#' @param ... passed arguments
#' @export
parameters <- function(x,...)UseMethod('parameters')

#' Get Parameters for Numeric
#' 
#' Gets parameters for numeric by coercing to character.
#' @inheritParams parameters
#' @export
parameters.numeric <- function(x,...)parameters(as.character(x,...))

#' Get Parameters for Character.
#' 
#' Gets parameters, treating character as model names. If x is length one, 
#' slightly more details are returned such as datafile, reference model, and feature.
#' Otherwise results are bound together, one model per column.
#' @inheritParams parameters
#' @return data.frame
#' @export
parameters.character <- function(x,...){
  run <- function(x,...){
    y <- .parameters(x,...)
    y$run <- x
    y
  }
  m <- lapply(x,run,...)
  m %<>% bind_rows
  if(length(x) > 1) m %<>% filter(!symbol %in% c('dat','like','feature'))
  m %<>% mutate(symbol = factor(symbol,levels=unique(symbol)))
  m %<>% spread(run,value)
  m
}

#' Coerce to Pharmval
#' 
#' Coerces to pharmval
#' @param x object
#' @param ... passed arguments
#' @export
as.pharmval <- function(x,...)UseMethod('as.pharmval')

#' Coerce Numeric to Pharmval
#' 
#' Coerces numeric to pharmval by coercing first to character.
#' @inheritParams as.pharmval
#' @export
as.pharmval.numeric <- function(x,...)as.pharmval(as.character(x),...)


#' Coerce Character to Pharmval
#' 
#' Coerces to pharmval by treating character as modelname.  Combines the metasuperset
#' (model ouptut in meta format) with the meta version of the model data.file.
#' @inheritParams as.pharmval
#' @param pvlfile if not supplied, guessed to be model's datafile except with 
#' the value of ext in place of 'csv'.
#' @param ext default extention for pharmval version of model datafile.
#' @return meta data.frame 
#' @seealso metasuperset
#' @export
as.pharmval.character <- function(
  x,
  ...,
  pvlfile,
  ext = 'pvl'
){
  d <- x %>% datafile(...)
  if(missing(pvlfile)) pvlfile <- d %>% sub('csv$',ext,.)
  p <- as.meta(pvlfile)
  y <- x %>% metasuperset
  p %<>% bind_rows(y)
  p %<>% unique
  p
}

#' Plot Numeric in Project Context
#' 
#' Plots numeric in project context by coercing first to character.
#' @param x object
#' @param y passed to plot
#' @param ... passed arguments
#' @export
plot.numeric <- function(x, y, ...)plot(as.character(x),y=y,...)

#' Plot Character in Project Context
#' 
#' Plots in project context by treating character as modelname.  A dataset
#' is constructed by combining the meta version of the model input with a
#' meta version of the model output and calling plot with the result.
#' 
#' @param x object
#' @param y passed to plot
#' @param ... passed arguments
#' @seealso as.pharmval
#' @export
#' 
plot.character <- function(x, y,...){ 
  z <- x %>% as.pharmval
  plot(z,y,...)
}

#' Retrieve Model Outputs in Meta Format
#' 
#' Retrieves model outputs in meta format. Inputs should have EVID.
#' 
#' @param x model name
#' @param opt alternative specification of project directory
#' @param project direct specification of project directory
#' @param group_by vector of key column names in superset
#' @param meta data.frame with item, symbol, unit, label
#' @param ... passed arguments
#' @return meta
#' @export
metasuperset <- function(
  x,
  opt = getOption('project'),
  project = if(is.null(opt))getwd() else opt,
  group_by = c('USUBJID','DATETIME'),
  meta = as.definitions(x,...),
  ...
){
  stopifnot(length(x)==1)
  y <- x %>% as.superset
  y %<>% filter(VISIBLE==1)
  y %<>% filter(EVID==0)
  need <- c('item','symbol','label','guide')
  miss <- setdiff(need, names(meta))
  if(length(miss))stop('meta is missing columns ', paste(miss,collapse=', '))
  if(!file.exists(filepath))stop('could not find ',filepath)
  meta %<>% 
    select(symbol,label,unit) %>%
    rename(VARIABLE=symbol,LABEL=label,GUIDE=unit) %>%
    gather(META,VALUE,LABEL,GUIDE)
  targets <- intersect(meta$VARIABLE,names(y))
  meta %>% filter(VARIABLE %in% targets)
  y %<>% select_(.dots = c(group_by,targets))
  y %<>% group_by_(.dots=group_by) %>% fold
  y %<>% bind_rows(meta)
  y %<>% as.meta
  y
}

