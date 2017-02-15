globalVariables(c('symbol','run','feature','cov','ofv'))
#' Identify the Single Model Problem Statement
#' 
#' Identifies a single model problem statement.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
.problem <- function(x,...)UseMethod('.problem')


#' Identify the Single Model Problem Statement from Character
#' 
#' Identifies a single model problem statement from character (i.e. model name).
#' 
#' @inheritParams  .problem
#' @param opt alternative specification of project directory
#' @param project direct specification of project directory
#' @param rundir model run directory
#' @param ctlext model file extension
#' @param ctlfile name of model file
#' @param ctlpath path to model file
#' @return character
#' @export
#' @keywords internal
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
#' @import encode
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
  c <- encode::codes(v,simplify=FALSE)
  d <- encode::decodes(v,simplify=FALSE)
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
  c <- encode::codes(v,simplify=FALSE)
  d <- encode::decodes(v,simplify=FALSE)
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
#' @export
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
  p %<>% tidyr::spread_ ('symbol',x)
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
#' @param digits significance for parameters
#' @param places rounding for objective function
#' @return data.frame
#' @export
runlog.character <- function(
  x, 
  dependencies=F,
  opt=getOption('project'),
  project = if(is.null(opt)) getwd() else opt,
  digits = 3,
  places = 0,
  ...
){
  dirs <- file.path(project,x)
  x <- x[file.exists(dirs)]
  stopifnot(length(x) > 0)
  if(dependencies) x %<>% depends
  dummy <- data.frame(
    stringsAsFactors = FALSE,
    run = character(0),
    like = character(0),
    feature = character(0),
    min = character(0),
    cov = character(0),
    ofv = character(0),
    delta = numeric(0)
  )
  safe <- function(x,...)tryCatch(
    .runlog(x,...),
    error=function(e){
      warning(
        'skipping ', x, 
        call.= FALSE, 
        immediate. = TRUE,
        noBreaks. = TRUE
      )
      return(dummy)
    }
  )
  r <- lapply(x,safe,project=project,digits=digits,places=places,...)
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
  path <- file.path(project,x,paste0(x,extension))
  ctl <- read.nmctl(path)
  ctl$problem <- paste0('   ',encode::encode(c('like','but'),c(x,'tweaked initials')))
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
#' @param extension extension for the model file
#' @param include regular expressions for files to copy to new directory
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
  extension = '.ctl',
  include = '.def',
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
  for(p in include){
    srcdir <- file.path(project,x)
    target <- file.path(project,y)
    files  <- dir(srcdir,pattern = p)
    as     <- sub(x,p,files)
    file.copy(
      file.path(srcdir,files),
      file.path(target,as)
    )
  }
  c <- read.nmctl(file.path(project,x,paste0(x,extension)))
  like <- x
  c$problem <- encode::encode(c('like','but'),c(like,but),...)
  if(nchar(c$problem) > 58)warning('problem statement more than 40 chars')
  write.nmctl(c, file.path(project,y,paste0(y,extension)))
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
  ...,
  opt=getOption('project'),
  project = if(is.null(opt))getwd() else opt
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

.parameters <- function(x,digits=3,places=0,...){
  stopifnot(length(x) == 1)
  p <- x %>% as.partab(verbose=F,digits=digits)
  # parfile <- paste(sep='/',getOption('project'),x,paste0(x,'.par'))
  if(!'symbol' %in% names(p))stop('symbol not defined in control stream nor *.def')
  need <- p %>% filter(symbol %>% is.na) %$% parameter
  if(length(need))warning('symbols undefined for ',need %>% paste(collapse=', '))
  p %<>% select(symbol,estimate)
  p$estimate <- as.character(p$estimate)
  p %<>% rename(value = estimate)
  min <- x %>% xpath('//termination_status')
  if(length(min) == 0) min <- NA
  cov <- x %>% xpath('//covariance_status/@error')
  if(length(cov) == 0) cov <- NA
  ofv <- x %>% xpath('//final_objective_function') %>% round(digits=places)
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
#' See \code{\link{estimates}} and \code{\link{errors}} for a more formal interface to model estimates and asymptotic standard errors.
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
  m %<>% tidyr::spread(run,value)
  m
}

######### ESTIMATES
#' Get Estimates
#' 
#' Gets estimates
#' @param x object
#' @param ... passed arguments
#' @export
estimates <- function(x,...)UseMethod('estimates')

#' Get Estimates for Numeric
#' 
#' Gets estimates for numeric by coercing to character.
#' @inheritParams estimates
#' @export
estimates.numeric <- function(x,...)estimates(as.character(x,...))

#' Get Estimates for Character.
#' 
#' Gets model parameter estimates in canonical order, treating character as model names. 
#' See \code{\link{parameters}} for a less formal interface.
#' 
#' @param x character (modelname)
#' @param project parent directory of model directories
#' @param opt alternative argument for setting project
#' @param rundir specific model directory
#' @param xmlfile path to xml file
#' @param ctlfile path to control stream
#' @param strip.namespace whether to strip e.g. nm: from xml elements for easier xpath syntax
#' @param digits passed to signif
#' @param ... dots
#' @return numeric
#' @seealso as.canonical errors
#' @export
#' @import magrittr
#' @import dplyr
estimates.character <- function(
  x,
  project = if(is.null(opt)) getwd() else opt, 
  opt = getOption('project'),
  rundir = file.path(project,x),
  xmlfile = file.path(rundir,paste0(x,'.xml')),
  ctlfile = file.path(rundir,paste0(x,'.ctl')),
  strip.namespace=TRUE,
  digits = 3,
  ...
){
  y <- x %>% as.xml_document(strip.namespace=strip.namespace,verbose=FALSE,project=project,file=xmlfile,...)
  theta   <- y %>% val_name('theta',  'theta','estimate')
# thetase <- y %>% val_name('thetase','theta','se')
  sigma   <- y %>% row_col('sigma',   'sigma','estimate')
# sigmase <- y %>% row_col('sigmase', 'sigma','se')
  omega   <- y %>% row_col('omega',   'omega','estimate')
# omegase <- y %>% row_col('omegase', 'omega','se')
  # theta %<>% left_join(thetase,by='parameter')
  # omega %<>% left_join(omegase,by=c('parameter','offdiag'))
  # sigma %<>% left_join(sigmase,by=c('parameter','offdiag'))
  theta %<>% mutate(offdiag = 0)
  param <- rbind(theta,omega,sigma)
  nms <- x %>% as.canonical
  res <- with(param, estimate[match(nms,parameter)])
  res %<>% signif(digits)
  res
}

######### STANDARD ERRORS
#' Get Errors
#' 
#' Gets errors.
#' @param x object
#' @param ... passed arguments
#' @export
errors <- function(x,...)UseMethod('errors')

#' Get Errors for Numeric
#' 
#' Gets errors for numeric by coercing to character.
#' @inheritParams errors
#' @export
errors.numeric <- function(x,...)errors(as.character(x,...))

#' Get Errors for Character.
#' 
#' Gets model asymptotic standard errors in canonical order, treating character as model names. 
#' See \code{\link{parameters}} for a less formal interface.
#' 
#' @param x character (modelname)
#' @param project parent directory of model directories
#' @param opt alternative argument for setting project
#' @param rundir specific model directory
#' @param xmlfile path to xml file
#' @param ctlfile path to control stream
#' @param strip.namespace whether to strip e.g. nm: from xml elements for easier xpath syntax
#' @param digits passed to signif
#' @param ... dots
#' @return numeric
#' @seealso as.canonical errors
#' @export
#' @import magrittr
#' @import dplyr
errors.character <- function(
  x,
  project = if(is.null(opt)) getwd() else opt, 
  opt = getOption('project'),
  rundir = file.path(project,x),
  xmlfile = file.path(rundir,paste0(x,'.xml')),
  ctlfile = file.path(rundir,paste0(x,'.ctl')),
  strip.namespace=TRUE,
  digits = 3,
  ...
){
  y <- x %>% as.xml_document(strip.namespace=strip.namespace,verbose=FALSE,project=project,file=xmlfile,...)
  # theta   <- y %>% val_name('theta',  'theta','estimate')
  thetase <- y %>% val_name('thetase','theta','se')
  # sigma   <- y %>% row_col('sigma',   'sigma','estimate')
  sigmase <- y %>% row_col('sigmase', 'sigma','se')
  # omega   <- y %>% row_col('omega',   'omega','estimate')
  omegase <- y %>% row_col('omegase', 'omega','se')
  # theta %<>% left_join(thetase,by='parameter')
  # omega %<>% left_join(omegase,by=c('parameter','offdiag'))
  # sigma %<>% left_join(sigmase,by=c('parameter','offdiag'))
  thetase %<>% mutate(offdiag = 0)
  param <- rbind(thetase,omegase,sigmase)
  nms <- x %>% as.canonical
  res <- with(param, se[match(nms,parameter)])
  res %<>% signif(digits)
  res
}

