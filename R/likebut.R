globalVariables(c('symbol','run','feature','cov','ofv','label','unit'))
globalVariables(c('VARIABLE','META','VALUE','LABEL','VISIBLE','GUIDE'))
globalVariables(c('column','guide'))

#' Identify the Single Model Problem Statement
#' 
#' Identifies a single model problem statement.
#' 
#' @param x character
#' @param ... passed arguments
problem_ <- function(
  x,
  ...
){
  y <- read.model(modelfile(x,...))
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
#' Identifies the model problem statement for character (model name).
#' 
#' @inheritParams problem
#' @return character
#' @export
problem.character <- function(x,...)sapply(x,problem_, ...)

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
#' @return character
#' @import encode
#' @export
like.default <- function(x, ...){
  m <- data.frame(
    stringsAsFactors=F,
    run=x,
    value=problem(x,...)
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
#' @return character
#' @export
but.default <- function(x, ...){
  m <- data.frame(
    stringsAsFactors=F,
    run=x,
    value=problem(x,...)
  )
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
#' @return character
#' @export
depends.default <- function(x, ...){
  res <- lapply(x,dependsOne,...)
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
  p <- filter(p, symbol %in% c('min','cov','like','feature','ofv'))
  p <- tidyr::spread_ (p,'symbol',x)
  p <- mutate(p, run = x)
  p <- select(p, run,like,feature,min,cov,ofv)
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
#' @param digits significance for parameters
#' @param places rounding for objective function
#' @return data.frame
#' @export
runlog.character <- function(
  x, 
  dependencies=F,
  digits = 3,
  places = 0,
  ...
){
  mods <- sapply(x, modelfile, ...)
  x <- x[file.exists(mods)]
  stopifnot(length(x) > 0)
  if(dependencies) x <- depends(x)
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
  r <- lapply(x,safe,digits=digits,places=places,...)
  b <- do.call(bind_rows,r)
  b <- mutate(b, delta = as.numeric(ofv) - as.numeric(ofv)[match(like,run)])
  b
}

#' Tweak a Model
#' 
#' Tweaks a model by jittering initial estimates. Creates a new model directory
#' in project context and places the model there. Copies helper files as well.
#' Expects that x is a number. Assumes nested directory structure (run-specific directories).
#' @inheritParams tweak
#' @param project project directory
#' @param ext file extension for control streams
#' @param start a number to use as the first modelname
#' @param n the number of variants to generate (named start:n)
#' @param include regular expressions for files to copy to new directory
#' @param ... pass ext to over-ride default model file extension
#' @return character: vector of names for models created
#' @export
tweak.default <- function(
  x, 
  project = getOption('project', getwd()),
  ext = getOption('modex','ctl'),
  start=NULL, 
  n=10,
  include = '.def$',
  ...
){
  if(!getOption('nested',TRUE))stop('tweak assumes nested directory structure')
  stopifnot(length(x) == 1) # a run number
  path <- modelfile(x, project = project, ...)
  ctl <- read.model(path)
  ctl$problem <- paste0('   ',encode::encode(c('like','but'),c(x,'tweaked initials')))
  #set.seed(1)
  if(is.null(start)) start <- max(as.numeric(dir(project)),na.rm=TRUE) +1
  for(i in seq(from=start,length.out=n)) dir.create(file.path(project,i))
  for(i in seq(from=start,length.out=n)) write.model(tweak(ctl),file.path(project,i,paste(i,ext,sep='.')))
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
#' @param project project directory
#' @param nested model files nested in run-specific directories
#' @param overwrite whether to overwrite y if it exists
#' @param ext extension for the model file
#' @param include regular expressions for files to copy to new directory
#' @param ... passed arguments
#' @return the value of y
#' @export
 
likebut <- function(
  x,
  but='better',
  y=NULL,
  project = getOption('project', getwd() ),
  nested = getOption('nested', TRUE),
  overwrite=FALSE,
  ext = getOption('modex','ctl'),
  include = '\\.def$',
  ...
){
  if(is.null(y)){
    if(!nested){
      d <- dir(project, pattern = paste0('\\.',ext,'$'))
      d <- basename(d)
      d <- text2decimal(d)
    }else{
      d <- dir(project)
      suppressWarnings(d <- as.numeric(d))
    }
    d <- d[is.defined(d)]
    d <- max(as.numeric(d)) + 1
    d <- padded(d)
    y <- d
  }
  mod <- modelfile(y, project = project, nested = nested, ext = ext, ... )
  if(file.exists(mod))if(!overwrite)stop(mod,' already exists')
  srcdir <- modeldir(x, project = project, nested = nested, ...)
  target <- modeldir(y, project = project, nested = nested, ...)
  dir.create(target)
  for(p in include){
    pattern = if(nested) p else paste0(x,p)
    files  <- dir(srcdir,pattern = pattern)
    as     <- sub(x,y,files)
    file.copy(
      file.path(srcdir,files),
      file.path(target,as)
    )
  }
  c <- read.model(modelfile(x, project = project, nested = nested, ext = ext,...))
  like <- x
  c$problem <- encode::encode(c('like','but'),c(like,but),...)
  if(nchar(c$problem) > 58)warning('problem statement more than 40 chars')
  write.model(c,modelfile(y, project = project, nested = nested, ext = ext,...))
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



.parameters <- function(x,digits=3,places=0,...){
  stopifnot(length(x) == 1)
  p <- partab(x, verbose=F,digits=digits)
  # parfile <- paste(sep='/',getOption('project'),x,paste0(x,'.par'))
  if(!'symbol' %in% names(p))stop('symbol not defined in control stream nor *.def')
  need <- filter(p, is.na(symbol))$parameter
  if(length(need))warning('symbols undefined for ', paste(need, collapse=', '))
  p <- select(p, symbol,estimate)
  p$estimate <- as.character(p$estimate)
  p <- rename(p, value = estimate)
  min <- xpath(x, '//termination_status')
  if(length(min) == 0) min <- NA
  cov <- xpath(x, '//covariance_status/@error')
  if(length(cov) == 0) cov <- NA
  ofv <- round(digits=places,xpath(x, '//final_objective_function'))
  if(length(ofv) == 0) ofv <- NA
  dat <- datafile(x)
  if(length(datafile) == 0) datafile <- NA
  like <- like(x,...)
  but <- but(x,...)
  met <- data.frame(
    stringsAsFactors = F,
    symbol = c('min','cov','like','ofv','dat','feature'),
    value = c(min,cov,like,ofv,dat,but)
  )
  p <- bind_rows(p, met)
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
  m <- bind_rows(m)
  if(length(x) > 1) m <- filter(m, !symbol %in% c('dat','like','feature'))
  m <- mutate(m, symbol = factor(symbol,levels=unique(symbol)))
  m <- tidyr::spread(m, run,value)
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
#' @param xmlfile path to xml file
#' @param strip.namespace whether to strip e.g. nm: from xml elements for easier xpath syntax
#' @param digits passed to signif
#' @param ... dots
#' @return numeric
#' @seealso nms_canonical errors
#' @export
#' @import magrittr
#' @import dplyr
estimates.character <- function(
  x,
  xmlfile = modelpath(x, ext = 'xml',...),
  strip.namespace=TRUE,
  digits = 3,
  ...
){
  y <- as.xml_document(x, strip.namespace=strip.namespace,verbose=FALSE,file = xmlfile,...)
  theta   <- val_name(y, 'theta',  'theta','estimate')
# thetase <- y %>% val_name('thetase','theta','se')
  sigma   <- row_col(y,'sigma',   'sigma','estimate')
# sigmase <- y %>% row_col(y, 'sigmase', 'sigma','se')
  omega   <- row_col(y, 'omega',   'omega','estimate')
# omegase <- y %>% row_col('omegase', 'omega','se')
  # theta %<>% left_join(thetase,by='parameter')
  # omega %<>% left_join(omegase,by=c('parameter','offdiag'))
  # sigma %<>% left_join(sigmase,by=c('parameter','offdiag'))
  theta <- mutate(theta, offdiag = 0)
  param <- rbind(theta,omega,sigma)
  nms <- nms_canonical(x)
  res <- with(param, estimate[match(nms,parameter)])
  res <- signif(res, digits)
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
#' @param xmlfile path to xml file
#' @param strip.namespace whether to strip e.g. nm: from xml elements for easier xpath syntax
#' @param digits passed to signif
#' @param ... dots
#' @return numeric
#' @seealso nms_canonical errors
#' @export
#' @import magrittr
#' @import dplyr
errors.character <- function(
  x,
  xmlfile = modelpath(x, ext = 'xml', ...),
  strip.namespace=TRUE,
  digits = 3,
  ...
){
  y <- as.xml_document(x, strip.namespace=strip.namespace,verbose=FALSE,file=xmlfile,...)
  # theta   <- y %>% val_name('theta',  'theta','estimate')
  thetase <- val_name(y, 'thetase','theta','se')
  # sigma   <- y %>% row_col('sigma',   'sigma','estimate')
  sigmase <- row_col(y, 'sigmase', 'sigma','se')
  # omega   <- y %>% row_col('omega',   'omega','estimate')
  omegase <- row_col(y, 'omegase', 'omega','se')
  # theta %<>% left_join(thetase,by='parameter')
  # omega %<>% left_join(omegase,by=c('parameter','offdiag'))
  # sigma %<>% left_join(sigmase,by=c('parameter','offdiag'))
  thetase <- mutate(thetase, offdiag = 0)
  param <- rbind(thetase,omegase,sigmase)
  nms <-  nms_canonical(x)
  res <- with(param, se[match(nms,parameter)])
  res <- signif(res,digits)
  res
}

