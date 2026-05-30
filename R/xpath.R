#' Create an xml_document in a Project Context
#'
#' Creates an xml_document in a project context.
#' @param x object of dispatch
#' @param ... arguments to methods
#' @seealso \code{\link{xpath}}
#' @return xml_document
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% as.xml_document
#' @export
#' @family xpath
as.xml_document <- function(x,...)UseMethod('as.xml_document')

#' Coerce xml_document to xml_document
#' 
#' Coerces xml_document to xml_document
#' @param x xml_document
#' @param ... ignored
#' @return xml_document
#' @describeIn as.xml_document xml_document method
#' @export
#' @family xpath
as.xml_document.xml_document <- function(x,...)x

#' Coerce numeric to xml_document
#' 
#' Coerces numeric to xml_document
#' @param x numerc
#' @param ... passed arguments
#' @return xml_document
#' @keywords internal
#' @export
#' @family xpath
as.xml_document.numeric  <- function(x,...)as.xml_document(as.character(x,...),...)

#' Create xml_document From Character
#'
#' Creates an xml_document from character (modelname or filepath).
#' @import xml2
#' @param x file path or run name
#' @param strip.namespace whether to strip e.g. nm: from xml elements
#' @param ... passed to modelpath()
#' @return xml_document
#' @export
#' @family xpath
as.xml_document.character <- function(x,strip.namespace=TRUE, ...){
  if ( file_test("-f", x            )) return(readxml(x, ...))
  if ( file_test("-d", modelpath(x) )) x <- modelpath(x, "xml", ...)
  if (!file_test("-f", x            )) stop("could not find ", x)
  x <- readxml(x, strip.namespace = strip.namespace, ...)
  x
}

#' Read XML with Conditional Filtering
#' 
#' Reads XML with conditional filtering, i.e. selecting among multiple estimation steps.
#' 
#' @importFrom xml2 read_xml xml_find_all xml_remove
#' @param x file path
#' @param strip.namespace whether to strip e.g. nm: from xml elements
#' @param estimation integer indicating which estimation step to select, if multiple. Defaults to last.
#' @param ... passed arguments
#' @return xml_document
#' @export
#' @family xpath
readxml <- function(x, strip.namespace = TRUE, estimation = integer(0), ...){
  # at this point (e.g. called from as.xml_document.character)
  # x should be a file that exists
  stopifnot(is.character(x))
  stopifnot(length(x) == 1)
  stopifnot(file.exists(x))
  stopifnot(is.integer(estimation))
  stopifnot(length(estimation) <= 1)
  # we read the file
  x <- readLines(x)
  if(strip.namespace){
    x <- paste(x, collapse = " ")
    x <- gsub("<[a-zA-Z]+:", "<", x)
    x <- gsub("</[a-zA-Z]+:", "</", x)
    x <- gsub(" +[a-zA-Z]+:", " ", x)
  }
  x <- read_xml(x)
  x <- filter_estimation(x, estimation = estimation, ...)
  x
}


#' Filter XML for Relevant Estimation Step
#' 
#' Filters XML for relevant estimation step.  Defaults to last.
#' 
#' @importFrom xml2 read_xml xml_find_all xml_remove
#' @param x file path
#' @param estimation integer indicating which estimation step to select, if multiple. Defaults to last.
#' @param ... passed arguments
#' @return xml_document
#' @export
#' @family xpath
filter_estimation <- function(x, estimation = integer(0), ...){
  stopifnot(inherits(x, 'xml_document'))
  stopifnot(is.integer(estimation))
  stopifnot(length(estimation) <= 1)
  est <- xml_find_all(x, "//estimation")
  if (length(est) > 1){
    message('multiple estimation steps')
    options <- seq_len(est)
    if(!length(estimation)) estimation <- rev(options)[[1]]
    message('looking for estimation ', estimation)
    if(!(estimation %in% options)){
      warning('requested estimation not found')
    }else{
      for (i in options){
        if(i != estimation){
          xml_remove(est[[i]])
        }
      }
    }
  }
  x
}




#' Evaluate Xpath Expression
#'
#' Evaluates an xpath expression.
#' 
#' The resulting nodeset is scavenged for text, and coerced to best of numeric or character.
#' @param x xml_document
#' @param ... passed arguments
#' @export
#' @family xpath
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% xpath('//etashrink/row/col')
xpath <- function(x,...)UseMethod('xpath')

#' Evaluate xpath Expression in Default Context
#'
#' Coerces x to xml_document and evaluates.
#' @param x default
#' @param ... passed arguments
#' @return vector
#' @export
#' @family xpath
xpath.default <- function(x,...){
  # previous to 0.1.12, xpath.default called as.xml_document(x) without forwarding ..., 
  # so nested=FALSE never reached modelpath 
  # when xpath was called with a run name rather than an xml_document. 
  # This patch extracts named args from ... and forwards them.
  dots <- list(...)
  named <- dots[nzchar(names(dots))]
  doc <- do.call(as.xml_document, c(list(x), named))
  xpath(doc, ...)
}

#' Evaluate xpath Expression in Document Context
#'
#' Evaluates an xpath expression for a given document.
#' 
#' The resulting nodeset is scavenged for text, and coerced to best of numeric or character.
#' @import magrittr
#' @import xml2
#' @param x xml_document
#' @param ... ignored
#' @param xpath xpath expression to evaluate
#' @return vector
#' @export
#' @family xpath
xpath.xml_document <- function(x, xpath,...)as.best(xml_text(xml_find_all(x,xpath)))
  

