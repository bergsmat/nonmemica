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
as.xml_document <- function(x,...)UseMethod('as.xml_document')

#' Coerce xml_document to xml_document
#' 
#' Coerces xml_document to xml_document
#' @inheritParams as.xml_document
#' @return xml_document
#' @describeIn as.xml_document xml_document method
#' @export
as.xml_document.xml_document <- function(x,...)x

#' Coerce numeric to xml_document
#' 
#' Coerces numeric to xml_document
#' @inheritParams as.xml_document
#' @return xml_document
#' @describeIn as.xml_document numeric method
#' @export
as.xml_document.numeric  <- function(x,...)as.xml_document(as.character(x),...)

#' Create xml_document From Character
#'
#' Creates an xml_document from character (modelname or filepath).
#' @import xml2
#' @inheritParams as.xml_document
#' @param strip.namespace whether to strip e.g. nm: from xml elements
#' @return xml_document
#' @describeIn as.xml_document filepath method
#' @export
as.xml_document.character <- function(x,strip.namespace=TRUE,...){
  # x is a model name or a file path
  if(!file.exists(x)) x %<>% modelpath('xml',...)
  if(!strip.namespace)return(read_xml(x))
  x <- readLines(x)
  x <- paste(x,collapse=' ')
  x <- gsub('<[a-zA-Z]+:','<',x)
  x <- gsub('</[a-zA-Z]+:','</',x)
  x <- gsub(' +[a-zA-Z]+:',' ',x)
  read_xml(x)
}

#' Evaluate xpath Expression
#'
#' Evaluates an xpath expression.
#' 
#' The resulting nodeset is scavenged for text, and coerced to best of numeric or character.
#' @param x xml_document
#' @param ... passed arguments
#' @export
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% xpath('//etashrink/row/col')
xpath <- function(x,...)UseMethod('xpath')

#' Evaluate xpath Expression in Default Context
#'
#' Coerces x to xml_document and evaluates.
#' @inheritParams xpath
#' @return vector
#' @describeIn xpath default method
#' @export
xpath.default <- function(x,...)xpath(as.xml_document(x),...)

#' Evaluate xpath Expression in Document Context
#'
#' Evaluates an xpath expression for a given document.
#' 
#' The resulting nodeset is scavenged for text, and coerced to best of numeric or character.
#' @import magrittr
#' @import xml2
#' @inheritParams xpath
#' @param xpath xpath expression to evaluate
#' @return vector
#' @describeIn xpath xml_document method
#' @export
xpath.xml_document <- function(x, xpath,...){
  x %>%
    xml_find_all(xpath) %>%
    xml_text %>%
    as.best
}

