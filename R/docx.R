#' Coerce to docx
#'
#' Coerces to docx.
#' @param x object of dispatch
#' @param ... arguments to methods
#' @param title passed to docx
#' @param template passed to docx
#' @param empty_template passed to docx
#' @param list.definition passed to docx
#' @seealso \code{link{as.docx.data.frame}}
#' @seealso \code{\link[ReporteRs]{docx}}
#' @seealso \code{\link[ReporteRs]{addFlexTable}}
#' @seealso \code{\link[ReporteRs]{FlexTable}}
#' @return docx
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='nonmemica'))
#' 1001 %>% partab %>% as.docx %>% as.file('1001.docx')
#' @export
as.docx <- function(x,...)UseMethod('as.docx')
#' Coerce docx to docx
#' 
#' Coerces docx to docx.
#' 
#' Returns the object.
#' @inheritParams as.docx
#' @return docx
#' @describeIn as.docx docx method
#' @export
as.docx.docx <- function(x,...)x

#' Coerce data.frame to docx
#' 
#' Coerces data.frame to docx
#' @inheritParams as.docx
#' @return docx
#' @describeIn as.docx data.frame method
#' @export
as.docx.data.frame <- function(
  x,
  title='untitled',
  template,
  empty_template= FALSE,
  list.definition = getOption("ReporteRs-list-definition"),
  ...
){
  requireNamespace('ReporteRs')
  doc <- ReporteRs::docx(
    title=title,
    template=template,
    empty_template = empty_template,
    list.definition = list.definition
  )
  doc <- ReporteRs::addFlexTable(doc, as.flextable(x,...),...)
  doc
}

#' Coerce partab to docx
#' 
#' Coerces partab to docx.
#' @inheritParams as.docx
#' @return docx
#' @describeIn as.docx partab method
#' @export
as.docx.partab <- function(
  x,
  title='parameter table',
  template,
  empty_template= FALSE,
  list.definition = getOption("ReporteRs-list-definition"),
  ...
){
  requireNamespace('ReporteRs')
  doc <- ReporteRs::docx(
    title=title,
    template=template,
    empty_template = empty_template,
    list.definition = list.definition
  )
  tab <- as.flextable(x,...)
  doc <- ReporteRs::addFlexTable(doc, tab,...)
  doc
}

#' Coerce to flextable
#' 
#' Coerces to flextable.
#' @param x object
#' @param ... passed to methods
#' @seealso \code{\link[ReporteRs]{docx}}
#' @seealso \code{\link[ReporteRs]{addFlexTable}}
#' @seealso \code{\link[ReporteRs]{FlexTable}}
#' @export
as.flextable <- function(x,...)UseMethod('as.flextable')
#' Coerce data.frame to flextable
#' 
#' Coerces data.frame to flextable
#' @inheritParams as.flextable
#' @seealso \code{\link[ReporteRs]{docx}}
#' @seealso \code{\link[ReporteRs]{addFlexTable}}
#' @seealso \code{\link[ReporteRs]{FlexTable}}
#' @return flextable
#' @describeIn as.flextable data.frame method
#' @export
as.flextable.data.frame <- function(x,...){
  requireNamespace('ReporteRs')
  dots <- list(...)
  nms <- names(dots)
  valid <- nms %in% c(
    'numrow','numcol','header.columns','add.rownames','body.cell.props',
    'body.par.props','body.text.props','header.cell.props','header.par.props',
    'header.text.props'
  )
  dots <- dots[valid]
  args <- c(
    list(data=x),
    dots
  )
  do.call(ReporteRs::FlexTable,args)
}
#' Coerce partab to flextable
#' 
#' Coerces partab to flextable.
#' 
#' Calls the data.frame method and adds remaining arguments. Pass zero-length values (e.g. NULL) to suppress.
#' @inheritParams as.flextable
#' @param body.par.props passed to FlexTable
#' @param header.par.props passed to FlexTable
#' @param inner.vertical passed to setFlexTableBorders
#' @param inner.horizontal passed to setFlexTableBorders
#' @param outer.vertical passed to setFlexTableBorders
#' @param outer.horizontal passed to setFlexTableBorders
#' @param footer passed to setFlexTableBorders
#' @seealso \code{\link[ReporteRs]{docx}}
#' @seealso \code{\link[ReporteRs]{addFlexTable}}
#' @seealso \code{\link[ReporteRs]{FlexTable}}
#' @return flextable
#' @describeIn as.flextable partab method
#' @export
as.flextable.partab <- function(
  x,
  body.par.props = ReporteRs::parProperties(padding = 5),
  header.par.props = ReporteRs::parProperties(padding = 5),
  inner.vertical = ReporteRs::borderProperties( style = "none" ), 
  inner.horizontal = ReporteRs::borderProperties( width = 1 ), 
  outer.vertical = ReporteRs::borderProperties( width = 0 ), 
  outer.horizontal = ReporteRs::borderProperties( width = 2 ),
  footer = TRUE,
  ...
){
  requireNamespace('ReporteRs')
    y <- as.flextable.data.frame(
    x,
    body.par.props = body.par.props,
    header.par.props = header.par.props,
    ...
  )
  y <- ReporteRs::setFlexTableBorders(
    y,
    inner.vertical = inner.vertical,
    inner.horizontal = inner.horizontal,
    outer.vertical = outer.vertical,
    outer.horizontal = outer.horizontal,
    footer=footer
  )
  y
}

#' Coerce to file
#' 
#' Coerces to file.
#' @param x object
#' @param ... passed to methods
#' @export
as.file <- function(x,...)UseMethod('as.file')

#' Coerce docx to File
#' 
#' Coerces docx to file.
#' @inheritParams as.file
#' @param file storage path for docx file
#' @seealso \code{\link[ReporteRs]{docx}}
#' @seealso \code{\link[ReporteRs]{writeDoc}}
#' @describeIn as.file docx method
#' @export
as.file.docx <- function(x,file,...){
  requireNamespace('ReporteRs')
  ReporteRs::writeDoc(doc=x,file=file,...)
  invisible(x)
}
