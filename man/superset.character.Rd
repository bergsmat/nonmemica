% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/superset.R
\name{superset.character}
\alias{superset.character}
\title{Coerce to Superset from Character}
\usage{
\method{superset}{character}(
  x,
  read.input = list(read.csv, header = TRUE, as.is = TRUE),
  read.output = list(read.table, header = TRUE, as.is = TRUE, skip = 1, comment.char =
    "", check.names = FALSE, na.strings = c("", "\\\\s", ".", "NA")),
  include = character(0),
  exclude = character(0),
  rename = NULL,
  digits = 5,
  visible = "VISIBLE",
  after = NULL,
  groups = character(0),
  imputation = generalize,
  ...
)
}
\arguments{
\item{x}{object}

\item{read.input}{a methodology for acquiring the input}

\item{read.output}{a methodology for acquiring the output}

\item{include}{column names in output to consider adding}

\item{exclude}{column names in output to reject}

\item{rename}{logical: whether to keep and rename columns with re-used names}

\item{digits}{significant digits for assessing informativeness when exclusive=NULL}

\item{visible}{a name for the flag column indicating visibility}

\item{after}{place new columns after this column; at end by default (NULL); TRUE places them after last model-visible column (see input statement)}

\item{groups}{character vector of groupings within which any imputations will be performed}

\item{imputation}{a list of functions (or arguments to match.fun()) to perform imputations within cells defined by groups: e.g. generalize, forbak, etc (to be tried in succession for new columns only).}

\item{...}{passed arguments}
}
\value{
superset: a data.frame  where row count is a multiple of (typically equal to) input row count.
}
\description{
Coerces to superset from character, treating \code{x} as a model name.
}
\details{
Given a model name, (\code{project} passed or set as global option) superset() figures out the run directory and location of a NONMEM control stream. It reads the control stream to identify the run-time location of input and output files, as well as the "ignore" (and/or "accept") criteria that relate extent of input records to extent of output records. `read.input` and `read.output` are lists consisting of functions and arguments appropriate for reading input and output file formats, respectively. The ignore criteria will be reconstructed per row so that output can be mapped unambiguously to input. A column named VISIBLE is bound to the input data, showing 1 where a record was visible to NONMEM, and 0 otherwise.
During integration, naming convention of the input is retained, and output column names are mapped by position, using the control stream input criteria. Output tables are restored to input dimensions using the "ignore" criteria, then checked for length: currently, superset ignores output tables having fewer rows than the input, as well as output tables whose row count is not a multiple of input row count.
Output tables may contain versions of input columns. Disposition depends on the values of \code{include}, \code{exclude}, and \code{rename}. If \code{include} has length, other columns are excluded.  Then, if \code{exclude} has length, these columns are excluded. Then, if \code{rename} is FALSE all remaining columns with re-used names will be dropped. If TRUE, such columns will be renamed (*.n, where n is table number). If NULL, only informative columns will be retained and renamed. A column is informative if any element is informative. An element is informative if it is newly generated (not NA and not zero, but original is NA) or if it is an alteration (non-NA, and different from non-NA original). If the column pair can be interpreted as numeric, "different" is determined using only the first \code{digits} digits.
Only the first instance of any column among successive output tables is retained.
In the control stream, avoid use of FIRSTONLY, as this alters the number of rows.
}
\examples{
library(magrittr)
library(dplyr)
library(wrangle)
options(project = system.file('project/model',package='nonmemica'))
1001 \%>\% superset \%>\% head
1001 \%>\% superset \%>\% filter(VISIBLE == 1) \%>\% group_by(ID,TIME) \%>\% status
1001 \%>\% ignored \%>\% table
}
\seealso{
Other superset: 
\code{\link{generalize}()},
\code{\link{ignored}()},
\code{\link{meta.character}()},
\code{\link{meta.numeric}()},
\code{\link{metaplot.character}()},
\code{\link{metaplot.numeric}()},
\code{\link{metaplot_character}()},
\code{\link{metasuperset}()},
\code{\link{meta}()},
\code{\link{ninput.character}()},
\code{\link{ninput.numeric}()},
\code{\link{ninput}()},
\code{\link{shuffle}()},
\code{\link{superset.numeric}()},
\code{\link{superset}()},
\code{\link{superspec.character}()},
\code{\link{superspec.numeric}()},
\code{\link{superspec}()}
}
\concept{superset}
