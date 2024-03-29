% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{as.model.character}
\alias{as.model.character}
\title{Coerce character to model}
\usage{
\method{as.model}{character}(
  x,
  pattern = "^\\\\s*\\\\$(\\\\S+)(\\\\s.*)?$",
  head = "\\\\1",
  tail = "\\\\2",
  parse = TRUE,
  ...
)
}
\arguments{
\item{x}{character}

\item{pattern}{pattern to identify record declarations}

\item{head}{subpattern to identify declaration type}

\item{tail}{subpattern remaining}

\item{parse}{whether to convert thetas omegas and sigmas to inits, tables to items, and runrecords to fields}

\item{...}{ignored}
}
\value{
list
}
\description{
Coerces chacter to model.
}
\examples{
library(magrittr)
options(project = system.file('project/model',package='nonmemica'))
1001 \%>\% as.model
}
\seealso{
Other as.model: 
\code{\link{[.model}()},
\code{\link{[[.model}()},
\code{\link{as.model.numeric}()},
\code{\link{as.model}()},
\code{\link{read.model}()},
\code{\link{write.model}()}
}
\concept{as.model}
