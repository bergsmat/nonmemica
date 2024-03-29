% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/superset.R
\name{metaplot.character}
\alias{metaplot.character}
\title{Metaplot Character}
\usage{
\method{metaplot}{character}(x, ..., groups, meta = match.fun("meta")(x), subset)
}
\arguments{
\item{x}{object}

\item{...}{unquoted names of variables to plot, or other named arguments (passed)}

\item{groups}{columns by which to group the dataset}

\item{meta}{metadata; meta(x) by default}

\item{subset}{a condition for filtering data}
}
\description{
Plots character by treating as model name.  A dataset
is constructed by combining the meta version of the model input with a
meta version of the model output and calling metaplot with the result.
}
\examples{
library(magrittr)
library(metaplot)
options(project = system.file('project/model',package='nonmemica'))
\dontrun{
1001 \%>\% metaplot(
 CWRESI, TAD, SEX, 
 groups = c('ID','TIME'), 
 subset = 'MDV == 0',
 yref = 0, 
 ysmooth = TRUE
)
}
}
\seealso{
Other superset: 
\code{\link{generalize}()},
\code{\link{ignored}()},
\code{\link{meta.character}()},
\code{\link{meta.numeric}()},
\code{\link{metaplot.numeric}()},
\code{\link{metaplot_character}()},
\code{\link{metasuperset}()},
\code{\link{meta}()},
\code{\link{ninput.character}()},
\code{\link{ninput.numeric}()},
\code{\link{ninput}()},
\code{\link{shuffle}()},
\code{\link{superset.character}()},
\code{\link{superset.numeric}()},
\code{\link{superset}()},
\code{\link{superspec.character}()},
\code{\link{superspec.numeric}()},
\code{\link{superspec}()}
}
\concept{superset}
