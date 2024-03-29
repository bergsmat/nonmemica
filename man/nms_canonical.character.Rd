% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/canonical.R
\name{nms_canonical.character}
\alias{nms_canonical.character}
\title{Generate Canonical Names for Character}
\usage{
\method{nms_canonical}{character}(x, ...)
}
\arguments{
\item{x}{object of dispatch}

\item{...}{passed arguments}
}
\description{
Generates canonical names for character by converting to parsed model.
}
\examples{
library(magrittr)
options(project = system.file('project/model',package='nonmemica'))
1001 \%>\% nms_canonical
}
\seealso{
Other nms: 
\code{\link{as_nms_canonical}()},
\code{\link{as_nms_nonmem}()},
\code{\link{as_nms_pmx}()},
\code{\link{as_nms_psn}()},
\code{\link{nms_canonical.model}()},
\code{\link{nms_canonical.nms_nonmem}()},
\code{\link{nms_canonical.nms_pmx}()},
\code{\link{nms_canonical.numeric}()},
\code{\link{nms_canonical}()},
\code{\link{nms_nonmem.character}()},
\code{\link{nms_nonmem.model}()},
\code{\link{nms_nonmem.nms_canonical}()},
\code{\link{nms_nonmem.nms_pmx}()},
\code{\link{nms_nonmem.numeric}()},
\code{\link{nms_nonmem}()},
\code{\link{nms_pmx.nms_canonical}()},
\code{\link{nms_pmx.nms_nonmem}()},
\code{\link{nms_pmx}()},
\code{\link{nms_psn.character}()},
\code{\link{nms_psn.model}()},
\code{\link{nms_psn.numeric}()},
\code{\link{nms_psn}()}
}
\concept{nms}
