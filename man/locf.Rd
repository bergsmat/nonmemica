% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{locf}
\alias{locf}
\alias{forbak}
\alias{bakfor}
\alias{nocb}
\title{Impute Missing Vector Values}
\usage{
locf(x)

forbak(x)

bakfor(x)

nocb(x)
}
\arguments{
\item{x}{a vector possibly with some missing values and some non-missing values}
}
\value{
a vector like \code{x}
}
\description{
\code{locf()} implements 'last observation carried forward': \code{NA}'s are imputed with the most recent non-\code{NA} value. \code{nocb()} is the complement: 'next observation carried backward': \code{NA}'s are imputed with the next non-NA value. \code{forbak()} first executes \code{locf()}, then \code{nocb()}, so that even leading NAs are imputed. If even one non-NA value is present, \code{forbak()} should not return any \code{NA}'s. \code{bakfor()} does the reverse.
}
\section{Functions}{
\itemize{
\item \code{forbak()}: forbak locf followed by nocb

\item \code{bakfor()}: bakfor nocb followed by locf

\item \code{nocb()}: nocb next observation carried backward

}}
\examples{
locf(c(NA,1,2,NA,NA,3,NA,4,NA))
nocb(c(NA,1,2,NA,NA,3,NA,4,NA))
forbak(c(NA,1,2,NA,NA,3,NA,4,NA))
}
\seealso{
Other util: 
\code{\link{\%contains\%}()},
\code{\link{as.best.default}()},
\code{\link{as.best}()},
\code{\link{enclose}()},
\code{\link{padded}()},
\code{\link{parens}()},
\code{\link{pool}()},
\code{\link{runhead}()},
\code{\link{text2decimal}()}

Other util: 
\code{\link{\%contains\%}()},
\code{\link{as.best.default}()},
\code{\link{as.best}()},
\code{\link{enclose}()},
\code{\link{padded}()},
\code{\link{parens}()},
\code{\link{pool}()},
\code{\link{runhead}()},
\code{\link{text2decimal}()}

Other util: 
\code{\link{\%contains\%}()},
\code{\link{as.best.default}()},
\code{\link{as.best}()},
\code{\link{enclose}()},
\code{\link{padded}()},
\code{\link{parens}()},
\code{\link{pool}()},
\code{\link{runhead}()},
\code{\link{text2decimal}()}

Other util: 
\code{\link{\%contains\%}()},
\code{\link{as.best.default}()},
\code{\link{as.best}()},
\code{\link{enclose}()},
\code{\link{padded}()},
\code{\link{parens}()},
\code{\link{pool}()},
\code{\link{runhead}()},
\code{\link{text2decimal}()}
}
\concept{util}
\keyword{internal}
