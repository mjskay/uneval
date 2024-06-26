% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default.R
\name{is_default}
\alias{is_default}
\title{Is \code{x} a DEFAULT?}
\usage{
is_default(x)
}
\arguments{
\item{x}{an object.}
}
\value{
scalar \code{\link{logical}}.
}
\description{
\code{TRUE} if \code{x} is a \code{\link[=DEFAULT]{DEFAULT()}} or a promise to a \code{\link[=DEFAULT]{DEFAULT()}}, \code{FALSE}
otherwise. Designed to detect \code{\link[=DEFAULT]{DEFAULT()}} in \code{\link{arglist}}s without evaluating
arguments in many cases.
}
\examples{
is_default(DEFAULT())
is_default(promise(DEFAULT()))
x = DEFAULT()
is_default(promise(x))

}
\seealso{
Other DEFAULT constructors and predicates: 
\code{\link{DEFAULT}()}
}
\concept{DEFAULT constructors and predicates}
