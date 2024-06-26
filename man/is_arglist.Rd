% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arglist.R
\name{is_arglist}
\alias{is_arglist}
\title{Is \code{x} an argument list?}
\usage{
is_arglist(x)
}
\arguments{
\item{x}{an object.}
}
\value{
scalar \code{\link{logical}}.
}
\description{
\code{TRUE} if \code{x} is an \code{\link{arglist}}, \code{FALSE} otherwise.
}
\examples{
is_arglist(arglist())

}
\concept{argument list constructors and predicates}
