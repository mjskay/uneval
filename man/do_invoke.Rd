% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invoke.R
\name{do_invoke}
\alias{do_invoke}
\title{Invoke a function using an argument list that may contain promises}
\usage{
do_invoke(f, args, call_env = parent.frame(), f_expr = substitute(f))
}
\arguments{
\item{f}{<\code{\link{closure}} | \code{\link{primitive}}> function to call}

\item{args}{<\code{\link{arglist}} | \code{\link{list}} | \code{\link{pairlist}}> list of arguments
to call the function with.}

\item{call_env}{<\code{\link{environment}}> environment to call \code{f} from. This will be
available as \code{\link[=parent.frame]{parent.frame()}} within \code{f}.}

\item{f_expr}{<\code{\link{language}}> an expression representing \code{f}. This is primarly
cosmetic; it does not affect what function is called, but may be used for
printing purposes and will be available as the first element of \code{\link[=sys.call]{sys.call()}}
within \code{f}.}
}
\value{
The result of evaluating the function \code{f}.
}
\description{
Call a function using an explicit argument list that may contain \link{promise}s.
This is a low-level interface intended to mimic \code{\link[=do.call]{do.call()}}; for a
higher-level interface, see \code{\link[=invoke]{invoke()}}.

Currently, if \code{f} is a \code{\link{primitive}} function, \code{\link[=invoke]{invoke()}} falls back to using
\code{\link[=do.call]{do.call()}}, so \code{f_expr} cannot be used to set the call expression seen by
\code{\link[=sys.call]{sys.call()}} in \code{f}.
}
\details{
This is intended as an alternative to \code{\link[=do.call]{do.call()}} that provides
better support for promises when calling \code{\link{closure}}s. In particular,
promises in \code{args} will be left as-is, allowing precise manipulation of the
expressions and environments of the arguments to \code{f} by using functions like
\code{\link[=promise]{promise()}}, \code{\link[=promises]{promises()}}, \code{\link[=capture]{capture()}}, \code{\link[=capture_all]{capture_all()}}, etc.
}
\examples{
# TODO

}
\seealso{
\code{\link[=invoke]{invoke()}}, the higher-level interface to \code{\link[=do_invoke]{do_invoke()}} intended
for normal use.
}
