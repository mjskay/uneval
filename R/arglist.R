# argument lists -----------------------------------------------------------------

#' Low-level argument list constructor
#' @param x <[`list`]> a list of arguments
#' @returns a [`list`] with class `c("uneval_arglist", "list")`.
#' @noRd
new_arglist = function(x = list()) {
  stopifnot(is.list(x))
  class(x) = c("uneval_arglist", "list")
  x
}

#' An argument list
#'
#' @description
#' A lightweight wrapper around [`list`] designed to store values to be passed
#' to function arguments, including [`promise`]s.
#'
#' @param ... objects, including [`promise`]s, possibly with names.
#'
#' @details
#' `arglist`s are [`list`]s that have more useful output for any [`promise`]s
#' they contains when [`print`]ed or [`format`]ed, as [`promise`]s by themselves
#' do not print well. They otherwise act as normal [`list`]s.
#'
#' @returns a [`list`] with class `c("uneval_arglist", "list")`.
#'
#' @examples
#' # promises in normal lists don't print well
#' list(promise(x + 1))
#'
#' # arglists show more useful information
#' arglist(promise(x + 1))
#'
#' @family promise list constructors and predicates
#' @export
arglist = function(...) {
  new_arglist(list(...))
}


# predicates --------------------------------------------------------------

#' Is `x` an argument list?
#'
#' @description
#' `TRUE` if `x` is an [`arglist`], `FALSE` otherwise.
#'
#' @param x an object.
#'
#' @returns scalar [`logical`].
#'
#' @family argument list constructors and predicates
#'
#' @examples
#' is_arglist(arglist())
#'
#' @export
is_arglist = function(x) {
  inherits(x, "uneval_arglist")
}


# indexing ----------------------------------------------------------------

#' @export
`[.uneval_arglist` = function(x, ...) {
  new_arglist(NextMethod())
}


# concatenation -----------------------------------------------------------

#' @export
c.uneval_arglist = function(...) {
  out = NextMethod()
  if (is.list(out)) out = new_arglist(out)
  out
}


# printing and formatting -------------------------------------------------

#' @export
print.uneval_arglist = function(x, ...) {
  cat0("<arglist>:\n")
  print(lapply(x, make_printable))
  invisible(x)
}

#' @export
format.uneval_arglist = function(x, ...) {
  format(lapply(x, make_printable), ...)
}

#' @export
print.uneval_formatted_promise = function(x, ...) {
  cat0(x, "\n")
  invisible(x)
}

make_printable = function(x) {
  if (typeof(x) == "promise") {
    expr = promise_expr(x)
    env = promise_env(x)
    structure(
      paste0("<promise: ", deparse0(expr), ", ", format(env), ">"),
      class = c("uneval_formatted_promise", "character")
    )
  } else {
    x
  }
}
