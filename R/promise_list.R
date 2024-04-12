# promise lists -----------------------------------------------------------------

#' Low-level promise list constructor
#' @param x a list
#' @noRd
new_promise_list = function(x = list()) {
  stopifnot(is.list(x))
  class(x) = c("uneval_promise_list", "list")
  x
}

#' A promise list
#'
#' @description
#' A lightweight wrapper around [list] designed to store
#' [promise]s. It is allowed to contain other types of objects as well.
#'
#' @param ... [promise]s (or other objects), possibly with names.
#'
#' @details
#' The primary use of [promise_list]s are as [list]s that have more useful
#' output for any [promise]s they contains when [print]ed or [format]ed, as
#' [promise]s by themselves do not print well.
#'
#' @returns a [list] with class `c("uneval_promise_list", "list")`.
#'
#' @examples
#' # promises in normal lists don't print well
#' list(promise(x + 1))
#'
#' # promise_lists show more useful information
#' promise_list(promise(x + 1))
#'
#' @family promise list constructors and predicates
#' @export
promise_list = function(...) {
  new_promise_list(list(...))
}


# predicates --------------------------------------------------------------

#' Is `x` a promise list?
#'
#' @description
#' `TRUE` if `x` is a [promise_list], `FALSE` otherwise.
#'
#' @param x an object.
#'
#' @returns scalar [logical].
#'
#' @family promise list constructors and predicates
#'
#' @examples
#' is_promise_list(promise_list())
#'
#' @export
is_promise_list = function(x) {
  inherits(x, "uneval_promise_list")
}


# indexing ----------------------------------------------------------------

#' @export
`[.uneval_promise_list` = function(x, ...) {
  new_promise_list(NextMethod())
}


# printing and formatting -------------------------------------------------

#' @export
print.uneval_promise_list = function(x, ...) {
  cat0("<promise_list>:\n")
  print(lapply(x, make_printable))
  invisible(x)
}

#' @export
format.uneval_promise_list = function(x, ...) {
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
