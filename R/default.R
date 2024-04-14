# DEFAULT ------------------------------------------------------------------

#' A default argument
#'
#' @description
#' A flag indicating that the default value of an argument should be used.
#'
#' @details
#' [DEFAULT()] is a flag passed to an argument of an [autopartial] function
#' that indicates the argument should keep its default value (or the most
#' recently partially-applied value of that argument). It is styled in all caps,
#' like `NULL` or `TRUE`, as a reminder that it is a special flag.
#'
#' @seealso [autopartial()]
#' @family DEFAULT constructors and predicates
#'
#' @examples
#' f = autopartial(function(x, y = "b") {
#'   c(x = x, y = y)
#' })
#'
#' f("a")
#'
#' # uses the default value of `y` ("b")
#' f("a", y = DEFAULT())
#'
#' # partially apply `f`
#' g = f(y = "c")
#' g
#'
#' # uses the last partially-applied value of `y` ("c")
#' g("a", y = DEFAULT())
#' @export
DEFAULT = function() {
  structure(list(), class = "uneval_default")
}


# type predicates ---------------------------------------------------------

#' Is `x` a DEFAULT?
#'
#' @description
#' `TRUE` if `x` is a [DEFAULT()] or a promise to a [DEFAULT()], `FALSE`
#' otherwise. Designed to detect [DEFAULT()] in [`arglist`]s without evaluating
#' arguments in many cases.
#'
#' @param x an object.
#'
#' @returns scalar [`logical`].
#'
#' @family DEFAULT constructors and predicates
#'
#' @examples
#' is_default(DEFAULT())
#' is_default(promise(DEFAULT()))
#' x = DEFAULT()
#' is_default(promise(x))
#'
#' @export
is_default = is_default_


# printing and formating --------------------------------------------------

#' @export
print.uneval_default = function(x, ...) {
  cat("DEFAULT()\n")
}

#' @export
format.uneval_default = function(x, ...) {
  "DEFAULT()"
}


# helpers -----------------------------------------------------------------

#' Remove DEFAULTs from an argument list
#' @param args a named list of promises
#' @returns A modified version of `args` with any default arguments
#' (arguments for which [is_default()] returns `TRUE`) removed
#' @noRd
remove_defaults = function(args) {
  default = vapply(args, is_default, logical(1))
  args[!default]
}

