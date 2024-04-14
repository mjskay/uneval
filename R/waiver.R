# waiver ------------------------------------------------------------------

#' A default argument
#'
#' @description
#' A flag indicating that the default value of an argument should be used.
#'
#' @details
#' A [waiver()] is a flag passed to an argument of an [autopartial] function
#' that indicates the argument should keep its default value (or the most
#' recently partially-applied value of that argument).
#'
#' @seealso [autopartial()]
#' @family waiver constructors and predicates
#'
#' @examples
#' f = autopartial(function(x, y = "b") {
#'   c(x = x, y = y)
#' })
#'
#' f("a")
#'
#' # uses the default value of `y` ("b")
#' f("a", y = waiver())
#'
#' # partially apply `f`
#' g = f(y = "c")
#' g
#'
#' # uses the last partially-applied value of `y` ("c")
#' g("a", y = waiver())
#' @export
waiver = function() {
  structure(list(), class = "waiver")
}


# type predicates ---------------------------------------------------------

#' Is `x` a waiver?
#'
#' @description
#' `TRUE` if `x` is a [`waiver`] or a promise to a [`waiver`], `FALSE`
#' otherwise. Designed to detect waivers in [`arglist`]s without evaluating
#' arguments in many cases.
#'
#' @param x an object.
#'
#' @returns scalar [`logical`].
#'
#' @family waiver constructors and predicates
#'
#' @examples
#' is_waiver(waiver())
#' is_waiver(promise(waiver()))
#' x = waiver()
#' is_waiver(promise(x))
#'
#' @export
is_waiver = is_waiver_


# helpers -----------------------------------------------------------------

#' Remove waivers from an argument list
#' @param args a named list of promises
#' @returns A modified version of `args` with any waived arguments (i.e.
#' promises for which [is_waiver()] returns `TRUE`) removed
#' @noRd
remove_waivers = function(args) {
  waived = vapply(args, is_waiver, logical(1))
  args[!waived]
}

