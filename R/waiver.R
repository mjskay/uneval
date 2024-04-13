# waiver ------------------------------------------------------------------

#' A default argument
#'
#' A flag indicating that the default value of an argument should be used.
#'
#' @details
#' A [waiver()] is a flag passed to an argument of an [autopartial] function
#' that indicates the arguments should keep its default value (or the most
#' recently partially-applied value of that argument).
#'
#' @seealso [autopartial()]
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

#' waiver-coalescing operator
#' @noRd
`%|W|%` = function(x, y) {
  if (inherits(x, "waiver")) y
  else x
}

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

