# capture many args -------------------------------------------------------

#' Capture promises for arguments to a function call
#'
#' @description
#' Capture a list of [promise]s representing arguments to the surrounding
#' function (or another function specified by `which`).
#'
#' @param which (scalar [integer]) the frame number to get call information
#' from, as returned by a function like [sys.parent()]. The default looks at
#' the arguments passed to the function that called this one.
#'
#' @details
#' `capture_all()` captures all arguments to the function call.
#'
#' @returns A [promise_list] where names are names of arguments to the function
#' in the given frame and values are the promises corresponding to those
#' arguments.
#'
#' @seealso [capture()] for capturing the [promise] associated with a single
#' argument.
#'
#' @examples
#' # captures x, y, z
#' f = function(x, y, ...) capture_all()
#' f(1, y = 2, z = 3)
#'
#' # captures x, y
#' f = function(x, y, ...) capture_named()
#' f(1, y = 2, z = 3)
#'
#' # captures z
#' f = function(x, y, ...) capture_dots()
#' f(1, y = 2, z = 3)
#'
#' @name capture_all
#' @export
capture_all = function(which = sys.parent()) {
  named_args = capture_named(which)
  dots_args = capture_dots(which)
  all_args = c(named_args, dots_args)
  # TODO: this line might be redundant / can maybe just return all_args
  new_promise_list(match_function_args(sys.function(which), all_args))
}

#' @details
#' `capture_named()` captures named arguments (i.e. those explicitly
#' listed in the function definition).
#'
#' @rdname capture_all
#' @export
capture_named = function(which = sys.parent()) {
  env = sys.frame(which)
  dots_env = do.call(parent.frame, list(), envir = env)

  f = sys.function(which)
  call = match.call(f, sys.call(which), envir = dots_env)
  arg_names = intersect(names(call[-1]), names(formals(args(f))))
  promises = lapply(arg_names, find_promise, env)
  names(promises) = arg_names
  new_promise_list(promises)
}

#' @details
#' `capture_dots()` captures arguments passed via `...`
#'
#' @rdname capture_all
#' @export
capture_dots = function(which = sys.parent()) {
  env = sys.frame(which)
  dots = env$...
  if (missing(dots)) {
    new_promise_list()
  } else {
    new_promise_list(dots_to_list_(dots))
  }
}


# capture one arg ---------------------------------------------------------

#' Capture promise for one argument to a function call
#'
#' @description
#' Capture a [promise] representing one argument to the surrounding
#' function (or another function specified by `env`).
#'
#' @param x (bare [symbol]) the name of the argument.
#' @param env ([environment]) the environment to search for the promise. The
#' default looks at the arguments passed to the function that called this one.
#'
#' @returns One of:
#' - If `x` refers to a promise, the promise with the given name from the
#'   given environment. Promises whose code is itself a promise (possibly
#'   recursively) are unwrapped so that the code referred to by the returned
#'   promise is not also a promise.
#' - If `x` does not refer to a promise, it is returned as-is.
#'
#' @template returns-promise
#'
#' @seealso [capture_all()], [capture_named()], and [capture_dots()] for
#' capturing the [promise]s associated with multiple arguments.
#'
#' @examples
#' f = function(x) capture(x)
#'
#' # if we capture a raw promise, its output is not
#' # very informative...
#' f(1 + 2)
#'
#' # ... we can wrap it in a promise_list to get
#' # more information
#' promise_list(f(1 + 2))
#'
#' @export
capture = function(x, env = parent.frame()) {
  expr = substitute(x)
  stopifnot(is.symbol(expr))
  # seem to need to do wrap the call to find_promise() in a list rather than
  # returning directly to avoid evaluating the promise...
  list(find_promise(expr, env))[[1]]
}
