# capture many args -------------------------------------------------------

#' Capture promises for arguments to a function call
#'
#' @description
#' Capture a list of [`promise`]s representing arguments to the surrounding
#' function call (or another function call specified by its frame, `env`).
#'
#' @param env <[`environment`]> the frame to capture arguments from.
#' The default looks at the arguments passed to the function that called this one.
#'
#' @details
#' `capture_all()` captures all arguments to the function call.
#'
#' `capture_named()` captures named arguments (i.e. those explicitly
#' listed in the function definition).
#'
#' `capture_dots()` captures arguments passed via `...`
#'
#' @returns An [`arglist`] where names are names of arguments to the function
#' in the given frame and values are the promises corresponding to those
#' arguments.
#'
#' @seealso [capture()] for capturing the [`promise`] associated with a single
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
capture_all = function(env = parent.frame()) {
  named_args = capture_named(env)
  dots_args = capture_dots(env)
  all_args = c(named_args, dots_args)
  # TODO: this next two lines might be redundant / can maybe just return all_args
  f = do.call(sys.function, list(), envir = env)
  new_arglist(match_function_args(f, all_args))
}

#' @rdname capture_all
#' @export
capture_named = function(env = parent.frame()) {
  f = do.call(sys.function, list(), envir = env)
  call = do.call(match.call, list(), envir = env)
  arg_names = intersect(names(call[-1]), names(formals(args(f))))
  promises = lapply(arg_names, find_promise, env)
  names(promises) = arg_names
  new_arglist(promises)
}

#' @rdname capture_all
#' @export
capture_dots = function(env = parent.frame()) {
  dots = env$...
  if (missing(dots)) {
    new_arglist()
  } else {
    new_arglist(dots_to_list_(dots))
  }
}


# capture one arg ---------------------------------------------------------

#' Capture promise for one argument to a function call
#'
#' @description
#' Capture a [`promise`] representing one argument to the surrounding
#' function (or another function specified by `env`).
#'
#' @param x <bare [`symbol`]> the name of the argument.
#' @param env <[`environment`]> the environment to search for the promise. The
#' default looks at the arguments passed to the function that called this one.
#'
#' @returns One of:
#' - If `x` refers to a [`promise`], the promise with the given name from the
#'   given environment. Promises whose code is itself a promise (possibly
#'   recursively) are unwrapped so that the code referred to by the returned
#'   promise is not also a promise.
#' - If `x` does not refer to a promise, it is returned as-is.
#'
#' @template returns-promise
#'
#' @seealso [capture_all()], [capture_named()], and [capture_dots()] for
#' capturing the [`promise`]s associated with multiple arguments.
#'
#' @examples
#' f = function(x) capture(x)
#'
#' # if we capture a raw promise, its output is not
#' # very informative...
#' f(1 + 2)
#'
#' # ... we can wrap it in an arglist to get
#' # more information
#' arglist(f(1 + 2))
#'
#' @export
capture = function(x, env = parent.frame()) {
  # seem to need to do wrap the call to find_promise() in a list rather than
  # returning directly to avoid evaluating the promise...
  list(find_promise(substitute(x), env))[[1]]
}
