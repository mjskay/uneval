# promise -----------------------------------------------------------------

#' Low-level promise constructor
#'
#' @description
#' Manually construct a [`promise`].
#'
#' @param expr <[`language`]> expression to wrap in a promise.
#' @param env <[`environment`]> environment `expr` is promised to be executed in.
#'
#' @returns A [`promise`].
#' @template returns-promise-warning
#'
#' @template seealso-promise-methods
#' @template seealso-capture
#' @template seealso-capture-args
#' @family promise constructors
#'
#' @examples
#' arglist(new_promise(quote(x + 1)))
#'
#' @export
new_promise = function(expr, env = parent.frame()) {
  do.call(promises, list(expr), envir = env)[[1]]
}

#' A promise
#'
#' @description
#' Create a promise from a bare expression. Promises are what power R's lazy
#' evaluation: they are objects that contain an expression and an
#' [`environment`] in which that expression should be executed.
#'
#' @param expr <bare [`language`]> expression to wrap in a promise.
#'
#' @returns An R *promise*: an object with `typeof(.) == "promise"`. The
#' [`environment`] associated with the promise will be that of the function that
#' called `promise()`.
#' @template returns-promise-warning
#'
#' @template seealso-promise-methods
#' @template seealso-capture
#' @template seealso-capture-args
#' @family promise constructors
#'
#' @examples
#' arglist(promise(x + 1))
#'
#' @export
promise = function(expr) {
  capture(expr)
}


# promises ----------------------------------------------------------------

#' Construct a list of promises
#'
#' @description
#' Create an [`arglist`] of promises from bare expressions.
#'
#' @param ... bare expressions to wrap in promises.
#'
#' @returns an [`arglist`] of [`promise`]s.
#'
#' @template seealso-promise-methods
#' @template seealso-capture
#' @template seealso-capture-args
#' @family promise constructors
#'
#' @examples
#' promises(1, x + 1, y)
#'
#' @export
promises = function(...) {
  capture_dots()
}


# type conversion ---------------------------------------------------------

#' Convert an object to a promise
#'
#' @description
#' Convert an object to a [`promise`].
#'
#' @param x object to convert to a promise: may be a quoted expression, or
#' may be an object that wraps an expression and an environment, such as
#' a [`formula`].
#' @param env <[`environment`]> environment `expr` is promised to be executed in.
#' When applied to a [`formula`], the environment associated with the
#' [`formula`] will be used.
#'
#' @returns A [`promise`].
#' @template returns-promise-warning
#'
#' @template seealso-promise-methods
#' @template seealso-capture
#' @template seealso-capture-args
#' @family promise constructors
#'
#' @examples
#' arglist(as_promise(quote(x + 1)))
#'
#' arglist(as_promise( ~ x + 1))
#'
#' @export
as_promise = function(x, env = parent.frame()) {
  UseMethod("as_promise")
}

#' @rdname as_promise
#' @export
as_promise.default = function(x, env = parent.frame()) {
  new_promise(x, env)
}

#' @rdname as_promise
#' @export
as_promise.formula = function(x, env = parent.frame()) {
  new_promise(x[[length(x)]], environment(x))
}

# methods -----------------------------------------------------------------

#' Get a promise expression
#'
#' @description
#' Return the expression associated with a [`promise`].
#'
#' @param x <[`promise`]>
#'
#' @returns an expression if `x` is a [`promise`]; otherwise `x`.
#'
#' @examples
#' promise_expr(promise(x + 1))
#'
#' @export
promise_expr = function(x) {
  if (typeof(x) == "promise") {
    promise_expr_(x)
  } else {
    x
  }
}

#' Get a promise environment
#'
#' @description
#' Return the [`environment`] associated with a [`promise`].
#'
#' @param x <[`promise`]>
#'
#' @returns an [`environment`] if `x` is a [`promise`]; otherwise `NULL`.
#'
#' @examples
#' promise_env(promise(x + 1))
#'
#' @export
promise_env = function(x) {
  if (typeof(x) == "promise") {
    promise_env_(x)
  } else {
    NULL
  }
}
