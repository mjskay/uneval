#' Automatic partial function application
#'
#' @description
#' Construct an [autopartial] function; that is, a function that supports
#' *automatic partial application*.
#'
#' When a function created by [autopartial()] is called,
#' if all of its required arguments have not been provided, it returns a
#' modified version of itself that uses the arguments passed to it so far as
#' defaults. Once all required arguments have been supplied, the function is
#' evaluated.
#'
#' [autopartial()] can be considered a form of
#' [currying](https://en.wikipedia.org/wiki/Currying) with respect to
#' a function's required arguments, but I think *automatic partial application*
#' gets the idea across more clearly. [autopartial()] also has a number of
#' features, like named arguments and defaults, which are not typically
#' considered part of the classical definition of currying. See **Details**.
#'
#' @param .f ([closure] or [primitive]) function to turn into an automatically
#' partially-applied function.
#' @param ... optional arguments to be partially applied to `.f`.
#'
#' @details
#' Create an *automatically partially-applied* function by passing it to
#' `autopartial()`. The function can be called repeatedly until
#' all required arguments (i.e. those that do not have default values
#' provided in the function definition) have been supplied, then the function is
#' evaluated and the result returned.
#'
#' For example, consider the function `f`:
#'
#' ```r
#' f = function(x, y, z, a = 4) c(x, y, z, a)
#' f = autopartial(f)
#' ```
#'
#' It can be called as normal; e.g. `f(1, 2, 3)`, which yields `c(1, 2, 3, 4)`).
#' Equivalently, it could be called as `f(1)(2)(3)`, or `f(z = 3)(1, 2)`,
#' or `f(y = 2, z = 3)(1)`, or `f(y = 2)(1)(3)`, etc. Positional arguments
#' can be supplied by position or by name. Named arguments can be supplied
#' anywhere in the sequence; e.g. `f(a = 7)(1, 2, 3)` is equivalent to
#' `f(1, 2, 3, a = 7)`.
#'
#' Arguments supplied in one partial call can be overridden later in the sequence.
#' For example, `f(1)(x = 2)(x = 3)` is equivalent to just `f(x = 3)`.
#'
#' Arguments may also be passed the special value [waiver()]. If [waiver()] is
#' passed to an argument, its default value (or the most recently-partially-applied
#' non-[waiver] value) is used instead. For example, `f(a = waiver())` is
#' equivalent to `f(a = 4)` (since the default value of `a` is `4` in the
#' definition of `f`), and `f(x = 1)(x = waiver())` is equivalent to `f(x = 1)`.
#'
#' @section Implementation details:
#' Great pains are taken to ensure that [autopartial] functions act as much as
#' possible like normal R functions.
#'
#' The initial definition of an [autopartial] function has the same
#' [formals()] as the function it wraps. When it is further partially applied,
#' the [formals()] are updated to reflect the expressions of the modified
#' arguments. To allow for sequences of applications of positional arguments
#' (e.g. `f(1)(2)(3)`) to be equivalent to a single application (e.g.
#' `f(1, 2, 3)`), positional arguments are moved to the back of the [formals()]
#' list when they are applied.
#'
#' During partial application, arguments are stored as [promise]s and will not
#' be evaluated until the underlying function is ultimately called (and even
#' then, an argument may not be evaluated if that function does not evaluate
#' the argument in question---just like normal R functions). Thus, non-standard
#' evaluation constructs like [substitute()] should work correctly within the
#' underlying function.
#'
#' [waiver()] values are detected, as much as possible, without evaluating
#' arguments: a [waiver()] is valid only if it is stored in a symbol passed to
#' an argument or if `waiver()` is passed directly to the argument.
#'
#' The final function evaluation acts as much as possible like a normal function
#' evaluation. The underlying function is called from the same environment that
#' the final [autopartial] function is called from, so functions that inspect
#' their context (e.g. using [parent.frame()]) should work correctly. The call
#' expression associated with the function invocation is also constructed
#' to reflect all of the partially-applied arguments and the original function
#' name, so functions that use [sys.call()] or [match.call()] should also give
#' nice-looking output.
#'
#' For example, the [density()] function, which uses both [substitute()] and
#' [match.call()] to construct labels for its output, gives identical output
#' if invoked directly (e.g. `density(rnorm(10))`) or via [autopartial]
#' (e.g. `autopartial(density)(rnorm(10))`).
#'
#' Under the hood, [autopartial()] uses [new_autopartial()] to create the
#' *automatically partially-applied* function, captures intermediate arguments
#' as [promise]s using [capture_all()], and ultimately uses [invoke()]
#' to call the underlying function.
#'
#' @returns
#' A modified version of `.f` that will automatically be partially
#' applied until all of its required arguments have been given.
#'
#' @seealso [new_autopartial()] for lower-level control when constructing
#' (automatic or non-automatic) partial functions.
#' @seealso [partial()] to partially apply a function once.
#'
#' @examples
#' # create a custom automatically partially-applied function
#' f = autopartial(function(x, y, z = 3) (x + y) * z)
#' f()
#' f(1)
#' g = f(y = 2)(z = 4)
#' g
#' g(1)
#'
#' # pass waiver() to optional arguments to use existing values
#' f(z = waiver())(1, 2)  # uses default z = 3
#' f(z = 4)(z = waiver())(1, 2)  # uses z = 4
#'
#' @name autopartial
NULL


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


# promises ----------------------------------------------------------------

promises = function(...) {
  capture_dots()
}

new_promise = function(expr, env = parent.frame()) {
  do.call(promises, list(expr), envir = env)[[1]]
}

promise = function(x, env = parent.frame()) {
  new_promise(substitute(x), env)
}

promise_expr = function(x) {
  if (typeof(x) == "promise") {
    promise_expr_(x)
  } else {
    x
  }
}

promise_env = function(x) {
  if (typeof(x) == "promise") {
    promise_env_(x)
  } else {
    NULL
  }
}


# autopartial ------------------------------------------------------------

#' @rdname autopartial
#' @export
autopartial = function(.f, ...) {
  args = remove_waivers(match_function_args(.f, capture_dots()))
  new_autopartial(.f, args, f_expr = substitute(.f))
}

#' Partial function application
#'
#' @description
#' Partially apply a function.
#'
#' @param .f ([closure] or [primitive]) function to be partially applied.
#' @param ... arguments to be partially applied to `.f`.
#'
#' @details
#' Partially applies the provided arguments to the function `.f`. Acts like
#' [autopartial()], except that the next invocation will evaluate the function
#' rather than waiting for all required arguments to be supplied.
#'
#' Arguments may also be passed the special value [waiver()]. If [waiver()] is
#' passed to an argument, its default value is used instead.
#'
#' Great pains are taken to ensure that the resulting function acts as much as
#' possible like the original function. See the **Implementation details**
#' section of [autopartial()] for more information.
#'
#' @returns
#' A modified version of `.f` with the arguments in `...` applied to it.
#'
#' @seealso [autopartial()] for automatic partial function application.
#'
#' @examples
#' f = function(x, y, z = 3) c(x, y, z)
#' fp = partial(f, 1, z = 4)
#' fp
#' fp(2)
#'
#' @export
partial = function(.f, ...) {
  args = remove_waivers(match_function_args(.f, capture_dots()))
  new_autopartial(.f, args, required_arg_names = character(), f_expr = substitute(.f))
}

#' Low-level constructor for automatically partially-applied functions
#'
#' @description
#' Construct a version of the function `f` that is partially applied when called
#' unless all required arguments have been supplied. This is a low-level
#' constructor that should be used only if you need to manually adjust
#' `required_arg_names`, `waivable`, or `f_expr`. In most cases, you should use
#' the higher-level interfaces [autopartial()] or [partial()].
#'
#' @param f <[`closure`] | [`primitive`]> function to automatically partially-apply.
#' @param args <[`list`] | [`pairlist`] | [`arglist`]> arguments to apply now.
#' @param required_arg_names <[`character`]> names of required arguments
#' in `f`. When all of these have been supplied, the function will be evaluated.
#' The default, `find_required_arg_names(f)`, considers all arguments without a
#' default value in the function definition to be required. Pass `NULL` or
#' `character()` to get traditional (non-automatic) partial application.
#' @param waivable <[`logical`]> if `TRUE`, if you pass `waiver()` to an
#' argument to this function, whatever value that argument already has will be
#' used instead.
#' @template param-invoke-f_expr
#'
#' @returns a [`function`] that when called will be partially applied until all
#' of the arguments in `required_arg_names` have been supplied.
#'
#' @seealso [autopartial()] and [partial()] for higher-level interfaces to
#' constructing partial functions.
#'
#' @examples
#' # TODO
#'
#' @export
new_autopartial = function(
  f,
  args = arglist(),
  required_arg_names = find_required_arg_names(f),
  waivable = TRUE,
  f_expr = substitute(f)
) {
  stopifnot(
    "`f_expr` must be a language object" = is.language(f_expr),
    "`f` must be a function" = is.function(f),
    "`f` cannot be a primitive function without an argument list, like `if`" = !is.null(args(f)),
    "`args` must be a list" = is.list(args) || is.pairlist(args),
    "`required_arg_names` must be a character vector" = is.character(required_arg_names) || is.null(required_arg_names),
    "`waivable` must be a scalar logical" = is.logical(waivable) && length(waivable) == 1
  )

  # we use these weird names to avoid clashing with argument names in f,
  # because partial_f will have a signature containing the same formals as f,
  # so if those formals include the names f, args, etc, things would break
  `>f` = f
  `>args` = args
  `>required_arg_names` = required_arg_names
  `>waivable` = waivable
  `>f_expr` = f_expr

  partial_f = function() {
    new_args = capture_all()
    if (`>waivable`) new_args = remove_waivers(new_args)
    args = update_args(`>args`, new_args)

    if (all(`>required_arg_names` %in% names(args))) {
      do_invoke(`>f`, args, call_env = parent.frame(), f_expr = `>f_expr`)
    } else {
      new_autopartial(`>f`, args, `>required_arg_names`, `>waivable`, `>f_expr`)
    }
  }
  partial_formals = formals(args(f))
  # update expressions in formals to match provided args
  updated_formal_names = intersect(names(partial_formals), names(args))
  partial_formals[updated_formal_names] = lapply(args[updated_formal_names], promise_expr)
  # move any positional args that have been applied to the end. this allows
  # f(1)(2)(3)... to be equivalent to f(1, 2, 3, ...) if positions 1, 2, 3, ...
  # correspond to required arguments.
  positional_arg_names = find_required_arg_names(f)
  is_updated_required = names(partial_formals) %in% intersect(updated_formal_names, positional_arg_names)
  partial_formals = c(partial_formals[!is_updated_required], partial_formals[is_updated_required])
  formals(partial_f) = partial_formals

  attr(partial_f, "f") = f
  attr(partial_f, "args") = args
  attr(partial_f, "waivable") = waivable
  attr(partial_f, "f_expr") = f_expr
  class(partial_f) = c("uneval_autopartial", "function")
  partial_f
}

short_function_name = function(f_expr) {
  if (is.symbol(f_expr)) deparse0(f_expr) else "."
}

#' @export
print.uneval_autopartial = function(x, ..., width = getOption("width")) {
  cat0("<autopartial", if (attr(x, "waivable")) " with waivers", ">:\n")

  f_expr = attr(x, "f_expr")
  if (!is.symbol(f_expr)) f_expr = "."
  cat0(f_expr, " = ")

  f = attr(x, "f")
  f_string = utils::capture.output(print(f, width = width - 2, ...))
  cat(f_string, sep = "\n  ")

  cat0(format(as.call(c(
    list(f_expr),
    lapply(attr(x, "args"), promise_expr)
  ))))

  invisible(x)
}

#' Given a function and a list of arguments, return a modified version of the
#' argument list where named arguments have been matched according to R's
#' argument-matching rules.
#' @param f a function
#' @param args a list of arguments, such as returned by [arglist()].
#' Should not contain a `...` argument.
#' @returns a standardized list of arguments (i.e. where all arguments with
#' names are named and in order) that can be supplied to `f`, or an error
#' if `args` is not a valid argument list for `f`.
#' @noRd
match_function_args = function(f, args) {
  # use match.call to figure out the names of arguments and the
  # argument order
  args_i = seq_along(args)
  names(args_i) = names(args)
  call =  as.call(c(list(quote(f)), args_i))
  args_i_call = match.call(args(f), call)[-1]
  args_i = as.integer(as.list(args_i_call))

  # fill in names and re-order the args
  names(args)[args_i] = names(args_i_call)
  args[args_i]
}

#' Update a list of arguments, overwriting named arguments in `old_args`
#' with values that appear in `new_args`. Positional arguments from both
#' argument lists are kept in the same order as they appear.
#' @param old_args a list of arguments
#' @param new_args a list of arguments
#' @returns a list of arguments
#' @noRd
update_args = function(old_args, new_args) {
  if (is.null(names(old_args))) names(old_args) = rep("", length(old_args))
  if (is.null(names(new_args))) names(new_args) = rep("", length(new_args))

  old_names = names(old_args)
  old_names = old_names[nzchar(old_names)]
  new_names = names(new_args)
  updated_names = intersect(old_names, new_names)
  old_args[updated_names] = new_args[updated_names]

  c(old_args, new_args[!names(new_args) %in% updated_names])
}

#' Return the names of required arguments for function `f`
#' @param f A function
#' @returns character vector of argument names
#' @noRd
find_required_arg_names = function(f) {
  args = formals(args(f))
  is_missing = vapply(args, is_missing_arg, logical(1))
  setdiff(names(args)[is_missing], "...")
}

#' Is `x` a missing argument?
#' @param an argument
#' @returns `TRUE` if `x` represents a missing argument
#' @noRd
is_missing_arg = function(x) {
  missing(x) || identical(x, quote(expr = ))
}

#' Remove waivers from an argument list
#' @param args a named list of promises
#' @returns A modified version of `args` with any waived arguments (i.e.
#' promises for which [is_waiver()] returns `TRUE`) removed
#' @noRd
remove_waivers = function(args) {
  waived = vapply(args, is_waiver, logical(1))
  args[!waived]
}

#' turn arguments that are in the given environment into expressions instead
#' of promises
#' @param args a named list of promises
#' @param env an environment
#' @returns a named list of promises and unevaluated expressions, where promises
#' with the environment `env` have been turned into their corresponding
#' expressions
#' @noRd
unpromise_in_env = function(args, env) {
  lapply(args, function(arg) {
    if (identical(promise_env(arg), env)) promise_expr(arg) else arg
  })
}
