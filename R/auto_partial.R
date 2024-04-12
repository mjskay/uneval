#' Automatic partial function application
#'
#' @description
#' Convert a function into one that supports *automatic partial application*.
#' When called, if all of its required arguments have not been provided, an
#' *automatically partially-applied* function returns a modified version of
#' itself that uses the arguments passed to it so far as defaults. Once all
#' required arguments have been supplied, the function is evaluated.
#'
#' `auto_partial()` can be considered a form of
#' [currying](https://en.wikipedia.org/wiki/Currying) with respect to
#' a function's required arguments, but I think *automatic partial application*
#' gets the idea across more clearly. `auto_partial()` also a number of
#' features, like named arguments and defaults, which are not typically
#' considered part of the classical definition of currying. See **Details**.
#'
#' @details
#' Create an *automatically partially-applied* function by passing it to
#' `auto_partial()`. The function can be called repeatedly until
#' all required arguments (i.e. those that do not have default values
#' provided in the function definition) have been supplied, then the function is
#' evaluated and the result returned.
#'
#' For example, consider the function `f`:
#'
#' ```r
#' f = function(x, y, z, a = 4) c(x, y, z, a)
#' f = auto_partial(f)
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
#' Great pains are taken to ensure that [auto_partial] functions act as much as
#' possible like normal R functions.
#'
#' The initial definition of an [auto_partial] function has the same
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
#' the final [auto_partial] function is called from, so functions that inspect
#' their context (e.g. using [parent.frame()]) should work correctly. The call
#' expression associated with the function invocation is also constructed
#' to reflect all of the partially-applied arguments and the original function
#' name, so functions that use [sys.call()] or [match.call()] should also give
#' nice-looking output.
#'
#' For example, the [density()] function, which uses both [substitute()] and
#' [match.call()] to construct labels for its output, gives identical output
#' if invoked directly (e.g. `density(rnorm(10))`) or via [auto_partial]
#' (e.g. `auto_partial(density)(rnorm(10))`).
#'
#' Under the hood, [auto_partial()] uses [new_auto_partial()] to create the
#' *automatically partially-applied* function, captures intermediate arguments
#' as [promise]s using [arg_promise_list()], and ultimately uses [pcall()]
#' to call the underlying function.
#' @examples
#' # TODO
#'
#' @name auto_partial
#' @aliases automatic-partial-functions
NULL


# waiver ------------------------------------------------------------------

#' A waived argument
#'
#' A flag indicating that the default value of an argument should be used.
#'
#' @details
#' A [waiver()] is a flag passed to an argument of an [auto_partial] function
#' that indicates the arguments should keep its default value (or the most
#' recently partially-applied value of that argument).
#'
#' @seealso [auto_partial()]
#' @examples
#' f = auto_partial(function(x, y = "b") {
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


# promise lists -----------------------------------------------------------------

new_promise_list = function(x = list()) {
  class(x) = "uneval_promise_list"
  x
}

#' construct a list of promises
#' @param ... unevaluated expressions, possibly with names
#' @returns a list of promises
#' @noRd
promise_list = function(...) {
  dot_arg_promise_list()
}

#' @export
`[.uneval_promise_list` = function(x, ...) {
  new_promise_list(NextMethod())
}

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

is_promise_list = function(x) {
  inherits(x, "uneval_promise_list")
}

# promises ----------------------------------------------------------------

# NOTE: individual promises are always stored in promise lists because
# they are fragile and prone to evaluating themselves if assigned to variables.
new_promise = function(expr, env = parent.frame()) {
  do.call(promise_list, list(expr), envir = env)
}

promise = function(x, env = parent.frame()) {
  new_promise(substitute(x), env)
}

arg_promise = function(x, env = parent.frame()) {
  expr = substitute(x)
  stopifnot(is.symbol(expr))
  # seem to need to do wrap the call to find_promise() in a list rather than
  # returning directly to avoid evaluating the promise...
  new_promise_list(list(find_promise(expr, env)))
}

#' Find a promise by name.
#' @param name the name of a promise as a string or symbol
#' @param env the environment to search
#' @returns One of:
#' - If `name` refers to a promise, the promise with the given name from the
#' given environment. Promises whose code is itself a promise (possibly
#' recursively) are unwrapped so that the code referred to by the returned
#' promise is not also a promise.
#' - If `name` does not refer to a promise, it is returned as a normal object.
#' @noRd
find_promise = find_promise_

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


# closures ----------------------------------------------------------------

#' Call a function using promises
#'
#' Calls a function using an argument list that may contain promises.
#'
#' @param f ([closure] or [primitive]) function to call
#' @param args ([promise_list], [list], or [pairlist]) list of arguments
#' to call the function with.
#' @param env ([environment]) environment to call `f` from. This will be
#' available as [parent.frame()] within `f`.
#' @param name ([language] or scalar [character]) name of the function to
#' use when constructing the unevaluated call. This is purely cosmetic; it
#' does not affect what function is called, but will be available as
#' the first element of [sys.call()] within `f`.
#'
#' This function is intended as an alternative to [do.call()] that provides
#' better support for promises when calling [closure]s. In particular,
#' promises in `args` will be left as-is, allowing precise manipulation of the
#' expressions and environments of the arguments to `f`.
#'
#' The name of the function in the unevaluated call provided to `f` via
#' [sys.call()] can also be set via `name`, making it possible to avoid
#' potentially-ugly captured function calls created by `match.call()` in `f`.
#' @noRd
pcall = function(f, args, env = parent.frame(), name = substitute(f)) {
  # a simpler version of the below would be something like:
  # > do.call(f, args, envir = env)
  # however this would lead to the function call appearing as the
  # full function body in things like match.call().
  switch(typeof(f),
    closure = {
      # for closures, we can manually construct the call such that the
      # following functions work when used in the closure:
      # (1) sys.call() / match.call() will pick up a nice-looking call
      # (2) substitute() gives nice-looking expressions
      # (3) parent.frame() gives the the same frame the user called us from
      arg_exprs = lapply(args, promise_expr)
      if (is.character(name)) name = as.symbol(name)
      call = as.call(c(list(name), arg_exprs))
      apply_closure(call, f, args, env)
    },
    special = {
      # for primitives / builtins we can at least ensure the calling frame
      # is the same frame the user called us from
      do.call(f, args, envir = env)
    },
    stop("`f` must be a function, not a ", typeof(f))
  )
}

#' Call a closure
#' @param call ([call]) expression of the function call with its arguments.
#' This is not evaluated, but will be available in the closure as [sys.call()].
#' @param fun ([closure]) the function to call.
#' @param args (coerced to [pairlist]) list of arguments as promises.
#' @param env ([environment]) environment to call the function from.
#' @noRd
apply_closure = function(call, fun, args, env) {
  apply_closure_(call, fun, as.pairlist(args), env)
}


# auto_partial ------------------------------------------------------------

#' @rdname auto_partial
#' @param .f A function
#' @param ... arguments to be partially applied to `.f`
#' @returns A modified version of `.f` that will automatically be partially
#' applied if all of its required arguments are not given.
#' @examples
#' # create a custom automatically partially applied function
#' f = auto_partial(function(x, y, z = 3) (x + y) * z)
#' f()
#' f(1)
#' g = f(y = 2)(z = 4)
#' g
#' g(1)
#'
#' # pass waiver() to optional arguments to use existing values
#' f(z = waiver())(1, 2)  # uses default z = 3
#' f(z = 4)(z = waiver())(1, 2)  # uses z = 4
#' @export
auto_partial = function(.f, ...) {
  name = short_function_name(substitute(.f))
  args = remove_waivers(match_function_args(.f, promise_list(...)))
  new_auto_partial(.f, args = args, name = name)
}

#' Partial function application
#'
#' Partially apply a function once.
#' @param .f a function
#' @param ... arguments to be partially applied to `.f`
#' @noRd
partial_ = function(.f, ...) {
  name = short_function_name(substitute(.f))
  args = remove_waivers(match_function_args(.f, promise_list(...)))
  new_auto_partial(.f, args = args, name = name, required_arg_names = character())
}

#' Low-level constructor for automatically partially-applied functions
#'
#' Construct a version of the function `f` that is partially applied when called
#' unless all required arguments have been supplied.
#' @param f ([closure] or [primitive]) function to automatically partially-apply
#' @param args ([list]; typically a [promise_list]) arguments to apply now
#' @param required_arg_names ([character] vector) names of required arguments
#' in `f`. When all of these have been supplied, the function will be evaluated.
#' The default, `find_required_arg_names(f)`, considers all arguments without a
#' default value in the function definition to be required. Pass `NULL` or
#' `character()` to get tradition (non-automatic) partial application.
#' @param name (scalar [character]) the name of the function. Used for printing
#' purposes only. If `NULL`, the expression passed in for `f` is used if that
#' expression is a symbol, and `"."` is used otherwise.
#' @param waivable (scalar [logical]) if `TRUE`, if you pass `waiver()` to an
#' argument to this function, whatever value that argument already has will be
#' used instead.
#' @returns a [function] that when called will be partially applied until all of
#' the arguments in `required_arg_names` have been supplied.
#' @noRd
new_auto_partial = function(
  f,
  args = promise_list(),
  required_arg_names = find_required_arg_names(f),
  name = NULL,
  waivable = TRUE
) {
  name = name %||% short_function_name(substitute(f))
  stopifnot(
    "`f` must be a function" = is.function(f),
    "`f` cannot be a primitive function without an argument lists, like `if`" = !is.null(args(f)),
    "`args` must be a list" = is.list(args) || is.pairlist(args),
    "`required_arg_names` must be a character vector" = is.character(required_arg_names) || is.null(required_arg_names),
    "`name` must be a string" = is.character(name) && length(name) == 1,
    "`waivable` must be a scalar logical" = is.logical(waivable) && length(waivable) == 1
  )

  # we use these weird names to avoid clashing with argument names in f,
  # because partial_f will have a signature containing the same formals as f,
  # so if those formals include the names f, args, etc, things would break
  `>f` = f
  `>args` = args
  `>required_arg_names` = required_arg_names
  `>name` = name
  `>waivable` = waivable

  partial_f = function() {
    new_args = arg_promise_list()
    if (`>waivable`) new_args = remove_waivers(new_args)
    `>args` = update_args(`>args`, new_args)

    if (all(`>required_arg_names` %in% names(`>args`))) {
      pcall(`>f`, `>args`, env = parent.frame(), name = `>name`)
    } else {
      new_auto_partial(`>f`, `>args`, `>required_arg_names`, `>name`, `>waivable`)
    }
  }
  partial_formals = formals(args(f))
  # update expressions in formals to match provided args
  updated_formal_names = intersect(names(partial_formals), names(args))
  partial_formals[updated_formal_names] = lapply(args[updated_formal_names], promise_expr)
  # move any required args that have been applied to the end. this allows
  # f(1)(2)(3)... to be equivalent to f(1, 2, 3, ...) if positions 1, 2, 3, ...
  # correspond to required arguments.
  is_updated_required = names(partial_formals) %in% intersect(updated_formal_names, required_arg_names)
  partial_formals = c(partial_formals[!is_updated_required], partial_formals[is_updated_required])
  formals(partial_f) = partial_formals

  attr(partial_f, "f") = f
  attr(partial_f, "args") = args
  attr(partial_f, "name") = name
  attr(partial_f, "waivable") = waivable
  class(partial_f) = c("uneval_auto_partial", "function")
  partial_f
}

short_function_name = function(f_expr) {
  if (is.symbol(f_expr)) deparse0(f_expr) else "."
}

#' @export
print.uneval_auto_partial = function(x, ..., width = getOption("width")) {
  cat0("<auto_partial", if (attr(x, "waivable")) " with waivers", ">:\n")

  name = attr(x, "name") %||% "."
  cat0(name, " = ")

  f = attr(x, "f")
  f_string = utils::capture.output(print(f, width = width - 2, ...))
  cat(f_string, sep = "\n  ")

  cat0(format(as.call(c(
    list(as.name(name)),
    lapply(attr(x, "args"), promise_expr)
  ))))

  invisible(x)
}

#' Given a function and a list of arguments, return a modified version of the
#' argument list where named arguments have been matched according to R's
#' argument-matching rules.
#' @param f a function
#' @param args a list of arguments, such as returned by [promise_list()].
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

#' @export
c.uneval_promise_list = function(...) {
  out = NextMethod()
  if (is.list(out)) out = new_promise_list(out)
  out
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

#' Retrieve a list of promises from arguments in
#' the surrounding function
#' @param which the frame number to get call information from.
#' @returns A list of promises for arguments to the
#' calling function
#' @noRd
arg_promise_list = function(which = sys.parent()) {
  named_arg_promises = named_arg_promise_list(which)
  dot_arg_promises = dot_arg_promise_list(which)
  arg_promises = c(named_arg_promises, dot_arg_promises)
  # TODO: this line might be redundant / maybe just return arg_promises
  new_promise_list(match_function_args(sys.function(which), arg_promises))
}

#' Retrieve a list of promises from named arguments in
#' the surrounding function
#' @param which the frame number to get call information from.
#' @returns A named list of promises for arguments to the
#' calling function
#' @noRd
named_arg_promise_list = function(which = sys.parent()) {
  env = sys.frame(which)
  dots_env = do.call(parent.frame, list(), envir = env)

  f = sys.function(which)
  call = match.call(f, sys.call(which), envir = dots_env)
  arg_names = intersect(names(call[-1]), names(formals(args(f))))
  promises = lapply(arg_names, find_promise, env)
  names(promises) = arg_names
  new_promise_list(promises)
}

#' Retrieve a list of promises from `...` arguments in
#' the surrounding function
#' @param which the frame number to get call information from.
#' @returns A list of (possibly named) promises for arguments to the
#' calling function
#' @noRd
dot_arg_promise_list = function(which = sys.parent()) {
  env = sys.frame(which)
  dots = env$...
  if (missing(dots)) {
    new_promise_list()
  } else {
    new_promise_list(dots_to_list_(dots))
  }
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
