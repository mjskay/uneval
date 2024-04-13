# invoke ------------------------------------------------------------------

#' Invoke a function using promises
#'
#' @description
#' Call a function using arguments that may be [promise]s.
#' [invoke()] is syntactic sugar for [do_invoke()] that is designed to look
#' more like a function call and which allows splicing-in of argument lists
#' via assignment to `...`.
#'
#' @param expr <bare [`call`]> an expression giving the function and
#' arguments to be called. Unlike normal functoin invocation, arguments will
#' not be automatically turned into promises, so must be wrapped in [promise()]
#' if you wish them to be evaluated lazily. Dots can be passed using `...`, and
#' lists of arguments can be spliced in using `... = <list of arguments>`.
#' @param env <[`environment`]> the environment to evaluate the function
#' definition and arguments extracted from `expr` in.
#' @template param-invoke-call_env
#' @template param-invoke-f_expr
#'
#' @details
#' This function allows you to call another function while explicitly giving
#' each argument as either an already-evaluated object or as a [`promise`].
#'
#' Consider a function like this:
#'
#' ```{r}
#' f = function(...) match.call()
#' ```
#'
#' We can call it as follows:
#'
#' ```{r}
#' y = 2
#' z = 3
#' f(1 + 2, x = y + z)
#' ```
#'
#' The standard function invocation `f(1, x = y + z)` creates [`promise`]s for
#' each argument, ensuring they are lazily evaluated by the underlying call.
#' Because they are not evaluated, `match.call()` shows the unevaluated
#' expressions when called.
#'
#' `invoke()` makes the creation of argument [`promise`]e explicit, requiring
#' you to wrap expressions in [promise()] if you wish them to be evaluated
#' lazily. Thus, the equivalent of the above call with `invoke()` is:
#'
#' ```{r}
#' invoke(f(promise(1 + 2), x = promise(y + z)))
#' ```
#'
#' By making construction of argument [`promise`]s explicit, we can more easily
#' manipulate when and how arguments are evaluated. For example, we can evaluate
#' arguments at call time by not wrapping them in [promise()]:
#'
#' ```{r}
#' invoke(f(1 + 2, x = y + z))
#' ```
#'
#' Or, we can pass down argument [`promise`]s captured via [`capture`]:
#'
#' ```{r}
#' g = function(x, ...) invoke(f(1 + 2, x = capture(x), ...))
#' g(x = y + z, i = j)
#' ```
#'
#' Notice how `...` is also forwarded above, allowing the `i` argument to be
#' forwarded. Lists of arguments can also be spliced in using `... = `:
#'
#' ```{r}
#' invoke(f(1 + 2, ... = list(x = y + z, i = promise(j)), m = 8))
#' ```
#'
#' @template details-invoke-f_expr
#'
#' @template returns-invoke
#'
#' @seealso [do_invoke()], the low-level function-calling interface used by
#' [invoke()].
#'
#' @examples
#' # TODO
#'
#' @export
invoke = function(expr, env = parent.frame(), call_env = env, f_expr = substitute(expr)[[1]]) {
  call = substitute(expr)
  stopifnot(is.call(call))

  f = eval(call[[1]], env)
  args = eval_args(call[-1], env)

  do_invoke(f, args, call_env = call_env, f_expr = f_expr)
}

#' Invoke a function using an argument list that may contain promises
#'
#' @description
#' Call a function using an explicit argument list that may contain [promise]s.
#' This is a low-level interface intended to mimic [do.call()]; for a
#' higher-level interface, see [invoke()].
#'
#' @param f <[`closure`] | [`primitive`]> function to call
#' @param args <[`promise_list`] | [`list`] | [`pairlist`]> list of arguments
#' to call the function with.
#' @template param-invoke-call_env
#' @template param-invoke-f_expr
#'
#' @details
#' This is intended as an alternative to [do.call()] that provides
#' better support for promises when calling [`closure`]s. In particular,
#' promises in `args` will be left as-is, allowing precise manipulation of the
#' expressions and environments of the arguments to `f` by using functions like
#' [promise()], [promises()], [capture()], [capture_all()], etc.
#'
#' @template details-invoke-f_expr
#'
#' @template returns-invoke
#'
#' @seealso [invoke()], the higher-level interface to [do_invoke()] intended
#' for normal use.
#'
#' @examples
#' # TODO
#'
#' @export
do_invoke = function(f, args, call_env = parent.frame(), f_expr = substitute(f)) {
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
      call = as.call(c(list(f_expr), arg_exprs))
      apply_closure(call, f, args, call_env)
    },
    special = {
      # for primitives / builtins we can at least ensure the calling frame
      # is the same frame the user called us from
      do.call(f, args, envir = call_env)
    },
    stop("`f` must be a function, not a ", typeof(f))
  )
}


# arg list helpers --------------------------------------------------------

#' Evaluate a list of argument expressions in an environment
#' @param arg_exprs <[`list`]> argument expressions. May contain `...`, which
#' will be turned into a list of arguments passed down from the surrounding
#' context. May also contain arguments with the name `...` pointing at lists,
#' which will be spliced into the argument list
#' @param env <[`environment`]> frame to evaluate arguments in
#' @noRd
eval_args = function(arg_exprs, env) {
  args = as.list(arg_exprs)

  # convert f(...) into f(... = <list of arguments from dots>) so it can be
  # spliced in by splice_dots()
  is_dots = vapply(args, identical, quote(...), FUN.VALUE = logical(1))
  if (any(is_dots)) {
    if (is.null(names(args))) names(args) = rep("", length(args))
    dots_i = which(is_dots)
    stopifnot(length(dots_i) == 1)
    args[[dots_i]] = capture_dots(env)
    names(args)[[dots_i]] = "..."
  }

  args[!is_dots] = lapply(args[!is_dots], eval, envir = env)

  splice_dots(args)
}

#' Splice `...` arguments into an argument list
#' @param args <[`list`]> argument list that may include any number of arguments
#' named `...` pointing to list-like objects.
#' @returns list of arguments where elements named `...` have had their contents
#' spliced into the list
#' @noRd
splice_dots = function(args) {
  if (any(names(args) == "...")) {
    arg_lists = lapply(seq_along(args), function(i) {
      name = names(args)[[i]]
      if (identical(name, "...")) {
        as.list(args[[i]])
      } else {
        setNames(list(args[[i]]), name)
      }
    })
    args = do.call(c, arg_lists)
  }

  args
}


# apply_closure -----------------------------------------------------------

#' Call a closure directly without wrapping arguments in [`promise`]s.
#' @param call <[`call`]> expression of the function call with its arguments.
#' This is not evaluated, but will be available in the closure as [sys.call()].
#' @param fun <[`closure`]> the function to call.
#' @param args <coercible to [`pairlist`]> list of arguments.
#' @param env <[`environment`]> environment to call the function from.
#' @noRd
apply_closure = function(call, fun, args, env) {
  apply_closure_(call, fun, as.pairlist(args), env)
}
