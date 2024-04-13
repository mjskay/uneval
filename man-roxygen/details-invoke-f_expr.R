#' When `f` is a [`closure`], the name of the function in the unevaluated call
#' provided to `f` via [sys.call()] can also be set via `f_expr`, making it
#' possible to avoid potentially-ugly captured function calls created by
#' [match.call()] in `f`.
#'
#' Currently, if `f` is a [`primitive`] function, [invoke()] falls back to using
#' [do.call()], so `f_expr` cannot be used to set the call expression seen by
#' [sys.call()] in `f`.
