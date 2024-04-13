#include <Rcpp.h>
using namespace Rcpp;

/**
 * Unwrap an SEXP that may be a promise to a promise to a promise (etc) into a
 * single promise. Non-promises are left as-is.
 * @param x an SEXP that might be a promise
 * @returns
 * - If `x` is a promise: a promise whose code is not a promise
 * - If `x` is not a promise: `x`
 */
SEXP unwrap_promise(SEXP x) {
  RObject expr = x;
  while (TYPEOF(expr) == PROMSXP) {
    x = expr;
    expr = PREXPR(x);
  }
  return x;
}

/**
 * Find a promise by name
 * @param name name of a variable
 * @param env environment to search
 * @returns an unwrapped promise (if `name` refers to a promise) or an object
 * (if `name` does not refer to a promise).
 */
// [[Rcpp::export]]
SEXP find_promise(Symbol name, Environment env) {
  RObject var = Rf_findVar(name, env);
  return unwrap_promise(var);
}

// [[Rcpp::export]]
SEXP promise_expr_(Promise promise) {
  promise = unwrap_promise(promise);
  return PREXPR(promise);
}

// [[Rcpp::export]]
SEXP promise_env_(Promise promise) {
  promise = unwrap_promise(promise);
  return PRENV(promise);
}

// identical(x, quote(waiver()))
bool is_waiver_call(SEXP x) {
  if (TYPEOF(x) == LANGSXP) {
    Language call = x;
    if (call.size() == 1 && TYPEOF(call[0]) == SYMSXP) {
      Symbol symbol = call[0];
      return symbol == "waiver";
    }
  }

  return false;
}

// [[Rcpp::export]]
bool is_waiver_(RObject x) {
  if (TYPEOF(x) == PROMSXP) {
    x = unwrap_promise(x);
    RObject expr = PREXPR(x);

    if (TYPEOF(expr) == SYMSXP) {
      // TODO: should this be PRVALUE?
      Environment env = PRENV(x);
      x = Rcpp_eval(expr, env);
    } else {
      x = expr;
    }
  }

  return is_waiver_call(x) || Rf_inherits(x, "waiver");
}

/**
 * Convert a dotted pairlist to a `list()`
 * @param dots a dotted pair list as passed to an R function via `...`
 * @returns the values of `dots` converted to a list of promises. Nested
 * promises are unwrapped so that they are only a single promise.
 */
// [[Rcpp::export]]
List dots_to_list_(DottedPair dots) {
  int n = dots.size();
  List list(n);

  list.names() = dots.attr("names");

  for (int i = 0; i < n; i++) {
    list[i] = unwrap_promise(dots[0]);
    dots.remove(0);
  }

  return list;
}
