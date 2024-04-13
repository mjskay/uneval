#include <Rcpp.h>
#include "promise.h"
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
