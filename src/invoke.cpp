#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP apply_closure_(Language call, RObject fun, DottedPair args, Environment env) {
  if (TYPEOF(fun) != CLOSXP) {
    const char* fmt = "`fun` must be a closure: [type=%s; target=CLOSXP].";
    throw not_compatible(fmt, Rf_type2char(TYPEOF(fun)));
  }
  return Rf_applyClosure(call, fun, args, env, R_NilValue);
}
