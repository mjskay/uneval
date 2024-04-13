#include <Rcpp.h>
#include "promise.h"
using namespace Rcpp;

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
