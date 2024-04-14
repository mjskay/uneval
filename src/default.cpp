#include <Rcpp.h>
#include "promise.h"
using namespace Rcpp;

// identical(x, quote(DEFAULT())) || identical(x, quote(uneval::DEFAULT()))
bool is_default_call(SEXP x) {
  if (TYPEOF(x) == LANGSXP) {
    Language call = x;
    if (call.size() == 1) {
      if (TYPEOF(call[0]) == SYMSXP) {
        Symbol symbol = call[0];
        return symbol == "DEFAULT";
      } else if (TYPEOF(call[0]) == LANGSXP) {
        Language f = call[0];
        if (f.size() == 3 && TYPEOF(f[0]) == SYMSXP && TYPEOF(f[1]) == SYMSXP && TYPEOF(f[2]) == SYMSXP) {
          Symbol f_call = f[0];
          Symbol f_pkg = f[1];
          Symbol f_fun = f[2];
          return f_call == "::" && f_pkg == "uneval" && f_fun == "DEFAULT";
        }
      }
    }
  }

  return false;
}

// [[Rcpp::export]]
bool is_default_(RObject x) {
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

  return is_default_call(x) || Rf_inherits(x, "uneval_default");
}
