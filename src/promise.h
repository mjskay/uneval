#pragma once
#include <Rcpp.h>

SEXP unwrap_promise(SEXP x);
SEXP find_promise(Rcpp::Symbol name, Rcpp::Environment env);
SEXP promise_expr_(Rcpp::Promise promise);
SEXP promise_env_(Rcpp::Promise promise);
