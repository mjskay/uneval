#include <Rcpp.h>
#include "promise.h"
using namespace Rcpp;

/**
 * Convert a dotted pairlist to a `list()`
 * @param dots a dotted pair list as passed to an R function via `...`
 * @returns the values of `dots` converted to a list of promises. Nested
 * promises are unwrapped so that they are only a single promise.
 */
// [[Rcpp::export]]
List dots_to_list(DottedPair dots) {
  int n = dots.size();
  List list(n);

  list.names() = dots.attr("names");

  for (int i = 0; i < n; i++) {
    list[i] = unwrap_promise(dots[0]);
    dots.remove(0);
  }

  return list;
}
