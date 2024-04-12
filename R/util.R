`%||%` = function (x, y) {
  if (is.null(x)) y else x
}

cat0 = function(...) {
  cat(..., sep = "")
}

# needed instead of deparse1() for R < 4.0
deparse0 = function (expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}
