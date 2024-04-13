# promises ----------------------------------------------------------------

test_that("promise expressions are not retrieved as byte code", {
  f = function(...) {
    lapply(promises(...), promise_expr_)
  }
  f = autopartial(f)
  g = compiler::cmpfun(function(...) {
    gx = 5
    f(x = gx, ...)
  })
  expect_equal(g(), list(x = quote(gx)))
})
