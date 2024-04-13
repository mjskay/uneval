test_that("basic invocation works", {
  f = function(...) match.call()

  y = 2
  z = 3
  f(1 + 2, x = y + z)

  expect_equal(invoke(f(promise(1 + 2), x = promise(y + z))), quote(f(1 + 2, x = y + z)))
  expect_equal(invoke(f(1 + 2, x = y + z)), quote(f(3, x = 5)))

  g = function(x, ...) invoke(f(1 + 2, x = capture(x), ...))
  expect_equal(g(x = y + z, i = j), quote(f(3, x = y + z, i = j)))
  expect_equal(
    invoke(f(1 + 2, ... = list(x = y + z, i = promise(j)), m = 8)),
    quote(f(3, x = 5, i = j, m = 8))
  )
})
