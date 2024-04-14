# default arguments ----------------------------------------------------------

test_that("is_default works", {
  x = DEFAULT()

  expect_true(is_default(x))
  expect_true(is_default(DEFAULT()))

  expect_true(is_default(promise(x)))
  expect_true(is_default(promise(DEFAULT())))
  expect_true(is_default(promise(uneval::DEFAULT())))

  f = function(x) capture(x)
  g = function(y) f(y)
  h = compiler::cmpfun(function(z) g(z))
  expect_true(is_default(h(x)))
  expect_true(is_default(h(DEFAULT())))
  expect_true(is_default(h(uneval::DEFAULT())))
})

test_that("DEFAULT arguments are detected correctly", {
  f = function(x = 1, y = 2, z = 3) list(x, y, z)
  f = autopartial(f)
  g = function(...) {
    gz = DEFAULT()
    f(z = gz, ...)
  }
  h = function(...) {
    g(y = DEFAULT(), ...)
  }

  expect_equal(g(), list(1, 2, 3))
  expect_equal(h(x = DEFAULT()), list(1, 2, 3))
})

test_that("DEFAULT works on arguments with default values", {
  foo = autopartial(function(x, a = 2) c(x, a))

  expect_equal(foo(a = DEFAULT())(1), c(1, 2))
  expect_equal(foo(1, a = DEFAULT()), c(1, 2))
  expect_equal(foo(a = DEFAULT())(x = 1), c(1, 2))
  expect_equal(foo(a = DEFAULT())(a = 4)(x = 1), c(1, 4))
  expect_equal(foo(a = 4)(a = DEFAULT())(x = 1), c(1, 4))

  foo = autopartial(function(x, y, a = 3, b = 4) c(x, y, a, b))

  expect_equal(foo(a = DEFAULT(), b = 5)(1)(y = -2, b = DEFAULT()), c(1, -2, 3, 5))
})

test_that("DEFAULT works on positional arguments", {
  foo = autopartial(function(x, y) c(x, y))

  expect_equal(foo(DEFAULT()), foo())
  expect_equal(foo(x = DEFAULT())(1), foo(1))
  expect_equal(foo(1, y = DEFAULT()), foo(1))
  expect_equal(foo(DEFAULT())(x = 1), foo(1))
  expect_equal(foo(x = DEFAULT())(x = 4)(y = 1), c(4, 1))
  expect_equal(foo(4)(x = DEFAULT())(y = 1), c(4, 1))
})

test_that("DEFAULT works on initial application", {
  f = function(x = 5) x
  expect_equal(autopartial(f, DEFAULT())(), 5)
  expect_equal(partial(f, DEFAULT())(), 5)
})
