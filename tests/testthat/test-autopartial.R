test_that("partial function printing works", {
  add1 = function(x, ...) {
    x + 1
  }

  add1_auto = autopartial(add1)

  expect_output(print(add1_auto()), "<autopartial.*add1\\(\\)")
  expect_output(print(add1_auto(a = 2)), "<autopartial.*add1\\(a = 2\\)")
  expect_output(print(add1_auto(a = 2)(a = 3, b = 4)), "<autopartial.*add1\\(a = 3, b = 4\\)")
  expect_equal(add1_auto(a = 2)(a = 3, b = 4)(1), 2)
})

test_that("classic curry-style invocation works", {
  f = function(x, y, z, a = 4) list(x, y, z, a)
  f = autopartial(f)

  expect_equal(f(1, 2, 3), list(1, 2, 3, 4))
  expect_equal(f(1)(2)(3), list(1, 2, 3, 4))
  expect_equal(f(x = 1, a = 5, 2, 3), list(1, 2, 3, 5))
  expect_equal(f(x = 1)(a = 5, 2)(3), list(1, 2, 3, 5))
})

test_that("function bodies without braces work", {
  add1_auto_nobrace = autopartial(function(x, ...) x + 1)
  expect_equal(add1_auto_nobrace(3), 4)
})

test_that("functions without arguments work", {
  expect_equal(autopartial(function() 5)(), 5)
})

test_that("wrapper functions work", {
  f = function(x, y = 1, z = 2) {
    y + z
  }
  f = autopartial(f)
  g = function(..., y = 2) f(..., y = y)

  expect_output(print(g(y = 2)), "<autopartial.*f\\(y = y\\)")
  expect_equal(g(1), f(1, y = 2))
  expect_equal(g(1, y = 3, z = 4), f(1, y = 3, z = 4))
})

test_that("dots args are not prematurely evaluated", {
  f = function(x, y, z) substitute(y)
  f = autopartial(f)
  g = function(...) {
    gz = 3
    f(z = gz, ...)
  }
  h = function(...) {
    hy = 2
    g(y = {stop("should not be evaluated"); hy}, ...)
  }
  h(x = 1)

  expect_silent(h(x = 1))
  expect_equal(h(x = 1), quote({stop("should not be evaluated"); hy}))
})

test_that("dots args are forwarded correctly", {
  f = function(x, y, z) list(x, y, z)
  f = autopartial(f)
  g = function(...) {
    gz = 3
    f(z = gz, ...)
  }
  h = function(...) {
    hy = 2
    g(y = {print("evaluated"); hy}, ...)
  }

  expect_silent(h())
  expect_s3_class(h(), "uneval_autopartial")
  expect_output(
    expect_equal(h(x = 1), list(1, 2, 3)),
    "evaluated"
  )
})

test_that("waivers are detected correctly", {
  f = function(x = 1, y = 2, z = 3) list(x, y, z)
  f = autopartial(f)
  g = function(...) {
    gz = waiver()
    f(z = gz, ...)
  }
  h = function(...) {
    g(y = waiver(), ...)
  }

  expect_equal(g(), list(1, 2, 3))
  expect_equal(h(x = waiver()), list(1, 2, 3))
})


test_that("autopartial works on primitive functions", {
  l = autopartial(log)
  expect_equal(l(exp(1)), 1)
  expect_equal(l(base = 2)(2), 1)
})

# match.call() in autopartial() ------------------------------------------

test_that("match.call supports multiple partial applications", {
  foo = function(x, y, z) match.call()
  foo = autopartial(foo)

  expect_equal(foo(x = 1)(y = 2)(z = 3), quote(foo(x = 1, y = 2, z = 3)))
})

test_that("match.call() captures expressions, not evaluated values", {
  f = function(x, y, ...) match.call()
  f = autopartial(f)
  g = function(...) f(x = stop("x"), ...)
  h = function(...) g(y = stop("y"), ...)
  expect_equal(h(rnorm(10)), quote(f(x = stop("x"), y = stop("y"), rnorm(10))))
})

test_that("match.call works on unnamed underlying functions", {
  foo = autopartial(function(x, y, z) match.call())

  expect_equal(
    foo(x = 1)(y = 2)(z = 3),
    as.call(list(quote(function(x, y, z) match.call()), x = 1, y = 2, z = 3))
  )
})


# parent.frame() in autopartial() ----------------------------------------

test_that("parent.frame() captures calling environment", {
  f = function() parent.frame()
  e = new.env()
  expect_equal(with(e, autopartial(f)()), e)
})


# waivers -----------------------------------------------------------------

test_that("is_waiver works", {
  x = waiver()

  expect_true(is_waiver(x))
  expect_true(is_waiver(waiver()))

  expect_true(is_waiver(new_promise(quote(x))[[1]]))
  expect_true(is_waiver(new_promise(quote(waiver()))[[1]]))

  f = function(x) promise_list(x)
  g = function(y) f(y)
  h = compiler::cmpfun(function(z) g(z))
  expect_true(is_waiver(h(x)[[1]]))
  expect_true(is_waiver(h(waiver())[[1]]))
})

test_that("waivers work on arguments with default values", {
  foo = autopartial(function(x, a = 2) c(x, a))

  expect_equal(foo(a = waiver())(1), c(1, 2))
  expect_equal(foo(1, a = waiver()), c(1, 2))
  expect_equal(foo(a = waiver())(x = 1), c(1, 2))
  expect_equal(foo(a = waiver())(a = 4)(x = 1), c(1, 4))
  expect_equal(foo(a = 4)(a = waiver())(x = 1), c(1, 4))

  foo = autopartial(function(x, y, a = 3, b = 4) c(x, y, a, b))

  expect_equal(foo(a = waiver(), b = 5)(1)(y = -2, b = waiver()), c(1, -2, 3, 5))
})

test_that("waivers work on positional arguments", {
  foo = autopartial(function(x, y) c(x, y))

  expect_equal(foo(waiver()), foo())
  expect_equal(foo(x = waiver())(1), foo(1))
  expect_equal(foo(1, y = waiver()), foo(1))
  expect_equal(foo(waiver())(x = 1), foo(1))
  expect_equal(foo(x = waiver())(x = 4)(y = 1), c(4, 1))
  expect_equal(foo(4)(x = waiver())(y = 1), c(4, 1))
})

test_that("waivers work on initial application", {
  f = function(x = 5) x
  expect_equal(autopartial(f, waiver())(), 5)
  expect_equal(partial_(f, waiver())(), 5)
})

# promises ----------------------------------------------------------------

test_that("promise expressions are not retrieved as byte code", {
  f = function(...) {
    lapply(promise_list(...), promise_expr_)
  }
  f = autopartial(f)
  g = compiler::cmpfun(function(...) {
    gx = 5
    f(x = gx, ...)
  })
  expect_equal(g(), list(x = quote(gx)))
})