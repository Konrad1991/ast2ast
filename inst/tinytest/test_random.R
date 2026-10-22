library(tinytest)

# --- normal / uniform: identical to set.seed + rnorm / runif -------------------

f <- function(seed, n, mean, sd, min, max, test) {
  argtypes(seed |> type(int), n |> type(int), mean |> type(double), sd |> type(double),
           min |> type(double), max |> type(double), test |> type(int))
  e <- mersenne_twister(seed)
  if (test == 1L) {
    d <- normal(e)
    return(draw(d, n))
  }
  if (test == 2L) {
    d <- normal(e, mean, sd)
    a <- draw(d, n)
    b <- draw(d, n)
    return(c(a, b))
  }
  if (test == 3L) {
    u <- uniform(e)
    return(draw(u, n))
  }
  u <- uniform(e, min = min, max = max)
  return(draw(u, n))
}
fcpp <- ast2ast::translate(f)

set.seed(1)
expect_identical(fcpp(1L, 10L, 0, 1, 0, 1, 1L), rnorm(10))
set.seed(42)
expect_identical(fcpp(42L, 5L, 3, 0.5, 0, 1, 2L), c(rnorm(5, 3, 0.5), rnorm(5, 3, 0.5)))
set.seed(7)
expect_identical(fcpp(7L, 10L, 0, 1, 0, 1, 3L), runif(10))
set.seed(123)
expect_identical(fcpp(123L, 10L, 0, 1, -2, 5, 4L), runif(10, -2, 5))
set.seed(1)
expect_identical(fcpp(1L, 1000L, 0, 1, 0, 1, 1L), rnorm(1000))

# like R: n = 0 -> empty, sd = 0 -> mean, sd < 0 / min > max -> NaN
expect_identical(fcpp(1L, 0L, 0, 1, 0, 1, 1L), numeric(0))
expect_identical(fcpp(1L, 3L, 2, 0, 0, 1, 2L), rep(2, 6))
expect_true(all(is.nan(fcpp(1L, 3L, 0, -1, 0, 1, 2L))))
expect_true(all(is.nan(fcpp(1L, 3L, 0, 1, 2, 1, 4L))))

# --- each distribution owns a copy of the engine: same engine -> same stream ---

f <- function() {
  e <- mersenne_twister(3L)
  a <- normal(e)
  b <- normal(e)
  c(draw(a, 3L), draw(b, 3L))
}
fcpp <- ast2ast::translate(f)
set.seed(3)
expect_identical(fcpp(), rep(rnorm(3), 2))

# --- slots are readable ---------------------------------------------------------

f <- function() {
  e <- mersenne_twister(5L)
  d <- normal(e, 2, 3)
  u <- uniform(e, -1, 4)
  c(d$mean, d$sd, u$min, u$max, as.numeric(e$seed))
}
fcpp <- ast2ast::translate(f)
expect_identical(fcpp(), c(2, 3, -1, 4, 5))

# --- draw inside an inner function (ref) ---------------------------------------

f <- function() {
  g <- function(d) {
    argtypes(d |> type(normal) |> ref())
    returntype(vec(double))
    draw(d, 2L)
  }
  d <- normal(mersenne_twister(9L))
  a <- g(d)
  b <- g(d)
  c(a, b)
}
fcpp <- ast2ast::translate(f)
set.seed(9)
expect_identical(fcpp(), rnorm(4))

# by value: warning, the caller repeats the draws
f <- function() {
  g <- function(d) {
    argtypes(d |> type(normal))
    returntype(vec(double))
    draw(d, 2L)
  }
  d <- normal(mersenne_twister(9L))
  c(g(d), g(d))
}
expect_warning(fcpp <- ast2ast::translate(f), pattern = "normal is copied, so its draws are repeated by the caller")
set.seed(9)
expect_identical(fcpp(), rep(rnorm(2), 2))

# --- user types cannot reuse the names of built-in types ------------------------

f <- function() {
  1
}
types <- function() {
  new_type(normal, slots(a |> type(double)))
}
expect_error(ast2ast::translate(f, types), pattern = "Invalid type name: is a built-in type --> normal")
types <- function() {
  new_layout(uniroot_result, double, names = c("a", "b"))
}
expect_error(ast2ast::translate(f, types), pattern = "Invalid layout name: is a built-in type --> uniroot_result")

# --- usable in derivative translations ----------------------------------------

f <- function(x) {
  argtypes(x |> type(vec(double)))
  d <- normal(mersenne_twister(1L))
  x + draw(d, length(x))
}
fcpp <- ast2ast::translate(f, derivative = "forward")
set.seed(1)
expect_equal(fcpp(c(1, 2)), c(1, 2) + rnorm(2))

# --- errors ---------------------------------------------------------------------

f <- function() {
  d <- normal(mersenne_twister(1L))
  d$mean <- 2
  draw(d, 1L)
}
expect_error(ast2ast::translate(f), pattern = "The fields of normal are read-only")

f <- function(d) {
  argtypes(d |> type(normal))
  draw(d, 1L)
}
expect_error(ast2ast::translate(f), pattern = "normal cannot be passed from R")

f <- function() {
  normal(mersenne_twister(1L))
}
expect_error(ast2ast::translate(f), pattern = "normal cannot be returned to R")

f <- function() {
  g <- function(d) {
    argtypes(d |> type(normal) |> const())
    returntype(vec(double))
    draw(d, 2L)
  }
  g(normal(mersenne_twister(1L)))
}
expect_error(ast2ast::translate(f), pattern = "draw: dist cannot be const")

f <- function() {
  draw(mersenne_twister(1L), 2L)
}
expect_error(ast2ast::translate(f), pattern = "draw: dist has to be created by normal\\(\\) or uniform\\(\\)")

f <- function() {
  draw(normal(mersenne_twister(1L)), c(1L, 2L))
}
expect_error(ast2ast::translate(f), pattern = "draw: n has to be a scalar integer")

f <- function() {
  d <- normal(1L)
  draw(d, 1L)
}
expect_error(ast2ast::translate(f), pattern = "normal: engine has to be created by mersenne_twister\\(\\)")

f <- function() {
  d <- normal(mersenne_twister(1L), sd = c(1, 2))
  draw(d, 1L)
}
expect_error(ast2ast::translate(f), pattern = "normal: sd has to be a scalar double")
