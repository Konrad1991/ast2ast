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

# --- sample / sample_int: identical to set.seed + sample / sample.int -----------

f <- function(seed, x, size, replace, prob, test) {
  argtypes(seed |> type(int), x |> type(vec(double)), size |> type(int),
           replace |> type(logical), prob |> type(vec(double)), test |> type(int))
  e <- mersenne_twister(seed)
  if (test == 1L) return(sample(e, x))
  if (test == 2L) return(sample(e, x, size, replace))
  if (test == 3L) return(sample(e, x, size, replace, prob))
  if (test == 4L) return(sample(e, x, prob = prob))
  if (test == 5L) return(as.numeric(sample_int(e, size)))
  return(as.numeric(sample_int(e, length(prob), size, replace, prob)))
}
fcpp <- ast2ast::translate(f)
x <- c(1.5, 2.5, 3.5, 4.5, 5.5)
p <- c(1, 1, 2, 2, 1)
set.seed(3)
expect_identical(fcpp(3L, x, 0L, FALSE, p, 1L), sample(x))
set.seed(4)
expect_identical(fcpp(4L, x, 12L, TRUE, p, 2L), sample(x, 12, TRUE))
set.seed(5)
expect_identical(fcpp(5L, x, 3L, FALSE, p, 2L), sample(x, 3))
# prob: with replace (cumulative), without replace, ties in prob
set.seed(6)
expect_identical(fcpp(6L, x, 10L, TRUE, p, 3L), sample(x, 10, TRUE, p))
set.seed(7)
expect_identical(fcpp(7L, x, 4L, FALSE, p, 3L), sample(x, 4, FALSE, p))
set.seed(8)
expect_identical(fcpp(8L, x, 0L, FALSE, p, 4L), sample(x, prob = p))
# prob with replace and more than 200 relevant entries: Walker's alias method
p300 <- 1 + (0:299) %% 7
set.seed(9)
expect_identical(fcpp(9L, x, 50L, TRUE, p300, 6L), as.numeric(sample.int(300, 50, TRUE, p300)))
set.seed(10)
expect_identical(fcpp(10L, x, 20L, FALSE, p, 5L), as.numeric(sample.int(20)))
# like R: a single number x >= 1 means 1:x
set.seed(11)
expect_identical(fcpp(11L, 7, 0L, FALSE, p, 1L), as.numeric(sample(7)))
set.seed(12)
expect_identical(fcpp(12L, 0.5, 3L, TRUE, p, 2L), sample(0.5, 3, TRUE))

f <- function() {
  e <- mersenne_twister(13L)
  sample(e, 1L:10L, 4L)
}
fcpp <- ast2ast::translate(f)
set.seed(13)
expect_identical(fcpp(), sample(1:10, 4))

f <- function(x) {
  argtypes(x |> type(vec(logical)))
  e <- mersenne_twister(14L)
  sample(e, x)
}
fcpp <- ast2ast::translate(f)
set.seed(14)
expect_identical(fcpp(c(TRUE, FALSE, FALSE)), sample(c(TRUE, FALSE, FALSE)))

# runtime errors like R
f <- function(n, size) {
  argtypes(n |> type(int), size |> type(int))
  e <- mersenne_twister(1L)
  sample_int(e, n, size)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(3L, 5L), pattern = "cannot take a sample larger than the population when 'replace = FALSE'")
expect_error(fcpp(3L, -1L), pattern = "invalid 'size' argument")
f <- function(p) {
  argtypes(p |> type(vec(double)))
  e <- mersenne_twister(1L)
  sample_int(e, 3L, 2L, prob = p)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(c(1, 2)), pattern = "incorrect number of probabilities")
expect_error(fcpp(c(1, -1, 2)), pattern = "negative probability")
expect_error(fcpp(c(1, NA, 2)), pattern = "NA in probability vector")
expect_error(fcpp(c(1, 0, 0)), pattern = "too few positive probabilities")

# translation errors
f <- function() {
  sample(normal(mersenne_twister(1L)), 1:3)
}
expect_error(ast2ast::translate(f), pattern = "sample: engine has to be created by mersenne_twister\\(\\)")
f <- function() {
  e <- mersenne_twister(1L)
  sample_int(e, c(1L, 2L))
}
expect_error(ast2ast::translate(f), pattern = "sample_int: n has to be a scalar integer")
f <- function() {
  e <- mersenne_twister(1L)
  sample(e, 1:3, prob = c(TRUE, FALSE, TRUE))
}
expect_error(ast2ast::translate(f), pattern = "sample: prob has to be a double vector")
