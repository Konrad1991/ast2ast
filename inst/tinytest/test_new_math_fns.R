library(tinytest)

# mean, round, sign, cumsum, colSums, rowSums, colMeans, rowMeans, sort, ifelse

# --- mean ----------------------------------------------------------------
f <- function(v) { argtypes(v |> type(vec(double))); return(mean(v)) }
fcpp <- ast2ast::translate(f)
x <- c(1.0, 2.0, 3.0, 4.0)
expect_equal(fcpp(x), mean(x))

# integer input -> double result, like R
f <- function(v) { argtypes(v |> type(vec(integer))); return(mean(v)) }
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(c(1L, 2L, 4L)), mean(c(1L, 2L, 4L)))

# matrix: mean over all elements
f <- function(m) { argtypes(m |> type(mat(double))); return(mean(m)) }
fcpp <- ast2ast::translate(f)
m <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
expect_equal(fcpp(m), mean(m))

# --- round -------------------------------------------------------------
f <- function(v) { argtypes(v |> type(vec(double))); return(round(v)) }
fcpp <- ast2ast::translate(f)
x <- c(-2.5, -0.5, 0.4, 0.5, 1.5, 2.5, 2.7)
expect_equal(c(fcpp(x)), round(x)) # R uses ties-to-even, so is nearbyint

# --- sign -------------------------------------------------------------
f <- function(v) { argtypes(v |> type(vec(double))); return(sign(v)) }
fcpp <- ast2ast::translate(f)
x <- c(-3.0, -0.0, 0.0, 2.5, -100.0)
expect_equal(c(fcpp(x)), sign(x))

# --- cumsum ---------------------------------------------------------------
f <- function(v) { argtypes(v |> type(vec(double))); return(cumsum(v)) }
fcpp <- ast2ast::translate(f)
x <- c(1.0, 3.0, -2.0, 5.0)
expect_equal(c(fcpp(x)), cumsum(x))

f <- function(v) { argtypes(v |> type(vec(integer))); return(cumsum(v)) }
fcpp <- ast2ast::translate(f)
expect_equal(c(fcpp(c(1L, 2L, 3L, 4L))), cumsum(c(1L, 2L, 3L, 4L)))

# --- colSums / rowSums / colMeans / rowMeans ----------------------------
f <- function(m) { argtypes(m |> type(mat(double))); return(colSums(m)) }
fcpp <- ast2ast::translate(f)
m <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
expect_equal(c(fcpp(m)), colSums(m))

f <- function(m) { argtypes(m |> type(mat(double))); return(rowSums(m)) }
fcpp <- ast2ast::translate(f)
expect_equal(c(fcpp(m)), rowSums(m))

f <- function(m) { argtypes(m |> type(mat(double))); return(colMeans(m)) }
fcpp <- ast2ast::translate(f)
expect_equal(c(fcpp(m)), colMeans(m))

f <- function(m) { argtypes(m |> type(mat(double))); return(rowMeans(m)) }
fcpp <- ast2ast::translate(f)
expect_equal(c(fcpp(m)), rowMeans(m))

# non-matrix argument is rejected at translate time
expect_error(
  ast2ast::translate(function(v) { argtypes(v |> type(vec(double))); return(colSums(v)) }),
  "matrix"
)

# --- sort --------------------------------------------------------------
f <- function(v) { argtypes(v |> type(vec(double))); return(sort(v)) }
fcpp <- ast2ast::translate(f)
x <- c(3.0, 1.0, 4.0, 1.5, 5.0, 9.0, 2.6)
expect_equal(c(fcpp(x)), sort(x))

# decreasing
f <- function(v) { argtypes(v |> type(vec(double))); return(sort(v, TRUE)) }
fcpp <- ast2ast::translate(f)
expect_equal(c(fcpp(x)), sort(x, decreasing = TRUE))

# NA is dropped, like R's default na.last = NA
f <- function(v) { argtypes(v |> type(vec(double))); return(sort(v)) }
fcpp <- ast2ast::translate(f)
xna <- c(3.0, NA, 1.0, NA, 2.0)
expect_equal(c(fcpp(xna)), sort(xna))

# sort does not support AD
expect_error(
  ast2ast::translate(
    function(v) { argtypes(v |> type(vec(double))); s <- sort(v); return(deriv(s, v)) },
    derivative = "reverse"
  ),
  "differ|reverse|forward|support"
)

# --- ifelse -----------------------------------------------------------
f <- function(v) {
  argtypes(v |> type(vec(double)))
  return(ifelse(v > 0.0, v, -v))
}
fcpp <- ast2ast::translate(f)
x <- c(-2.0, 3.0, -1.0, 0.0, 5.0)
expect_equal(c(fcpp(x)), ifelse(x > 0, x, -x))

# scalar yes / no branches
f <- function(v) {
  argtypes(v |> type(vec(double)))
  return(ifelse(v > 2.0, 1.0, 0.0))
}
fcpp <- ast2ast::translate(f)
expect_equal(c(fcpp(x)), ifelse(x > 2, 1, 0))

# reverse-mode AD flows through the taken branch:
# d/dx sum(ifelse(x > 0, x^2, x)) = ifelse(x > 0, 2x, 1)
g <- ast2ast::translate(
  function(x) {
    argtypes(x |> type(vec(double)))
    y <- sum(ifelse(x > 0.0, x^2, x))
    return(deriv(y, x))
  },
  derivative = "reverse"
)
xx <- c(-1.0, 2.0, -3.0, 0.5)
expect_equal(c(g(xx)), ifelse(xx > 0, 2 * xx, 1))

# --- mean / cumsum reverse-mode AD -------------------------------------
g <- ast2ast::translate(
  function(x) { argtypes(x |> type(vec(double))); y <- mean(x); return(deriv(y, x)) },
  derivative = "reverse"
)
expect_equal(c(g(c(1.0, 2.0, 3.0, 4.0))), rep(1 / 4, 4))

g <- ast2ast::translate(
  function(x) { argtypes(x |> type(vec(double))); y <- sum(cumsum(x)); return(deriv(y, x)) },
  derivative = "reverse"
)
# sum(cumsum(x)) = sum((n - i + 1) * x_i); gradient is n, n-1, ..., 1
expect_equal(c(g(c(1.0, 1.0, 1.0, 1.0))), c(4, 3, 2, 1))
