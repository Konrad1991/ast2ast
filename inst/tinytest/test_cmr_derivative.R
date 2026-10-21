library(tinytest)

# Skipped on CRAN: AD compiles are expensive.
if (!at_home()) exit_file("slow: AD compiles; runs at home / CI only")

# x[[1]] = t, x[2:6] = knot values; knot times fixed
value <- function(x) {
  argtypes(x |> type(vec(double)))
  returntype(double)
  tk <- c(0, 1, 2, 3, 4)
  cmr(x[[1L]], tk, x[2L:6L])
}
value_cpp <- ast2ast::translate(value)

finite_differences <- function(x, h = 1e-6) {
  vapply(seq_along(x), function(i) {
    xp <- x
    xm <- x
    xp[[i]] <- xp[[i]] + h
    xm[[i]] <- xm[[i]] - h
    (value_cpp(xp) - value_cpp(xm)) / (2 * h)
  }, numeric(1L))
}

jac <- function(x) {
  argtypes(x |> type(vec(double)))
  rhs <- function(x) {
    argtypes(x |> type(vec(double)))
    returntype(vec(double))
    tk <- c(0, 1, 2, 3, 4)
    out <- numeric(1L)
    out[[1L]] <- cmr(x[[1L]], tk, x[2L:6L])
    out
  }
  jacobian(rhs, x)
}
jac_forward_cpp <- ast2ast::translate(jac, derivative = "forward")
jac_reverse_cpp <- ast2ast::translate(jac, derivative = "reverse")

reverse <- function(y, x) {
  argtypes(y |> type(vec(double)), x |> type(vec(double)))
  tk <- c(0, 1, 2, 3, 4)
  y[[1L]] <- cmr(x[[1L]], tk, x[2L:6L])
  return(deriv(y, x))
}
reverse_cpp <- ast2ast::translate(reverse, derivative = "reverse")

y_knots <- c(1.0, 3.0, 2.0, 5.0, 4.0)
# interior segment, left ghost point, right ghost point
for (t in c(1.3, 0.4, 3.6)) {
  x <- c(t, y_knots)
  fd <- finite_differences(x)
  info <- paste("t =", t)
  expect_equal(c(jac_forward_cpp(x)), fd, tolerance = 1e-6, info = info)
  expect_equal(c(jac_reverse_cpp(x)), fd, tolerance = 1e-6, info = info)
  expect_equal(c(reverse_cpp(0, x)), fd, tolerance = 1e-6, info = info)
}

# clamped outside the knots: the first / last knot value, no dependence on t
expect_equal(c(jac_forward_cpp(c(-1, y_knots))), c(0, 1, 0, 0, 0, 0))
expect_equal(c(jac_reverse_cpp(c(-1, y_knots))), c(0, 1, 0, 0, 0, 0))
expect_equal(c(jac_forward_cpp(c(5, y_knots))), c(0, 0, 0, 0, 0, 1))
expect_equal(c(jac_reverse_cpp(c(5, y_knots))), c(0, 0, 0, 0, 0, 1))
expect_equal(c(reverse_cpp(0, c(-1, y_knots))), c(0, 1, 0, 0, 0, 0))
