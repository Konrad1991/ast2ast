library(tinytest)

# =============================================================================
# Zero-length vector creation -- integer(0) / logical(0L) / numeric(0.0)
# =============================================================================

f <- function() {
  a <- integer(0)
  return(a)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp() |> c(), integer(0))

f <- function() {
  a <- logical(0L)
  return(a)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp() |> c(), logical(0))

f <- function() {
  a <- numeric(0.0)
  return(a)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp() |> c(), numeric(0))

# =============================================================================
# a <- numeric(0), then grown by repeated c() calls
# =============================================================================

f <- function() {
  a <- numeric(0)
  a <- c(a, 1.0)
  a <- c(a, 2.0)
  a <- c(a, 3.0)
  return(a)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp() |> c(), c(1, 2, 3))
