library(tinytest)

# Inf/NaN/NA are built via c(); c() of only non-finite values was logical (#47)
f <- function(idx) {
  argtypes(idx |> type(int))
  if (idx == 1L) return(c(Inf, NaN, NA))
  if (idx == 2L) return(c(Inf))
  if (idx == 3L) return(-c(Inf))
  if (idx == 4L) return(c(1.0, -Inf))
  if (idx == 5L) return(c(-Inf, NaN))
  return(c(1.0, Inf, NaN, NA))
}
fcpp <- ast2ast::translate(f)
expect_identical(fcpp(1L), c(Inf, NaN, NA))
expect_identical(fcpp(2L), Inf)
expect_identical(fcpp(3L), -Inf)
expect_identical(fcpp(4L), c(1, -Inf))
expect_identical(fcpp(5L), c(-Inf, NaN))
expect_identical(fcpp(6L), c(1, Inf, NaN, NA))

# NA alone stays logical, as in R
g <- function() {
  return(c(NA, NA))
}
expect_identical(ast2ast::translate(g)(), c(NA, NA))

# a bare scalar Inf is still rejected
h <- function() {
  y <- -Inf
  return(y)
}
expect_error(ast2ast::translate(h, getsource = TRUE), pattern = "unallowed base type Inf")
