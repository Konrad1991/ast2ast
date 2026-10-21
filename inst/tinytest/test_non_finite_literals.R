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

# as.logical(NaN) is NA, as in R
f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(as.logical(x))
}
x <- c(NaN, 0, 2, NA, -Inf)
expect_identical(ast2ast::translate(f)(x), as.logical(x))
g <- function(x) {
  argtypes(x |> type(double))
  return(as.logical(x))
}
expect_identical(ast2ast::translate(g)(NaN), NA)

# scalar double args accept NA/NaN, as in R
h <- function(x) {
  argtypes(x |> type(double))
  return(x)
}
hcpp <- ast2ast::translate(h)
expect_identical(hcpp(NaN), NaN)
expect_identical(hcpp(NA_real_), NA_real_)

# scalar logical/integer args accept NA, as in R
m <- function(b, i) {
  argtypes(b |> type(logical), i |> type(int))
  if (is.na(b)) return(i + 1L)
  return(i)
}
mcpp <- ast2ast::translate(m)
expect_identical(mcpp(NA, 1L), 2L)
expect_identical(mcpp(TRUE, NA_integer_), NA_integer_)
expect_identical(mcpp(NA, NA_integer_), NA_integer_)
n <- function(b) {
  argtypes(b |> type(logical))
  if (b) return(1L)
  return(0L)
}
expect_error(ast2ast::translate(n)(NA), "missing value where TRUE/FALSE needed")

# if () on a length-1 vector errors on NA/NaN, as in R
k <- function(x) {
  argtypes(x |> type(vec(double)))
  if (x) return(1.0)
  return(0.0)
}
kcpp <- ast2ast::translate(k)
expect_error(kcpp(NaN), "argument is not interpretable as logical")
expect_error(kcpp(NA_real_), "missing value where TRUE/FALSE needed")
expect_identical(kcpp(2), 1)

# is.na(NaN) is TRUE, as in R
p <- function(x, s) {
  argtypes(x |> type(vec(double)), s |> type(double))
  returntype(vec(logical))
  r <- is.na(x)
  r[[1L]] <- is.na(s)
  r
}
pcpp <- ast2ast::translate(p)
expect_identical(pcpp(c(1, NA, NaN), NaN), c(TRUE, TRUE, TRUE))
expect_identical(pcpp(c(1, NA, NaN), 1), c(FALSE, TRUE, TRUE))

# & and | with NA use R's truth table
q <- function(b, x) {
  argtypes(b |> type(vec(logical)), x |> type(vec(double)))
  returntype(vec(logical))
  r <- logical(6L)
  r[[1L]] <- b[[1L]] & FALSE
  r[[2L]] <- b[[1L]] | TRUE
  r[[3L]] <- b[[1L]] & TRUE
  r[[4L]] <- b[[1L]] | FALSE
  r[[5L]] <- x[[1L]] & 0
  r[[6L]] <- x[[1L]] | 1
  r
}
qcpp <- ast2ast::translate(q)
expect_identical(qcpp(NA, NaN), c(FALSE, TRUE, NA, NA, FALSE, TRUE))
expect_identical(qcpp(TRUE, 2), c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
