library(tinytest)

# zero-length arguments (#51)
f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(length(x))
}
expect_equal(ast2ast::translate(f)(numeric(0)), 0)

f <- function(x) {
  argtypes(x |> type(vec(int)))
  return(x)
}
expect_identical(ast2ast::translate(f)(integer(0)), integer(0))

f <- function(x) {
  argtypes(x |> type(borrow_vec(double)))
  return(length(x))
}
expect_equal(ast2ast::translate(f)(numeric(0)), 0)

f <- function(m) {
  argtypes(m |> type(mat(double)))
  return(ncol(m))
}
expect_equal(ast2ast::translate(f)(matrix(0, 0, 3)), 3)

# empty logical subset
f <- function(x) {
  argtypes(x |> type(vec(double)))
  y <- x[x > 10]
  return(sum(y))
}
expect_equal(ast2ast::translate(f)(c(1, 2)), 0)

f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(x[x > 10])
}
expect_identical(ast2ast::translate(f)(c(1, 2)), numeric(0))

# empty selection replaces nothing, whatever the rhs length (as in R)
f <- function(x) {
  argtypes(x |> type(vec(double)))
  y <- x
  y[y > 10] <- c(5, 6)
  y[which(y > 10)] <- 7
  return(y)
}
expect_identical(ast2ast::translate(f)(c(1, 2, 3)), c(1, 2, 3))

# empty rows of a matrix
f <- function(m) {
  argtypes(m |> type(mat(double)))
  return(m[which(m[, 1L] > 10), ])
}
expect_identical(dim(ast2ast::translate(f)(matrix(1:6 + 0, 2))), c(0L, 3L))

# max/min of an empty double vector: -Inf/Inf with a warning
f <- function(x, use_max) {
  argtypes(x |> type(vec(double)), use_max |> type(logical))
  y <- x[x > 10]
  if (use_max) return(max(y))
  return(min(y))
}
fcpp <- ast2ast::translate(f)
expect_warning(res <- fcpp(c(1, 2), TRUE), "no non-missing arguments to max; returning -Inf", fixed = TRUE)
expect_identical(res, -Inf)
expect_warning(res <- fcpp(c(1, 2), FALSE), "no non-missing arguments to min; returning Inf", fixed = TRUE)
expect_identical(res, Inf)

# elementwise operations on an empty vector
f <- function(x) {
  argtypes(x |> type(vec(double)))
  y <- x[x > 4]
  return(rev(sin(y * 2 + 1)))
}
expect_identical(ast2ast::translate(f)(c(1, 2, 3)), numeric(0))

# if () on an empty vector
f <- function(x) {
  argtypes(x |> type(vec(double)))
  if (x) return(1.0)
  return(0.0)
}
expect_error(ast2ast::translate(f)(numeric(0)), "argument is of length zero")

# scalar argument with length 0 names the argument
f <- function(a) {
  argtypes(a |> type(double))
  return(a)
}
expect_error(ast2ast::translate(f)(numeric(0)), "Argument 'a' must be a scalar double, got length 0", fixed = TRUE)
expect_error(ast2ast::translate(f)(c(1, 2)), "Argument 'a' must be a scalar double, got length 2", fixed = TRUE)
