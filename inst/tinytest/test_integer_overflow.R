library(tinytest)
if (!at_home()) exit_file("slow: integer overflow compiles; runs at home / CI only")

# integer overflow gives NA with a warning, as in R (#49)
f <- function(a, b, op) {
  argtypes(a |> type(int), b |> type(int), op |> type(int))
  if (op == 1L) return(a + b)
  if (op == 2L) return(a - b)
  return(a * b)
}
fcpp <- ast2ast::translate(f)
max_int <- .Machine$integer.max
expect_warning(res <- fcpp(max_int, 1L, 1L), "NAs produced by integer overflow")
expect_identical(res, NA_integer_)
expect_warning(res <- fcpp(-max_int, 1L, 2L), "NAs produced by integer overflow")
expect_identical(res, NA_integer_)
expect_warning(res <- fcpp(100000L, 100000L, 3L), "NAs produced by integer overflow")
expect_identical(res, NA_integer_)
expect_silent(res <- fcpp(max_int - 1L, 1L, 1L))
expect_identical(res, max_int)
# INT_MIN is NA_integer_ in R
expect_warning(res <- fcpp(-max_int, -1L, 1L), "NAs produced by integer overflow")
expect_identical(res, NA_integer_)

# accumulation inside a loop
g <- function(n) {
  argtypes(n |> type(int))
  acc <- 1L
  for (i in 1L:n) {
    acc <- acc * 10L
  }
  return(acc)
}
gcpp <- ast2ast::translate(g)
expect_identical(gcpp(9L), 1000000000L)
expect_warning(res <- gcpp(10L), "NAs produced by integer overflow")
expect_identical(res, NA_integer_)

# vectors, the example from the issue
h <- function(a, v) {
  argtypes(a |> type(int), v |> type(vec(int)))
  return(c(a + a, (v * v)[[1L]]))
}
expect_warning(res <- ast2ast::translate(h)(max_int, c(100000L, 1L)), "NAs produced by integer overflow")
expect_identical(res, c(NA_integer_, NA_integer_))

# sum: R returns a double on overflow, ast2ast NA with R's old warning
s <- function(x) {
  argtypes(x |> type(vec(int)))
  return(sum(x))
}
scpp <- ast2ast::translate(s)
expect_warning(res <- scpp(c(max_int, 1L)), "integer overflow - use sum\\(as.numeric\\(.\\)\\)")
expect_identical(res, NA_integer_)
expect_identical(scpp(c(max_int, 1L, -2L)), max_int - 1L)
expect_identical(scpp(c(1L, NA_integer_)), NA_integer_)

# integer %/% and %% follow R's floor semantics for every sign combination
d <- function(x, y) {
  argtypes(x |> type(vec(int)), y |> type(vec(int)))
  return(c(x %/% y, x %% y))
}
dcpp <- ast2ast::translate(d)
x <- c(7L, -7L, 7L, -7L, 6L, -6L, 0L, 1L, max_int, -max_int, NA_integer_, 5L)
y <- c(2L, 2L, -2L, -2L, 3L, -3L, 5L, 7L, 2L, 3L, 2L, 0L)
expect_identical(dcpp(x, y), c(x %/% y, x %% y))
