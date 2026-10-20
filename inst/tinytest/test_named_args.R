library(tinytest)

# named arguments are matched like R (#48)
err_of <- function(f) {
  e <- try(ast2ast::translate(f, getsource = TRUE), silent = TRUE)
  expect_true(inherits(e, "try-error"))
  attributes(e)[["condition"]]$message
}
has <- function(m, pat) expect_true(grepl(pat, m, fixed = TRUE), info = m)

# --- examples from the issue --------------------------------------------
f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(ifelse(x > 1, no = 0, yes = 1))
}
expect_equal(ast2ast::translate(f)(c(1, 2, 3)), c(0, 1, 1))

f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(rep(x, each = 2))
}
has(err_of(f), "Unknown argument 'each' for rep")

f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(rep(x, length.out = 5L))
}
has(err_of(f), "Unknown argument 'length.out' for rep")

f <- function(a) {
  argtypes(a |> type(double))
  g <- function(x) {
    argtypes(x |> type(double))
    returntype(double)
    return(x^2 - 2)
  }
  r <- uniroot(g, c(0, a), maxiter = 30L, tol = 1e-12)
  return(r$root)
}
expect_equal(ast2ast::translate(f)(2), sqrt(2), tolerance = 1e-8)

f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(log(x, base = 2))
}
has(err_of(f), "Unknown argument 'base' for log")

f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(round(x, digits = 2L))
}
has(err_of(f), "Unknown argument 'digits' for round")

# --- reordering ------------------------------------------------------------
f <- function(x) {
  argtypes(x |> type(vec(double)))
  a <- rep(times = 2L, x = x)
  b <- sort(decreasing = TRUE, x = x)
  m <- matrix(ncol = 2L, nrow = 3L, data = a)
  v <- vector(length = 2L, mode = "numeric")
  s <- seq_len(length.out = 2L)
  return(c(a, b, m[3L, 2L], v, s))
}
x <- c(3, 1, 2)
expect_equal(ast2ast::translate(f)(x),
  c(rep(x, 2), sort(x, decreasing = TRUE), matrix(rep(x, 2), 3, 2)[3, 2], numeric(2), 1:2))

# --- errors ------------------------------------------------------------------
f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(matrix(x, ncol = 3L))
}
has(err_of(f), "Argument 'nrow' of matrix is missing, but a later argument is given")

f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(sort(x, decreasin = TRUE))
}
has(err_of(f), "Unknown argument 'decreasin' for sort. Did you mean 'decreasing'?")

f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(sort(x = x, x = x))
}
has(err_of(f), "Argument 'x' of sort is given twice")

f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(sum(x, na.rm = TRUE))
}
has(err_of(f), "sum does not take named arguments")

# --- inner functions -------------------------------------------------------
f <- function(a, b) {
  argtypes(a |> type(double), b |> type(double))
  g <- function(x, y) {
    argtypes(x |> type(double), y |> type(double))
    returntype(double)
    return(x - y)
  }
  return(g(y = a, x = b))
}
expect_equal(ast2ast::translate(f)(1, 5), 4)

f <- function(a) {
  argtypes(a |> type(double))
  g <- function(x, y) {
    argtypes(x |> type(double), y |> type(double))
    returntype(double)
    return(x - y)
  }
  return(g(a, z = a))
}
has(err_of(f), "Unknown argument 'z' for g")
