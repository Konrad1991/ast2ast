library(tinytest)

# --- uniroot -----------------------------------------------------------

f <- function(interval) {
  g <- fn(
    args_f = function(x) x |> type(double),
    return_value = type(double),
    block = function(x) return(x^2 - 4)
  )
  res <- uniroot(g, interval, 1e-10, 1000)
  return(res$root)
}
fcpp <- ast2ast::translate(f, args_f = function(interval) interval |> type(vec(double)))

check_uniroot_case <- function(interval, info = "") {
  root_etr <- fcpp(interval)
  root_ref <- stats::uniroot(function(x) x^2 - 4, interval, tol = 1e-10, maxiter = 1000)$root
  expect_equal(root_etr, root_ref, tolerance = 1e-6, info = info)
}

# case 1: positive root
check_uniroot_case(c(0, 10), "case1")

# case 2: negative root
check_uniroot_case(c(-10, 0), "case2")

# case 3: root close to the lower bound
check_uniroot_case(c(1.9, 10), "case3")

# case 4: root close to the upper bound
check_uniroot_case(c(-10, -1.9), "case4")

# printing a uniroot_result
f_print <- function(interval) {
  g <- fn(
    args_f = function(x) x |> type(double),
    return_value = type(double),
    block = function(x) return(x^2 - 4)
  )
  res <- uniroot(g, interval, 1e-10, 1000)
  print(res)
  return(res$root)
}
fcpp_print <- ast2ast::translate(f_print, args_f = function(interval) interval |> type(vec(double)))
out <- capture.output(fcpp_print(c(0, 10)))
expect_true(any(grepl("uniroot_result", out)))
expect_true(any(grepl("root:", out)))
expect_true(any(grepl("f_root:", out)))
expect_true(any(grepl("iter:", out)))
expect_true(any(grepl("estim_prec:", out)))

# maxiter accepts an integer literal too, not just double
f_int_maxiter <- function(interval) {
  g <- fn(
    args_f = function(x) x |> type(double),
    return_value = type(double),
    block = function(x) return(x^2 - 4)
  )
  res <- uniroot(g, interval, 1e-10, 1000L)
  return(res$root)
}
fcpp_int_maxiter <- ast2ast::translate(f_int_maxiter, args_f = function(interval) interval |> type(vec(double)))
expect_equal(fcpp_int_maxiter(c(0, 10)), stats::uniroot(function(x) x^2 - 4, c(0, 10), tol = 1e-10, maxiter = 1000)$root, tolerance = 1e-6)

# --- nnls ----------------------------------------------------------------

if (!requireNamespace("nnls", quietly = TRUE)) {
  exit_file("nnls package not installed")
}

f_nnls <- function(A, b) {
  x <- nnls(A, b)
  return(x)
}
f_nnls_args <- function(A, b) {
  A |> type(matrix(double))
  b |> type(vec(double))
}
fcpp_nnls <- ast2ast::translate(f_nnls, f_nnls_args)

check_nnls_case <- function(A, b, info = "") {
  x_etr <- fcpp_nnls(A, b) |> c()
  attr(x_etr, "dim") <- NULL
  x_ref <- nnls::nnls(A, b)$x
  expect_equal(x_etr, x_ref, tolerance = 1e-6, info = info)
}

# case 1: unconstrained solution already feasible, no active bound
A <- matrix(c(1, 1, 0, 0, 1, 1), nrow = 3, ncol = 2)
b <- c(2, 1, 1)
check_nnls_case(A, b, "case1")

# case 2: unconstrained solution negative -> clamped to 0
A <- matrix(c(1, 1), nrow = 2, ncol = 1)
b <- c(-1, -1)
check_nnls_case(A, b, "case2")

# case 3: mixed -- one variable clamped, one free
A <- matrix(c(1, 0, 0, 0, 1, 1), nrow = 3, ncol = 2)
b <- c(1, -1, -1)
check_nnls_case(A, b, "case3")

# case 4: random overdetermined problem
set.seed(1)
A <- matrix(rnorm(30), nrow = 10, ncol = 3)
b <- rnorm(10)
check_nnls_case(A, b, "case4")

# --- which -----------------------------------------------------------------

f <- function(v) {
  return(which(v > 3.0))
}
fcpp <- ast2ast::translate(f, args_f = function(v) v |> type(vec(double)))
x <- c(1.0, 5.0, 2.0, 8.0)
res <- fcpp(x) |> c()

expect_equal(res, which(x > 3.0))

# no matches -- must return a zero-length integer vector, not error
x <- c(1.0, 2.0, 3.0)
res <- fcpp(x) |> c()
expect_equal(res, integer(0))

# a scalar condition still returns a vector, matching R
f_scalar <- function(x) {
  return(which(x > 3.0))
}
fcpp_scalar <- ast2ast::translate(f_scalar, args_f = function(x) x |> type(double))
expect_equal(c(fcpp_scalar(5.0)), 1L)
expect_equal(c(fcpp_scalar(1.0)), integer(0))

# --- abs() on arrays / lazy expressions ------------------------------------
# abs had a scalar overload but no array one (dispatch layer 3, not 1), so
# abs(vec), abs(mat), abs(a - b) all leaked a g++ error.

f <- function(a) abs(a)
fcpp <- ast2ast::translate(f, args_f = function(a) a |> type(vec(double)))
x <- c(-1.5, 2.5, -3.5, 0.0)
expect_equal(c(fcpp(x)), abs(x))

# lazy expression argument
f <- function(a) abs(a - 1.0)
fcpp <- ast2ast::translate(f, args_f = function(a) a |> type(vec(double)))
expect_equal(c(fcpp(x)), abs(x - 1))

# nested in another op, and a reduction
f <- function(a) sum(abs(a - 1.0))
fcpp <- ast2ast::translate(f, args_f = function(a) a |> type(vec(double)))
expect_equal(fcpp(x), sum(abs(x - 1)))

# matrix
f <- function(a) abs(a)
fcpp <- ast2ast::translate(f, args_f = function(a) a |> type(mat(double)))
m <- matrix(c(-1, 2, -3, 4), 2)
expect_equal(fcpp(m), abs(m))

# integer element type is preserved (like R: abs(1:3) is integer)
f <- function(a) abs(a)
fcpp <- ast2ast::translate(f, args_f = function(a) a |> type(vec(integer)))
expect_equal(c(fcpp(c(-2L, 3L, -4L))), c(2L, 3L, 4L))

# reverse-mode AD through abs: d/dx sum(abs(x)) = sign(x)
g <- ast2ast::translate(function(x) { y <- sum(abs(x)); return(deriv(y, x)) },
  derivative = "reverse")
expect_equal(c(g(c(-2.0, 3.0, -0.5))), c(-1, 1, -1))

# --- diag(x): 1-argument forms -------------------------------------------------
# diag(n): scalar size -> n-by-n identity (n is a dimension, not data)
f <- function() diag(4L)
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), diag(4))

# diag(v): vector -> square matrix with v on the diagonal
f <- function(v) diag(v)
fcpp <- ast2ast::translate(f, args_f = function(v) v |> type(vec(double)))
expect_equal(fcpp(c(2.0, 3.0, 4.0)), diag(c(2, 3, 4)))

# 3-arg construction still works
f <- function() diag(1.0, 2L, 3L)
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), diag(1, 2, 3))

# 2 args is still an arity error
expect_error(ast2ast::translate(function() diag(3L, 3L)),
  pattern = "Wrong number of arguments")

# diag(matrix) is rejected -> pointed at get_diag
expect_error(
  ast2ast::translate(function(m) diag(m), args_f = function(m) m |> type(mat(double))),
  pattern = "get_diag"
)

# diag(n) with a double scalar size
f <- function() diag(3.0)
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), diag(3))

# diag(v) with an integer vector -> integer matrix (like R)
f <- function(v) diag(v)
fcpp <- ast2ast::translate(f, args_f = function(v) v |> type(vec(integer)))
expect_equal(fcpp(c(1L, 2L, 3L)), diag(1:3))

# diag(n) as a size taken from an argument
f <- function(n) diag(n)
fcpp <- ast2ast::translate(f, args_f = function(n) n |> type(integer))
expect_equal(fcpp(5L), diag(5))

# 3-arg square, scalar x
f <- function() diag(2.0, 3L, 3L)
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), diag(2, 3, 3))

# 3-arg with a vector x, recycled along the diagonal
f <- function(v) diag(v, 3L, 3L)
fcpp <- ast2ast::translate(f, args_f = function(v) v |> type(vec(double)))
expect_equal(fcpp(c(1.0, 2.0)), diag(c(1, 2), 3, 3))

# 3-arg non-square, more rows than cols
f <- function() diag(1.0, 4L, 2L)
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), diag(1, 4, 2))

# reverse-mode AD through diag(v): s = sum(get_diag(diag(x) %*% diag(x))) = sum(x^2)
g <- ast2ast::translate(function(x) {
  M <- diag(x)
  s <- sum(get_diag(M %*% M))
  return(deriv(s, x))
}, derivative = "reverse",
  args_f = function(x) {
    x |> type(vec(double))
  }
)
expect_equal(c(g(c(2.0, 3.0, 4.0))), c(4, 6, 8))

# forward-mode AD through diag(v)
fw <- ast2ast::translate(function(x) {
  jac <- matrix(0.0, 1L, 3L)
  for (i in 1L:3L) {
    seed(x, i)
    M <- diag(x)
    s <- sum(get_diag(M %*% M))
    d <- get_dot(s)
    jac[1L, i] <- d[[1L]]
    unseed(x, i)
  }
  return(jac)
}, derivative = "forward",
  args_f = function(x) {
    x |> type(vec(double))
  }
)
expect_equal(c(fw(c(2.0, 3.0, 4.0))), c(4, 6, 8))
