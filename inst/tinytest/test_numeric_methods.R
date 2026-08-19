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
