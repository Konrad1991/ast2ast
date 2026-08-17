library(tinytest)

if (!requireNamespace("nnls", quietly = TRUE)) {
  exit_file("nnls package not installed")
}

f <- function(A, b) {
  x <- nnls(A, b)
  return(x)
}
f_args <- function(A, b) {
  A |> type(matrix(double))
  b |> type(vec(double))
}
fcpp <- ast2ast::translate(f, f_args)

check_case <- function(A, b, info = "") {
  x_etr <- fcpp(A, b)
  x_ref <- nnls::nnls(A, b)$x
  expect_equal(x_etr, x_ref, tolerance = 1e-6, info = info)
}

# case 1: unconstrained solution already feasible, no active bound
A <- matrix(c(1, 1, 0, 0, 1, 1), nrow = 3, ncol = 2)
b <- c(2, 1, 1)
check_case(A, b, "case1")

# case 2: unconstrained solution negative -> clamped to 0
A <- matrix(c(1, 1), nrow = 2, ncol = 1)
b <- c(-1, -1)
check_case(A, b, "case2")

# case 3: mixed -- one variable clamped, one free
A <- matrix(c(1, 0, 0, 0, 1, 1), nrow = 3, ncol = 2)
b <- c(1, -1, -1)
check_case(A, b, "case3")

# case 4: random overdetermined problem
set.seed(1)
A <- matrix(rnorm(30), nrow = 10, ncol = 3)
b <- rnorm(10)
check_case(A, b, "case4")
