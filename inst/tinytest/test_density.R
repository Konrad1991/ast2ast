library(tinytest)

# --- dnorm / dunif: identical to R ---------------------------------------------

f <- function(x, a, b, log, test) {
  argtypes(x |> type(vec(double)), a |> type(vec(double)), b |> type(vec(double)),
           log |> type(logical), test |> type(int))
  if (test == 1L) return(dnorm(x, a, b, log))
  if (test == 2L) return(dunif(x, a, b, log))
  if (test == 3L) return(dnorm(x))
  if (test == 4L) return(dunif(x, max = b))
  return(dnorm(x, log = log))
}
fcpp <- ast2ast::translate(f)
x <- c(-1, 0, 0.5, 3, 7, 40, -Inf, Inf)
expect_identical(fcpp(x, 0.5, 1.5, FALSE, 1L), dnorm(x, 0.5, 1.5))
expect_identical(fcpp(x, 0.5, 1.5, TRUE, 1L), dnorm(x, 0.5, 1.5, log = TRUE))
expect_identical(fcpp(x, -1, 3, FALSE, 2L), dunif(x, -1, 3))
expect_identical(fcpp(x, -1, 3, TRUE, 2L), dunif(x, -1, 3, log = TRUE))
expect_identical(fcpp(x, 0, 1, FALSE, 3L), dnorm(x))
expect_identical(fcpp(x, 0, 4, FALSE, 4L), dunif(x, max = 4))
expect_identical(fcpp(x, 0, 1, TRUE, 5L), dnorm(x, log = TRUE))
# recycling, empty, NA, sd = 0
expect_identical(fcpp(x, c(0, 1), c(1, 2, 3), TRUE, 1L), dnorm(x, c(0, 1), c(1, 2, 3), log = TRUE))
expect_identical(fcpp(numeric(0), 0, 1, FALSE, 1L), numeric(0))
expect_identical(fcpp(c(1, NA, 2), 0, 1, FALSE, 1L), dnorm(c(1, NA, 2)))
expect_identical(fcpp(c(1, 2), 1, 0, FALSE, 1L), dnorm(c(1, 2), 1, 0))
# NaN for sd < 0 / max <= min, with a warning like R
expect_warning(res <- fcpp(c(1, 2), 0, -1, FALSE, 1L), pattern = "NaNs produced")
expect_true(all(is.nan(res)))
expect_warning(res <- fcpp(c(1, 2), 3, 1, FALSE, 2L), pattern = "NaNs produced")
expect_true(all(is.nan(res)))

# scalars in, scalar out
f <- function(x) {
  argtypes(x |> type(double))
  returntype(double)
  dnorm(x, 1, 2, TRUE) + dunif(x, 0, 3, TRUE)
}
fcpp <- ast2ast::translate(f)
expect_identical(fcpp(1.2), dnorm(1.2, 1, 2, TRUE) + dunif(1.2, 0, 3, TRUE))

# errors
f <- function(x) {
  argtypes(x |> type(vec(logical)))
  dnorm(x)
}
expect_error(ast2ast::translate(f), pattern = "dnorm: x has to be a double vector or scalar")
f <- function(x) {
  argtypes(x |> type(vec(double)))
  dunif(x, log = c(TRUE, FALSE))
}
expect_error(ast2ast::translate(f), pattern = "dunif: log has to be a scalar logical")

# --- derivatives -----------------------------------------------------------------

# Skipped on CRAN: AD compiles are expensive.
if (!at_home()) exit_file("slow: AD compiles; runs at home / CI only")

# negative log-likelihood of y ~ N(mu, sd), theta = c(mu, sd)
nll <- function(theta, y) {
  argtypes(theta |> type(vec(double)), y |> type(vec(double)))
  loss <- function(theta, y) {
    argtypes(theta |> type(vec(double)), y |> type(vec(double)) |> const() |> ref())
    returntype(vec(double))
    out <- numeric(1L)
    out[[1L]] <- -sum(dnorm(y, theta[[1L]], theta[[2L]], TRUE))
    out
  }
  jacobian(loss, theta, y)
}
y <- c(0.3, -1.2, 2.5, 0.8, 1.1)
theta <- c(0.4, 1.3)
grad <- c(-sum(y - theta[1]) / theta[2]^2,
          length(y) / theta[2] - sum((y - theta[1])^2) / theta[2]^3)
for (d in list(NULL, "forward", "reverse")) {
  fcpp <- ast2ast::translate(nll, derivative = d)
  expect_equal(c(fcpp(theta, y)), grad, tolerance = 1e-12, info = if (is.null(d)) "plain" else d)
}

# hessian of the same loss
hess <- function(theta, y) {
  argtypes(theta |> type(vec(double)), y |> type(vec(double)))
  loss <- function(theta, y) {
    argtypes(theta |> type(vec(double)), y |> type(vec(double)) |> const() |> ref())
    returntype(double)
    -sum(dnorm(y, theta[[1L]], theta[[2L]], TRUE))
  }
  hessian(loss, theta, y)
}
fcpp <- ast2ast::translate(hess)
n <- length(y)
r <- y - theta[1]
s <- theta[2]
expected <- matrix(c(n / s^2, 2 * sum(r) / s^3,
                     2 * sum(r) / s^3, -n / s^2 + 3 * sum(r^2) / s^4), 2, 2)
expect_equal(fcpp(theta, y), expected, tolerance = 1e-10)

# dunif: derivative w.r.t. min and max inside the support
nll <- function(theta, y) {
  argtypes(theta |> type(vec(double)), y |> type(vec(double)))
  loss <- function(theta, y) {
    argtypes(theta |> type(vec(double)), y |> type(vec(double)) |> const() |> ref())
    returntype(vec(double))
    out <- numeric(1L)
    out[[1L]] <- -sum(dunif(y, theta[[1L]], theta[[2L]], TRUE))
    out
  }
  jacobian(loss, theta, y)
}
fcpp <- ast2ast::translate(nll)
y <- c(0.5, 1.5, 2.5)
expect_equal(c(fcpp(c(0, 4), y)), c(-3 / 4, 3 / 4), tolerance = 1e-12)
