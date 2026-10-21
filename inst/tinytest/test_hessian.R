library(tinytest)

# Skipped on CRAN: AD compiles are expensive.
if (!at_home()) exit_file("slow: AD compiles; runs at home / CI only")

rosenbrock_hessian <- function(x) {
  matrix(c(1200 * x[[1]]^2 - 400 * x[[2]] + 2, -400 * x[[1]],
           -400 * x[[1]], 200), 2, 2)
}

# --- plain translation: rosenbrock, hessian and hvp ---------------------------

f <- function(x, v, test) {
  argtypes(x |> type(vec(double)), v |> type(vec(double)), test |> type(int))
  rosen <- function(x) {
    argtypes(x |> type(vec(double)))
    returntype(double)
    100 * (x[[2L]] - x[[1L]]^2)^2 + (1 - x[[1L]])^2
  }
  if (test == 1L) return(hessian(rosen, x))
  return(matrix(hvp(rosen, x, v), 2L, 1L))
}
fcpp <- ast2ast::translate(f)
x <- c(-1.2, 1.0)
expect_equal(fcpp(x, x, 1L), rosenbrock_hessian(x))
expect_equal(c(fcpp(x, c(0.3, -2), 2L)), c(rosenbrock_hessian(x) %*% c(0.3, -2)))

# --- elementwise functions vs finite differences of an analytic gradient ------

g <- function(x) {
  argtypes(x |> type(vec(double)))
  smooth <- function(x) {
    argtypes(x |> type(vec(double)))
    returntype(double)
    t1 <- sin(x[[1L]]) * exp(x[[2L]]) + log(x[[3L]]) * sqrt(x[[4L]])
    t2 <- x[[1L]] / x[[3L]] + atan(x[[2L]] * x[[4L]]) + tanh(x[[1L]] - x[[4L]])
    t3 <- x[[3L]]^x[[2L]] + cos(x[[4L]])^2 + cosh(x[[2L]]) / sinh(x[[3L]])
    t4 <- tan(x[[1L]] / 4) + asin(x[[2L]] / 3) - acos(x[[4L]] / 5) + abs(x[[1L]] - 2) + log(x[[4L]]) / x[[1L]]
    t1 + t2 * t3 - t4
  }
  hessian(smooth, x)
}
smooth_r <- function(x) {
  t1 <- sin(x[1]) * exp(x[2]) + log(x[3]) * sqrt(x[4])
  t2 <- x[1] / x[3] + atan(x[2] * x[4]) + tanh(x[1] - x[4])
  t3 <- x[3]^x[2] + cos(x[4])^2 + cosh(x[2]) / sinh(x[3])
  t4 <- tan(x[1] / 4) + asin(x[2] / 3) - acos(x[4] / 5) + abs(x[1] - 2) + log(x[4]) / x[1]
  t1 + t2 * t3 - t4
}
hessian_fd <- function(f, x, h = 1e-4) {
  n <- length(x)
  H <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    ei <- replace(numeric(n), i, h)
    ej <- replace(numeric(n), j, h)
    H[i, j] <- (f(x + ei + ej) - f(x + ei - ej) - f(x - ei + ej) + f(x - ei - ej)) / (4 * h * h)
  }
  H
}
gcpp <- ast2ast::translate(g)
x <- c(0.7, 0.4, 1.3, 0.9)
H <- gcpp(x)
expect_equal(H, hessian_fd(smooth_r, x), tolerance = 1e-5)
expect_equal(H, t(H))

# --- extra data, forward and reverse translations ------------------------------

# data: columns x1, x2, y
nll <- function(beta, data) {
  argtypes(beta |> type(vec(double)), data |> type(mat(double)))
  loglik <- function(beta, data) {
    argtypes(beta |> type(vec(double)), data |> type(mat(double)))
    returntype(double)
    s <- 0
    for (i in 1L:nrow(data)) {
      eta <- beta[[1L]] * data[i, 1L] + beta[[2L]] * data[i, 2L]
      s <- s + data[i, 3L] * eta - log(1 + exp(eta))
    }
    -s
  }
  hessian(loglik, beta, data)
}
X <- cbind(1, c(-1, -0.5, 0, 0.5, 1, 1.5))
y <- c(0, 0, 1, 0, 1, 1)
beta <- c(0.2, 0.8)
p <- 1 / (1 + exp(-(X %*% beta)))
H_ref <- t(X) %*% (X * c(p * (1 - p)))
D <- cbind(X, y, deparse.level = 0)
expect_equal(ast2ast::translate(nll)(beta, D), H_ref)
expect_equal(ast2ast::translate(nll, derivative = "forward")(beta, D), H_ref)
expect_equal(ast2ast::translate(nll, derivative = "reverse")(beta, D), H_ref)

# --- errors --------------------------------------------------------------------

f <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- function(x) {
    argtypes(x |> type(vec(double)))
    returntype(vec(double))
    x * x
  }
  hessian(h, x)
}
expect_error(ast2ast::translate(f), pattern = "has to return a scalar double")

f <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- function(x) {
    argtypes(x |> type(vec(double)))
    returntype(double)
    m <- matrix(x, 2L, 2L)
    s <- m %*% m
    s[[1L]]
  }
  hessian(h, x)
}
expect_error(ast2ast::translate(f)(c(1, 2, 3, 4)), pattern = "not supported in hessian yet")

f <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- function(x) {
    argtypes(x |> type(vec(double)))
    returntype(double)
    sum(x)
  }
  hvp(h, x, c(1, 2))
}
expect_error(ast2ast::translate(f)(c(1, 2, 3)), pattern = "x and v differ in length")
