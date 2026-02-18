# d^2 = (y - µ) * C^{-1} * (y - µ)^{T}
# y is the observation vector
# µ is the mean
# C^-1 is the inverse covariance matrix
#
# Covariance matrix for 2 variables:
# C(x, y) = (
# Var(x)          Cov(x, y)
# Cov(y, x)       Var(y)
# )
#
# Where: Cov(x, y) = 1/n sum from i = 1 to n: (x_i - mean(x))*(y_i - mean(y))
# Thus, Vars on the diagonal and the rest are the covariance
#
# Missing functionality in ast2ast:
# mean
# sum
# inverse of matrix
# variance
# covariance
# transpose
# Vector Matrix multiplication
# Matrix Matrix multiplication
#
# Should I change the storage model of Array?
# * I could have two pointers in Buffer
#   - One storing the values (bool*, int*, or double*)
#   - A second one of type bool* which stores is_na for each entry
#   - This approach does not work for Variable<Double>
# ==> As it does not work for Variable<Double> it doesn't make too much sense
#
# Creating a dedicated function lin_alg is possible.
# Here the etr objects could be copied into raw C pointers
# Next, the raw C pointers could use the same Fortran interface as R does.
# The element returned from lin_alg is copied into an etr object.
# ==> Caveat, no derivatives are calculated using this approach.
# To enable derivatives of linear algebra operations with a decent speed:
# - rewrite your own linear algebra code. This would be insane.
# - using Eigen with own scalar system. This is super slow.
# - Or using Fortran in case Double is used otherwise use Eigen
#   even though it is super slow but the user gets the derivatives.
#   However, I want fast code. I don't think it makes sense to offer
#   linear algebra versions which are 10 times slower even if the derivatives
#   are calculated as well.

mahalanobis_d2 <- function(y, mu, P) {
  n <- length(y)
  # z <- y - mu
  z <- numeric(n)
  for (i in seq_len(n)) {
    z[i] <- y[[i]] - mu[[i]]
  }
  # w = P*z
  w <- numeric(n)
  for (i in seq_len(n)) {
    acc = 0.0
    for (j in seq_len(n)) {
      acc = acc + P[i, j] * z[[j]]
    }
    w[[i]] <- acc
  }
  # d^2 = z^T w
  d2 = 0.0
  for (i in seq_len(n)) {
    d2 = d2 + z[[i]]*w[[i]]
  }

  return(d2)
}

f_args <- function(y, mu, P) {
  y |> type(vec(double)) |> ref()
  mu |> type(vec(double)) |> ref()
  P |> type(mat(double)) |> ref()
}
mahalanobis_d2_cpp <- ast2ast::translate(mahalanobis_d2, f_args)

set.seed(1)
n  <- 5
y  <- rnorm(n)
mu <- rnorm(n)
P  <- diag(n)  # easiest test: identity => d2 = sum((y-mu)^2)

d2_cpp <- mahalanobis_d2_cpp(y, mu, P)
d2_r   <- mahalanobis_d2(y, mu, P)

print(d2_cpp)
print(d2_r)
stopifnot(all.equal(d2_cpp, d2_r, tolerance = 1e-10))

n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 27L))
groups <- data.frame(blood = c(rep(4000, 27L), rep(3500, 27L), rep(300, 27L)))
