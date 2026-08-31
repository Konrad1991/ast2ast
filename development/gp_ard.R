# =============================================================================
# GP marginal likelihood with an ARD kernel -- and its exact gradient.
#
# What is unique here: `gp_grad` is a *compiled reverse-mode gradient through a
# Cholesky factorisation* obtained from a plain R function. With one length-scale
# per input dimension the gradient has d + 2 entries; reverse AD costs one
# backward pass, central differences cost 2*(d + 2) forward passes.
#
#   K[i,j]  = sf^2 * exp(-0.5 * sum_k (xi_k - xj_k)^2 / ell_k^2)   (+ sn^2 on diag)
#   logtheta = (log ell_1..d, log sf, log sn)
#   nll      = 0.5 y' K^-1 y + sum(log diag(chol K)) + n/2 log(2*pi)
# =============================================================================
library(ast2ast)

gp_nll <- function(X, y, logtheta) {
  argtypes(
    X        |> type(mat(double)),
    y        |> type(vec(double)),
    logtheta |> type(vec(double))
  )
  n <- nrow(X)
  d <- ncol(X)
  sf2 <- exp(2.0 * logtheta[[d + 1L]])
  sn2 <- exp(2.0 * logtheta[[d + 2L]])
  K <- matrix(0.0, n, n)
  for (i in 1L:n) {
    for (j in 1L:n) {
      s <- 0.0
      for (k in 1L:d) {
        dif <- X[i, k] - X[j, k]
        s <- s + dif * dif * exp(-2.0 * logtheta[[k]])
      }
      K[i, j] <- sf2 * exp(-0.5 * s)
    }
    K[i, i] <- K[i, i] + sn2
  }
  L <- chol(K)
  b <- backsolve(L, forwardsolve(t(L), y))
  quad <- 0.0
  for (i in 1L:n) quad <- quad + y[[i]] * b[[i]]
  dgL <- get_diag(L)
  half_logdet <- 0.0
  for (i in 1L:n) half_logdet <- half_logdet + log(dgL[[i]])
  return(0.5 * quad + half_logdet + 0.5 * n * log(6.283185307179586))
}

# same body, ending in deriv(nll, logtheta) -- the DSL has no closures / calls
gp_grad <- function(X, y, logtheta) {
  argtypes(
    X        |> type(mat(double)),
    y        |> type(vec(double)),
    logtheta |> type(vec(double))
  )
  n <- nrow(X)
  d <- ncol(X)
  sf2 <- exp(2.0 * logtheta[[d + 1L]])
  sn2 <- exp(2.0 * logtheta[[d + 2L]])
  K <- matrix(0.0, n, n)
  for (i in 1L:n) {
    for (j in 1L:n) {
      s <- 0.0
      for (k in 1L:d) {
        dif <- X[i, k] - X[j, k]
        s <- s + dif * dif * exp(-2.0 * logtheta[[k]])
      }
      K[i, j] <- sf2 * exp(-0.5 * s)
    }
    K[i, i] <- K[i, i] + sn2
  }
  L <- chol(K)
  b <- backsolve(L, forwardsolve(t(L), y))
  quad <- 0.0
  for (i in 1L:n) quad <- quad + y[[i]] * b[[i]]
  dgL <- get_diag(L)
  half_logdet <- 0.0
  for (i in 1L:n) half_logdet <- half_logdet + log(dgL[[i]])
  nll <- 0.5 * quad + half_logdet + 0.5 * n * log(6.283185307179586)
  return(deriv(nll, logtheta))
}

nll_cpp  <- ast2ast::translate(gp_nll)
grad_cpp <- ast2ast::translate(gp_grad, derivative = "reverse")

# toy data: 8 inputs, only dims 1 and 3 drive y
set.seed(1)
n <- 90
d <- 8
X <- matrix(runif(n * d, -2, 2), n, d)
y <- sin(X[, 1]) + 0.5 * X[, 3]^2 + rnorm(n, sd = 0.1)
lt0 <- rep(0.0, d + 2L)

# pure-R reference + a finite-difference gradient
gp_nll_R <- function(X, y, lt) {
  n <- nrow(X); d <- ncol(X)
  Xs <- sweep(X, 2, exp(-lt[1:d]), "*")
  K  <- exp(2 * lt[d + 1]) * exp(-0.5 * as.matrix(dist(Xs))^2) + diag(exp(2 * lt[d + 2]), n)
  L  <- chol(K)
  b  <- backsolve(L, forwardsolve(t(L), y))
  0.5 * sum(y * b) + sum(log(diag(L))) + 0.5 * n * log(2 * pi)
}
num_grad <- function(fn, th, h = 1e-6) {
  vapply(seq_along(th), function(i) {
    tp <- th; tm <- th; tp[i] <- tp[i] + h; tm[i] <- tm[i] - h
    (fn(tp) - fn(tm)) / (2 * h)
  }, numeric(1))
}

stopifnot(
  isTRUE(all.equal(nll_cpp(X, y, lt0), gp_nll_R(X, y, lt0))),
  isTRUE(all.equal(as.vector(grad_cpp(X, y, lt0)),
                   num_grad(function(lt) gp_nll_R(X, y, lt), lt0),
                   tolerance = 1e-4))
)

# fit the hyper-parameters -- the length-scales flag which inputs matter
fit <- optim(lt0,
             fn = function(lt) nll_cpp(X, y, lt),
             gr = function(lt) as.vector(grad_cpp(X, y, lt)),
             method = "BFGS")
setNames(round(exp(fit$par[1:d]), 2), paste0("ell", 1:d))   # small = relevant

microbenchmark::microbenchmark(
  ast2ast_grad = grad_cpp(X, y, lt0),
  R_finitediff = num_grad(function(lt) gp_nll_R(X, y, lt), lt0),
  times = 50
)
