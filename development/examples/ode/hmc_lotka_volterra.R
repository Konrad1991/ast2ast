rm(list = ls())
# =============================================================================
# Hand-written HMC in ast2ast: Bayesian Lotka-Volterra
#
#   du/dt = a*u - b*u*v
#   dv/dt = d*u*v - g*v
#
# theta = log(a, b, d, g), y0 known, Gaussian noise (sigma known),
# prior theta ~ N(log(c(1, 0.5, 0.5, 0.1)), 1).
#
#  * log_post: cvode + log density, written in the DSL
#  * gradient: value_and_gradient(log_post, theta, data) -> value + gradient
#              from one CVODES solve with forward sensitivities
#  * HMC:      leapfrog + Metropolis step, RNG = mersenne_twister / normal /
#              uniform; whole sampler is one compiled function
#  * warmup:   unit mass matrix, then diagonal mass matrix = posterior variance
#              of the warmup draws, frozen for sampling
# =============================================================================
library(ast2ast)

hmc <- function(theta0, data, n_warm, n_iter, n_leap, eps_warm, eps, seed) {
  argtypes(
    theta0   |> type(vec(double)),
    data     |> type(vec(double)),
    n_warm   |> type(int),
    n_iter   |> type(int),
    n_leap   |> type(int),
    eps_warm |> type(double),
    eps      |> type(double),
    seed     |> type(int)
  )
  log_post <- function(theta, data) {
    argtypes(
      theta |> type(vec(double)),
      data |> type(vec(double))
    )
    returntype(double)

    lv <- function(t, y, ydot, p) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        ydot |> type(borrow_vec(double)) |> ref(),
        p |> type(vec(double)) |> ref()
      )
      returntype(void)
      ydot[[1L]] <- p[[1L]] * y[[1L]] - p[[2L]] * y[[1L]] * y[[2L]]
      ydot[[2L]] <- p[[4L]] * y[[1L]] * y[[2L]] - p[[3L]] * y[[2L]]
    }

    p <- exp(theta)
    y0 <- c(10, 5)
    times <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    sol <- c(cvode(lv, y0, times, p, cvode_options(reltol = 1e-6, abstol = 1e-8)))

    sigma <- 0.5
    # rough orders of magnitude, not the truth
    prior_mean <- c(log(1), log(0.5), log(0.5), log(0.1))
    ll <- -0.5 * sum(((sol - data) / sigma)^2)
    lp <- -0.5 * sum((theta - prior_mean)^2)
    ll + lp
  }

  # own seeds: two distributions on the same engine would share one stream
  momentum <- normal(mersenne_twister(seed))
  unif <- uniform(mersenne_twister(seed + 1L))

  n_par <- length(theta0)
  n_total <- n_warm + n_iter
  draws <- matrix(0, n_total, n_par)
  theta <- theta0
  vg <- value_and_gradient(log_post, theta, data)
  lp <- vg$value
  grad <- vg$gradient
  lp_new <- lp
  # inverse mass matrix (diagonal): 1 in warmup, posterior variance afterwards
  m_inv <- theta0
  for (j in 1L:n_par) {
    m_inv[[j]] <- 1.0
  }
  step <- eps_warm
  accepted <- 0L
  n_divergent <- 0L
  log_alpha <- 0.0

  for (it in 1L:n_total) {
    # end of warmup: variance of the second half of the warmup draws
    if (it == n_warm + 1L) {
      first <- n_warm %/% 2L + 1L
      n_used <- as.numeric(n_warm - first + 1L)
      for (j in 1L:n_par) {
        mu <- 0.0
        for (i in first:n_warm) mu <- mu + draws[i, j]
        mu <- mu / n_used
        v <- 0.0
        for (i in first:n_warm) v <- v + (draws[i, j] - mu)^2
        m_inv[[j]] <- v / (n_used - 1.0)
      }
      step <- eps
    }
    # r ~ N(0, M)
    r <- draw(momentum, n_par) / sqrt(m_inv)
    th <- theta
    g <- grad
    # leapfrog
    r_new <- r + 0.5 * step * g
    divergent <- FALSE
    for (l in 1L:n_leap) {
      th <- th + step * m_inv * r_new
      # cvode errors out for extreme parameters: reject before solving
      for (j in 1L:n_par) {
        if (abs(th[[j]] - theta[[j]]) > 10.0 * sqrt(m_inv[[j]])) divergent <- TRUE
      }
      if (divergent) break
      vg <- value_and_gradient(log_post, th, data)
      g <- vg$gradient
      lp_new <- vg$value
      if (l < n_leap) r_new <- r_new + step * g
    }
    if (divergent) {
      n_divergent <- n_divergent + 1L
    } else {
      r_new <- r_new + 0.5 * step * g
      # H = -log_post + r^T M^-1 r / 2
      log_alpha <- (lp_new - 0.5 * sum(m_inv * r_new^2)) - (lp - 0.5 * sum(m_inv * r^2))
    }
    if (!divergent && log(draw(unif, 1L)[[1L]]) < log_alpha) {
      theta <- th
      lp <- lp_new
      grad <- g
      if (it > n_warm) accepted <- accepted + 1L
    }
    for (j in 1L:n_par) {
      draws[it, j] <- theta[[j]]
    }
  }
  print("inverse mass matrix (diagonal):")
  print(m_inv)
  print("divergent trajectories:")
  print(n_divergent)
  print("acceptance rate after warmup:")
  print(as.numeric(accepted) / as.numeric(n_iter))
  return(draws)
}
hmc_cpp <- translate(hmc)

# --- simulated data --------------------------------------------------------------
simulate <- function(p) {
  argtypes(
    p |> type(vec(double))
  )
  lv <- function(t, y, ydot, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      p |> type(vec(double)) |> ref()
    )
    returntype(void)
    ydot[[1L]] <- p[[1L]] * y[[1L]] - p[[2L]] * y[[1L]] * y[[2L]]
    ydot[[2L]] <- p[[4L]] * y[[1L]] * y[[2L]] - p[[3L]] * y[[2L]]
  }
  times <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  c(cvode(lv, c(10, 5), times, p))
}
simulate_cpp <- translate(simulate)

truth <- c(1.1, 0.4, 0.4, 0.1)
set.seed(1234L)
data <- simulate_cpp(truth) + rnorm(22, sd = 0.5)

# --- run -------------------------------------------------------------------------
# warmup: unit mass, small step; afterwards the problem is ~unit scale -> large step
n_warm <- 500L
timing <- system.time(
  draws <- hmc_cpp(log(truth), data, n_warm, 1500L, 12L, 0.008, 0.1, 42L)
)
print(timing)

post <- exp(draws[-seq_len(n_warm), ])
colnames(post) <- c("a", "b", "d", "g")
print(rbind(
  truth = truth,
  mean = colMeans(post),
  q05 = apply(post, 2, quantile, 0.05),
  q95 = apply(post, 2, quantile, 0.95)
))
par(mfrow = c(2, 2))
for (j in 1:4) {
  plot(draws[, j], type = "l", main = colnames(post)[j], ylab = "log")
  abline(v = n_warm, col = "red")
}
