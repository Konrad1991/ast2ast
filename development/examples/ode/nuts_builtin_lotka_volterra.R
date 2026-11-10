# =============================================================================
# Built-in nuts(): Bayesian Lotka-Volterra
# (same model, data and prior as nuts_lotka_volterra.R, which writes NUTS by hand)
#
#  * log_post: log posterior of the log parameters, cvode inside
#  * a cvode failure counts as divergent transition (no guard needed)
#  * draws: n_samples x n_chains x n_par -> posterior::as_draws_array()
# =============================================================================
library(ast2ast)

# --- simulated data ------------------------------------------------------------
simulate <- function(p) {
  argtypes(p |> type(vec(double)))
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
  c(cvode(lv, c(10, 5), c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), p))
}
simulate_cpp <- translate(simulate)

truth <- c(1.1, 0.4, 0.4, 0.1)
set.seed(1234L)
data <- simulate_cpp(truth) + rnorm(22, sd = 0.5)

sample_lv <- function(theta0, data, n_threads) {
  argtypes(
    theta0 |> type(vec(double)),
    data |> type(vec(double)),
    n_threads |> type(int)
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
    times <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    sol <- c(
      cvode(
        lv, c(10, 5),
        times, p,
        cvode_options(reltol = 1e-6, abstol = 1e-8, max_steps = 100000L)
      )
    )
    sigma <- 0.5
    # rough orders of magnitude, not the truth
    # prior_mean <- c(log(1), log(0.5), log(0.5), log(0.1))
    # prior_mean <- rep(0, 4)
    lo <- 0.01
    hi <- 100
    mu <- (log(lo) + log(hi)) / 2
    s <- (log(hi) - log(lo)) / 0.5
    log_prior_mean <- sum(dnorm(theta, mu, s, log = TRUE))
    -0.5 * sum(((sol - data) / sigma)^2) + log_prior_mean
  }

  nuts(
    # nuts(f, theta0, n_warmup, n_samples, n_chains, seed, options = nuts_options())
    log_post, theta0, 500L,     1500L,     4L,       42L,
    nuts_options(n_threads = n_threads, max_treedepth = 6L), data
  )
}
sample_lv_cpp <- translate(sample_lv, debug = FALSE)

# --- run -------------------------------------------------------------------------
# NOTE: RcppThread pins workers to logical CPUs; with SMT, n_threads = 4 can mean
# 2 physical cores (~2x slower per chain)
theta0 <- log(truth)
theta0 <- rnorm(4, 0, 1)
n_threads <- 4L
timing <- system.time(res <- sample_lv_cpp(theta0, data, n_threads))
print(timing)
str(res)
res$step_size
res$lp[20]
exp(res$draws[20, 1, ])     # a ~ 4.5 = falscher Modus

par_names <- c("a", "b", "d", "g")
dimnames(res$draws) <- list(NULL, NULL, par_names)
post <- exp(res$draws)
print(rbind(
  truth = truth,
  mean = apply(post, 3, mean),
  q05 = apply(post, 3, quantile, 0.05),
  q95 = apply(post, 3, quantile, 0.95)
))
cat("mean tree depth:", mean(res$treedepth), " mean leapfrog:", mean(res$n_leapfrog),
    " divergent:", sum(res$divergent), "\n")
cat("step size per chain:", res$step_size, "\n")

if (requireNamespace("posterior", quietly = TRUE)) {
  print(posterior::summarise_draws(posterior::as_draws_array(res$draws)))
}

par(mfrow = c(2, 2))
for (j in seq_along(par_names)) {
  matplot(res$draws[, , j], type = "l", lty = 1, main = par_names[j], ylab = "log")
}
