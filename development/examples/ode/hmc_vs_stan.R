# =============================================================================
# Benchmark: hand-written HMC (ast2ast) vs Stan NUTS (rstan) on the same
# Bayesian Lotka-Volterra model (hmc_lotka_volterra.R).
#
# Both: BDF + CVODES forward sensitivities, reltol 1e-6, abstol 1e-8,
# 500 warmup + 1500 sampling iterations, 1 chain, several seeds, start at truth.
# Stan: max_num_steps 1e4 (a failed solve rejects the proposal).
# Metric: min bulk ESS over the 4 parameters per second (warmup included).
# Stan: compile time excluded. t = 0 data are dropped for Stan (constant
# w.r.t. theta, same posterior).
# =============================================================================
library(rstan)
library(posterior)

src <- readLines("development/examples/ode/hmc_lotka_volterra.R")
eval(parse(text = src[seq_len(grep("^# --- run", src) - 1L)]))

stan_code <- "
functions {
  vector lv(real t, vector y, vector p) {
    vector[2] dydt;
    dydt[1] = p[1] * y[1] - p[2] * y[1] * y[2];
    dydt[2] = p[4] * y[1] * y[2] - p[3] * y[2];
    return dydt;
  }
}
data {
  int<lower=1> n_t;
  array[n_t] real ts;
  array[n_t] vector[2] obs;
  vector[4] prior_mean;
}
parameters {
  vector[4] theta;
}
model {
  array[n_t] vector[2] sol = ode_bdf_tol(lv, [10, 5]', 0, ts, 1e-6, 1e-8, 10000, exp(theta));
  theta ~ normal(prior_mean, 1);
  for (k in 1:n_t) obs[k] ~ normal(sol[k], 0.5);
}
"
stan_model_lv <- stan_model(model_code = stan_code)

times <- 0:10
obs <- matrix(data, ncol = 2)
stan_data <- list(
  n_t = 10L, ts = times[-1],
  obs = lapply(2:11, function(k) obs[k, ]),
  prior_mean = log(c(1, 0.5, 0.5, 0.1))
)

n_warm <- 500L
n_iter <- 1500L
seeds <- c(42L, 7L, 123L)

res <- list()
for (seed in seeds) {
  timing <- system.time(
    draws <- hmc_cpp(log(truth), data, n_warm, n_iter, 12L, 0.008, 0.1, seed)
  )[["elapsed"]]
  post <- draws[-seq_len(n_warm), ]
  ess <- apply(post, 2, ess_bulk)
  res[[length(res) + 1L]] <- data.frame(
    sampler = "ast2ast HMC", seed = seed, seconds = timing, min_ess = min(ess),
    ess_per_s = min(ess) / timing, mean_a = mean(exp(post[, 1]))
  )

  # same start as the ast2ast chain (random inits run into very stiff regions)
  fit <- sampling(stan_model_lv, data = stan_data, chains = 1, warmup = n_warm,
                  iter = n_warm + n_iter, seed = seed, refresh = 500,
                  init = list(list(theta = log(truth))))
  timing <- sum(get_elapsed_time(fit))
  theta <- as.matrix(fit, pars = "theta")
  ess <- apply(theta, 2, ess_bulk)
  res[[length(res) + 1L]] <- data.frame(
    sampler = "Stan NUTS", seed = seed, seconds = timing, min_ess = min(ess),
    ess_per_s = min(ess) / timing, mean_a = mean(exp(theta[, 1]))
  )
}
res <- do.call(rbind, res)
print(res, digits = 3)
print(aggregate(cbind(seconds, min_ess, ess_per_s) ~ sampler, res, median), digits = 3)
