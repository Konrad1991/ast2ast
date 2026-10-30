# =============================================================================
# Hand-written NUTS in ast2ast: Bayesian Lotka-Volterra
# (same model and data as hmc_lotka_volterra.R)
#
#  * NUTS:   recursive tree doubling, multinomial sampling (uniform inside a
#            subtree, biased progressive at the top), U-turn criterion with
#            M^-1, divergence = energy error > 1000 (or parameters far away)
#  * warmup: dual averaging for the step size (target accept 0.8);
#            diagonal mass matrix from the middle warmup window
#            (Stan-like: 15 % step size only, 75 % collect, 10 % step size)
#  * gradient: value_and_gradient(log_post, ...) -> CVODES forward sensitivities
# =============================================================================
library(ast2ast)

nuts_types <- function() {
  new_type(
    Tree,
    slots(
      theta_minus |> type(vec(double)),
      r_minus |> type(vec(double)),
      grad_minus |> type(vec(double)),
      theta_plus |> type(vec(double)),
      r_plus |> type(vec(double)),
      grad_plus |> type(vec(double)),
      theta_prop |> type(vec(double)),
      grad_prop |> type(vec(double)),
      lp_prop |> type(double),
      log_w |> type(double),
      sum_accept |> type(double),
      n_leapfrog |> type(int),
      done |> type(logical),
      divergent |> type(logical)
    )
  )
}

nuts <- function(theta0, data, n_warm, n_iter, step0, max_depth, seed) {
  argtypes(
    theta0    |> type(vec(double)),
    data      |> type(vec(double)),
    n_warm    |> type(int),
    n_iter    |> type(int),
    step0     |> type(double),
    max_depth |> type(int),
    seed      |> type(int)
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

  log_sum_exp <- function(a, b) {
    argtypes(a |> type(double) |> const(), b |> type(double) |> const())
    returntype(double)
    m <- a
    if (b > a) m <- b
    m + log(exp(a - m) + exp(b - m))
  }

  u_turn <- function(theta_minus, theta_plus, r_minus, r_plus, m_inv) {
    argtypes(
      theta_minus |> type(vec(double)) |> ref() |> const(),
      theta_plus |> type(vec(double)) |> ref() |> const(),
      r_minus |> type(vec(double)) |> ref() |> const(),
      r_plus |> type(vec(double)) |> ref() |> const(),
      m_inv |> type(vec(double)) |> ref() |> const()
    )
    returntype(logical)
    d <- theta_plus - theta_minus
    turned <- FALSE
    if (sum(d * m_inv * r_minus) < 0) turned <- TRUE
    if (sum(d * m_inv * r_plus) < 0) turned <- TRUE
    return(turned)
  }

  # subtree of 2^depth leapfrog steps starting at (theta, r, grad) in direction
  # log weights relative to the start energy H0: log_w = H0 - H
  build_tree <- function(theta, r, grad, direction, depth, step, H0, m_inv, theta_ref, data, unif) {
    argtypes(
      theta |> type(vec(double)) |> ref() |> const(),
      r |> type(vec(double)) |> ref() |> const(),
      grad |> type(vec(double)) |> ref() |> const(),
      direction |> type(double) |> const(),
      depth |> type(int) |> const(),
      step |> type(double) |> const(),
      H0 |> type(double) |> const(),
      m_inv |> type(vec(double)) |> ref() |> const(),
      theta_ref |> type(vec(double)) |> ref() |> const(),
      data |> type(vec(double)) |> ref() |> const(),
      unif |> type(uniform) |> ref()
    )
    returntype(Tree)
    tree |> type(Tree)
    if (depth == 0L) {
      eps <- direction * step
      r_new <- r + 0.5 * eps * grad
      th <- theta + eps * m_inv * r_new
      tree$n_leapfrog <- 1L
      # cvode errors out for extreme parameters: count as divergent before solving
      # (5 sd cut good trajectories: ESS 222 vs 360)
      far <- FALSE
      for (j in 1L:length(th)) {
        if (abs(th[[j]] - theta_ref[[j]]) > 20.0 * sqrt(m_inv[[j]])) far <- TRUE
      }
      if (far) {
        tree$done <- TRUE
        tree$divergent <- TRUE
        tree$sum_accept <- 0.0
        tree$log_w <- 0.0
        return(tree)
      }
      vg <- value_and_gradient(log_post, th, data)
      r_new <- r_new + 0.5 * eps * vg$gradient
      H <- -vg$value + 0.5 * sum(m_inv * r_new^2)
      tree$theta_minus <- th
      tree$r_minus <- r_new
      tree$grad_minus <- vg$gradient
      tree$theta_plus <- th
      tree$r_plus <- r_new
      tree$grad_plus <- vg$gradient
      tree$theta_prop <- th
      tree$grad_prop <- vg$gradient
      tree$lp_prop <- vg$value
      tree$log_w <- H0 - H
      tree$sum_accept <- 1.0
      if (H0 - H < 0) tree$sum_accept <- exp(H0 - H)
      # NaN energy fails the comparison too
      tree$divergent <- TRUE
      if (H - H0 < 1000) tree$divergent <- FALSE
      tree$done <- tree$divergent
      return(tree)
    }

    left <- build_tree(theta, r, grad, direction, depth - 1L, step, H0, m_inv, theta_ref, data, unif)
    tree <- left
    if (left$done) return(tree)
    if (direction > 0) {
      right <- build_tree(left$theta_plus, left$r_plus, left$grad_plus, direction, depth - 1L,
                          step, H0, m_inv, theta_ref, data, unif)
    } else {
      right <- build_tree(left$theta_minus, left$r_minus, left$grad_minus, direction, depth - 1L,
                          step, H0, m_inv, theta_ref, data, unif)
    }
    tree$n_leapfrog <- left$n_leapfrog + right$n_leapfrog
    tree$sum_accept <- left$sum_accept + right$sum_accept
    if (right$done) {
      tree$done <- TRUE
      tree$divergent <- right$divergent
      return(tree)
    }
    log_w <- log_sum_exp(left$log_w, right$log_w)
    # uniform multinomial inside the subtree
    if (log(draw(unif, 1L)[[1L]]) < right$log_w - log_w) {
      tree$theta_prop <- right$theta_prop
      tree$grad_prop <- right$grad_prop
      tree$lp_prop <- right$lp_prop
    }
    tree$log_w <- log_w
    if (direction > 0) {
      tree$theta_plus <- right$theta_plus
      tree$r_plus <- right$r_plus
      tree$grad_plus <- right$grad_plus
    } else {
      tree$theta_minus <- right$theta_minus
      tree$r_minus <- right$r_minus
      tree$grad_minus <- right$grad_minus
    }
    tree$done <- u_turn(tree$theta_minus, tree$theta_plus, tree$r_minus, tree$r_plus, m_inv)
    return(tree)
  }

  # own seeds: two distributions on the same engine would share one stream
  momentum <- normal(mersenne_twister(seed))
  unif <- uniform(mersenne_twister(seed + 1L))

  n_par <- length(theta0)
  n_total <- n_warm + n_iter
  # columns: theta, step size, tree depth, leapfrog steps, divergent
  draws <- matrix(0, n_total, n_par + 4L)
  theta <- theta0
  vg <- value_and_gradient(log_post, theta, data)
  lp <- vg$value
  grad <- vg$gradient
  m_inv <- theta0
  for (j in 1L:n_par) {
    m_inv[[j]] <- 1.0
  }

  # warmup windows
  window_start <- as.integer(0.15 * as.numeric(n_warm)) + 1L
  window_end <- as.integer(0.9 * as.numeric(n_warm))

  # dual averaging (Hoffman & Gelman 2014, Alg. 5)
  delta <- 0.8
  gamma <- 0.05
  t0 <- 10.0
  kappa <- 0.75
  step <- step0
  mu <- log(10.0 * step)
  H_bar <- 0.0
  log_step_bar <- 0.0
  m_count <- 0.0

  tree |> type(Tree)
  sub |> type(Tree)
  for (it in 1L:n_total) {
    # r ~ N(0, M)
    r <- draw(momentum, n_par) / sqrt(m_inv)
    H0 <- -lp + 0.5 * sum(m_inv * r^2)
    tree$theta_minus <- theta
    tree$r_minus <- r
    tree$grad_minus <- grad
    tree$theta_plus <- theta
    tree$r_plus <- r
    tree$grad_plus <- grad
    tree$theta_prop <- theta
    tree$grad_prop <- grad
    tree$lp_prop <- lp
    tree$log_w <- 0.0
    n_leap <- 0L
    sum_accept <- 0.0
    depth <- 0L
    divergent <- FALSE
    while (depth < max_depth) {
      direction <- 1.0
      if (draw(unif, 1L)[[1L]] < 0.5) direction <- -1.0
      if (direction > 0) {
        sub <- build_tree(tree$theta_plus, tree$r_plus, tree$grad_plus, direction, depth,
                          step, H0, m_inv, theta, data, unif)
      } else {
        sub <- build_tree(tree$theta_minus, tree$r_minus, tree$grad_minus, direction, depth,
                          step, H0, m_inv, theta, data, unif)
      }
      n_leap <- n_leap + sub$n_leapfrog
      sum_accept <- sum_accept + sub$sum_accept
      depth <- depth + 1L
      if (sub$done) {
        divergent <- sub$divergent
        break
      }
      # biased progressive sampling: favours the new subtree
      if (log(draw(unif, 1L)[[1L]]) < sub$log_w - tree$log_w) {
        tree$theta_prop <- sub$theta_prop
        tree$grad_prop <- sub$grad_prop
        tree$lp_prop <- sub$lp_prop
      }
      tree$log_w <- log_sum_exp(tree$log_w, sub$log_w)
      if (direction > 0) {
        tree$theta_plus <- sub$theta_plus
        tree$r_plus <- sub$r_plus
        tree$grad_plus <- sub$grad_plus
      } else {
        tree$theta_minus <- sub$theta_minus
        tree$r_minus <- sub$r_minus
        tree$grad_minus <- sub$grad_minus
      }
      if (u_turn(tree$theta_minus, tree$theta_plus, tree$r_minus, tree$r_plus, m_inv)) break
    }
    theta <- tree$theta_prop
    lp <- tree$lp_prop
    grad <- tree$grad_prop
    accept_stat <- sum_accept / as.numeric(n_leap)

    for (j in 1L:n_par) {
      draws[it, j] <- theta[[j]]
    }
    draws[it, n_par + 1L] <- step
    draws[it, n_par + 2L] <- as.numeric(depth)
    draws[it, n_par + 3L] <- as.numeric(n_leap)
    if (divergent) draws[it, n_par + 4L] <- 1.0

    if (it <= n_warm) {
      m_count <- m_count + 1.0
      eta <- 1.0 / (m_count + t0)
      H_bar <- (1.0 - eta) * H_bar + eta * (delta - accept_stat)
      log_step <- mu - sqrt(m_count) / gamma * H_bar
      w <- m_count^(-kappa)
      log_step_bar <- w * log_step + (1.0 - w) * log_step_bar
      step <- exp(log_step)
      # end of the middle window: diagonal mass matrix (regularised like Stan),
      # restart the step size adaptation
      if (it == window_end) {
        n_used <- as.numeric(window_end - window_start + 1L)
        for (j in 1L:n_par) {
          mean_j <- 0.0
          for (i in window_start:window_end) mean_j <- mean_j + draws[i, j]
          mean_j <- mean_j / n_used
          v <- 0.0
          for (i in window_start:window_end) v <- v + (draws[i, j] - mean_j)^2
          v <- v / (n_used - 1.0)
          m_inv[[j]] <- (n_used / (n_used + 5.0)) * v + 1e-3 * (5.0 / (n_used + 5.0))
        }
        mu <- log(10.0 * step)
        H_bar <- 0.0
        log_step_bar <- 0.0
        m_count <- 0.0
      }
      if (it == n_warm) step <- exp(log_step_bar)
    }
  }
  print("inverse mass matrix (diagonal):")
  print(m_inv)
  print("step size after warmup:")
  print(step)
  return(draws)
}
nuts_cpp <- translate(nuts, types_f = nuts_types)

# --- simulated data (as in hmc_lotka_volterra.R) ------------------------------------
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
n_warm <- 500L
timing <- system.time(
  draws <- nuts_cpp(log(truth), data, n_warm, 1500L, 0.01, 10L, 42L)
)
print(timing)

sampling <- draws[-seq_len(n_warm), ]
post <- exp(sampling[, 1:4])
colnames(post) <- c("a", "b", "d", "g")
print(rbind(
  truth = truth,
  mean = colMeans(post),
  q05 = apply(post, 2, quantile, 0.05),
  q95 = apply(post, 2, quantile, 0.95)
))
cat("mean tree depth:", mean(sampling[, 6]), " mean leapfrog:", mean(sampling[, 7]),
    " divergent:", sum(sampling[, 8]), "\n")
par(mfrow = c(2, 2))
for (j in 1:4) {
  plot(draws[, j], type = "l", main = colnames(post)[j], ylab = "log")
  abline(v = n_warm, col = "red")
}
