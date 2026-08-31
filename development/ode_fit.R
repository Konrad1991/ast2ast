# =============================================================================
# Fitting ODE parameters -- compiled integrator + an exact compiled gradient.
#
# Lotka-Volterra, integrated with a hand-written RK4 loop:
#   du/dt = a*u - b*u*v
#   dv/dt = d*u*v - g*v
#
#  * loss_cpp : the RK4 loss as plain double C++ -- this is where you get the
#               big speed-up over the interpreted R loop (R can't vectorise a
#               time-stepper).
#  * grad_cpp : the reverse-mode gradient of the trajectory mismatch w.r.t.
#               (a, b, d, g). It is *exact* (no step size h to tune), drops
#               straight into optim(), and stays competitive with central
#               differences even though those get to reuse LAPACK-free scalar
#               loops here.
# =============================================================================
library(ast2ast)

lv_loss <- function(theta, y0, obs, dt, substeps) {
  args(
    theta    |> type(vec(double)),
    y0       |> type(vec(double)),
    obs      |> type(mat(double)),
    dt       |> type(double),
    substeps |> type(integer)
  )
  a <- theta[[1L]]; b <- theta[[2L]]; d <- theta[[3L]]; g <- theta[[4L]]
  u <- y0[[1L]]; v <- y0[[2L]]
  nrec <- nrow(obs)
  loss <- 0.0
  for (rec in 1L:nrec) {
    for (s in 1L:substeps) {
      k1u <- a * u - b * u * v
      k1v <- d * u * v - g * v
      u2 <- u + 0.5 * dt * k1u; v2 <- v + 0.5 * dt * k1v
      k2u <- a * u2 - b * u2 * v2
      k2v <- d * u2 * v2 - g * v2
      u3 <- u + 0.5 * dt * k2u; v3 <- v + 0.5 * dt * k2v
      k3u <- a * u3 - b * u3 * v3
      k3v <- d * u3 * v3 - g * v3
      u4 <- u + dt * k3u; v4 <- v + dt * k3v
      k4u <- a * u4 - b * u4 * v4
      k4v <- d * u4 * v4 - g * v4
      u <- u + (dt / 6.0) * (k1u + 2.0 * k2u + 2.0 * k3u + k4u)
      v <- v + (dt / 6.0) * (k1v + 2.0 * k2v + 2.0 * k3v + k4v)
    }
    du <- u - obs[rec, 1L]
    dv <- v - obs[rec, 2L]
    loss <- loss + du * du + dv * dv
  }
  return(loss)
}

# same body, ending in deriv(loss, theta)  (no closures in the DSL)
lv_grad <- function(theta, y0, obs, dt, substeps) {
  args(
    theta    |> type(vec(double)),
    y0       |> type(vec(double)),
    obs      |> type(mat(double)),
    dt       |> type(double),
    substeps |> type(integer)
  )
  a <- theta[[1L]]; b <- theta[[2L]]; d <- theta[[3L]]; g <- theta[[4L]]
  u <- y0[[1L]]; v <- y0[[2L]]
  nrec <- nrow(obs)
  loss <- 0.0
  for (rec in 1L:nrec) {
    for (s in 1L:substeps) {
      k1u <- a * u - b * u * v
      k1v <- d * u * v - g * v
      u2 <- u + 0.5 * dt * k1u; v2 <- v + 0.5 * dt * k1v
      k2u <- a * u2 - b * u2 * v2
      k2v <- d * u2 * v2 - g * v2
      u3 <- u + 0.5 * dt * k2u; v3 <- v + 0.5 * dt * k2v
      k3u <- a * u3 - b * u3 * v3
      k3v <- d * u3 * v3 - g * v3
      u4 <- u + dt * k3u; v4 <- v + dt * k3v
      k4u <- a * u4 - b * u4 * v4
      k4v <- d * u4 * v4 - g * v4
      u <- u + (dt / 6.0) * (k1u + 2.0 * k2u + 2.0 * k3u + k4u)
      v <- v + (dt / 6.0) * (k1v + 2.0 * k2v + 2.0 * k3v + k4v)
    }
    du <- u - obs[rec, 1L]
    dv <- v - obs[rec, 2L]
    loss <- loss + du * du + dv * dv
  }
  return(deriv(loss, theta))
}

loss_cpp <- ast2ast::translate(lv_loss)
grad_cpp <- ast2ast::translate(lv_grad, derivative = "reverse")

# -- data: integrate the truth, add noise -----------------------------------
lv_traj_R <- function(theta, y0, nrec, dt, substeps) {
  a <- theta[1]; b <- theta[2]; d <- theta[3]; g <- theta[4]
  u <- y0[1]; v <- y0[2]
  out <- matrix(0, nrec, 2)
  for (rec in 1:nrec) {
    for (s in 1:substeps) {
      k1u <- a*u - b*u*v;            k1v <- d*u*v - g*v
      u2 <- u + .5*dt*k1u; v2 <- v + .5*dt*k1v
      k2u <- a*u2 - b*u2*v2;         k2v <- d*u2*v2 - g*v2
      u3 <- u + .5*dt*k2u; v3 <- v + .5*dt*k2v
      k3u <- a*u3 - b*u3*v3;         k3v <- d*u3*v3 - g*v3
      u4 <- u + dt*k3u;    v4 <- v + dt*k3v
      k4u <- a*u4 - b*u4*v4;         k4v <- d*u4*v4 - g*v4
      u <- u + (dt/6)*(k1u + 2*k2u + 2*k3u + k4u)
      v <- v + (dt/6)*(k1v + 2*k2v + 2*k3v + k4v)
    }
    out[rec, ] <- c(u, v)
  }
  out
}
lv_loss_R <- function(theta, y0, obs, dt, substeps)
  sum((lv_traj_R(theta, y0, nrow(obs), dt, substeps) - obs)^2)

set.seed(1)
theta_true <- c(a = 1.1, b = 0.4, d = 0.1, g = 0.4)
y0   <- c(10, 5)
dt   <- 0.05
sub  <- 2L
nrec <- 120L
obs  <- lv_traj_R(theta_true, y0, nrec, dt, sub) + matrix(rnorm(nrec * 2, sd = 0.25), nrec, 2)

num_grad <- function(fn, th, h = 1e-6) vapply(seq_along(th), function(i) {
  tp <- th; tm <- th; tp[i] <- tp[i] + h; tm[i] <- tm[i] - h
  (fn(tp) - fn(tm)) / (2 * h)
}, numeric(1))

th0 <- c(0.9, 0.5, 0.15, 0.5)
stopifnot(
  isTRUE(all.equal(loss_cpp(th0, y0, obs, dt, sub), lv_loss_R(th0, y0, obs, dt, sub))),
  isTRUE(all.equal(as.vector(grad_cpp(th0, y0, obs, dt, sub)),
                   num_grad(function(th) lv_loss_R(th, y0, obs, dt, sub), th0),
                   tolerance = 1e-3))
)

# -- recover the parameters ------------------------------------------------
fit <- optim(th0,
             fn = function(th) loss_cpp(th, y0, obs, dt, sub),
             gr = function(th) as.vector(grad_cpp(th, y0, obs, dt, sub)),
             method = "L-BFGS-B", lower = rep(1e-3, 4))
rbind(true = theta_true, fitted = round(fit$par, 3))

# plain-double integrator: compiled loop vs interpreted R loop
microbenchmark::microbenchmark(
  loss_ast2ast = loss_cpp(th0, y0, obs, dt, sub),
  loss_R       = lv_loss_R(th0, y0, obs, dt, sub),
  times = 200
)

# exact reverse-mode gradient vs 8 finite-difference integrations
microbenchmark::microbenchmark(
  grad_ast2ast = grad_cpp(th0, y0, obs, dt, sub),
  grad_finitediff_R = num_grad(function(th) lv_loss_R(th, y0, obs, dt, sub), th0),
  times = 100
)
