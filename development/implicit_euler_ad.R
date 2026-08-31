implicit_euler <- function(yinit, tstart, tend, h) {
  n <- length(yinit)
  nsteps <- as.integer((tend - tstart) / h + 0.5)   # +0.5: 2/0.1 is 19.999..., round is not in the DSL
  tcurrent <- tstart
  ycurrent <- yinit
  # pre-allocated: row k is (t, y_1, ..., y_n); no dynamic growth in the DSL
  yres <- matrix(0.0, nsteps + 1L, n + 1L)
  yres[1L, 1L] <- tstart
  for (k in 1L:n) yres[1L, k + 1L] <- yinit[[k]]

  ode <- fn(
    args_f = function(y, t) {
      y |> type(vec(double)) |> ref() |> const()
      t |> type(double) |> const()
    },
    return_value = type(vec(double)),
    block = function(y, t) {
      # Van der Pol (autonomous, mu = 5 -- no closures in the DSL so it is a literal)
      dy <- numeric(2L)
      dy[[1L]] <- y[[2L]]
      dy[[2L]] <- 5.0 * (1.0 - y[[1L]] * y[[1L]]) * y[[2L]] - y[[1L]]
      return(dy)
    }
  )

  g <- fn(
    args_f = function(ycurrent, ynew, h, tcurrent) {
      ycurrent |> type(vec(double)) |> ref() |> const()
      ynew |> type(vec(double)) |> ref() |> const()
      h |> type(double)
      tcurrent |> type(double)
    },
    return_value = type(vec(double)),
    block = function(ycurrent, ynew, h, tcurrent) {
      ynew - h * ode(ynew, tcurrent) - ycurrent
    }
  )

  finite_differences <- fn(
    args_f = function(x, xnew, h, tcurrent) {
      x |> type(vec(double)) |> ref()
      xnew |> type(vec(double)) |> ref()
      h |> type(double) |> const()
      tcurrent |> type(double) |> const()
    },
    return_value = type(mat(double)),
    block = function(x, xnew, h, tcurrent) {
      fx <- g(x, xnew, h, tcurrent)
      jac <- matrix(0, length(fx), length(xnew))
      for (i in seq_along(xnew)) {
        xh <- xnew
        xh[i] <- xh[i] + h
        jac[, i] <- (g(x, xh, h, tcurrent) - fx) / h   # column i = d f / d x_i
      }
      return(jac)
    }
  )

  newton_raphson <- fn(
    args_f = function(x, xnew, h, tol, max_iter, tcurrent) {
      x |> type(vec(double)) |> ref()
      xnew |> type(vec(double)) |> ref()
      h |> type(double) |> const()
      tol |> type(double) |> const()
      max_iter |> type(int) |> const()
      tcurrent |> type(double) |> const()
    },
    return_value = type(vec(double)),
    block = function(x, xnew, h, tol, max_iter, tcurrent) {
      for (iter in 1:max_iter) {
        fx <- g(x, xnew, h, tcurrent)
        if (sqrt(sum(fx^2)) < tol) {
          return(xnew)
        }
        fx_deriv <- finite_differences(x, xnew, h, tcurrent)
        xnew <- xnew - solve(fx_deriv, fx)
      }
      stop("Did not converge")
      return(xnew) # Please compiler
    }

  )

  for (step in 1L:nsteps) {
    ynew_guess <- ycurrent + h * ode(ycurrent, tcurrent)
    ynew <- newton_raphson(ycurrent, ynew_guess, h = h, tol = 0.1, max_iter = 100L, tcurrent)
    tcurrent <- tcurrent + h
    ycurrent <- ynew
    yres[step + 1L, 1L] <- tcurrent
    for (k in 1L:n) yres[step + 1L, k + 1L] <- ynew[[k]]
  }
  return(yres)
}

args_f <- function(yinit, tstart, tend, h) {
  yinit |> type(vec(double))
  tstart |> type(double)
  tend |> type(double)
  h |> type(double)
}

fcpp <- ast2ast::translate(implicit_euler, args_f)

# --- same fixed-step scheme in plain R, for a fair benchmark ---------------
implicit_euler_R <- function(yinit, tstart, tend, h) {
  n <- length(yinit)
  nsteps <- as.integer((tend - tstart) / h + 0.5)
  vdp <- function(y, t) c(y[2], 5 * (1 - y[1]^2) * y[2] - y[1])
  g <- function(yc, yn, h, t) yn - h * vdp(yn, t) - yc
  fd_jac <- function(yc, yn, h, t) {
    fx <- g(yc, yn, h, t)
    J <- matrix(0, length(fx), length(yn))
    for (i in seq_along(yn)) {
      yh <- yn; yh[i] <- yh[i] + h
      J[, i] <- (g(yc, yh, h, t) - fx) / h
    }
    J
  }
  newton <- function(yc, yn, h, tol, maxit, t) {
    for (it in 1:maxit) {
      fx <- g(yc, yn, h, t)
      if (sqrt(sum(fx^2)) < tol) return(yn)
      yn <- yn - solve(fd_jac(yc, yn, h, t), fx)
    }
    stop("Did not converge")
  }
  out <- matrix(0, nsteps + 1L, n + 1L)
  out[1L, ] <- c(tstart, yinit)
  yc <- yinit; tc <- tstart
  for (step in 1L:nsteps) {
    yg <- yc + h * vdp(yc, tc)
    yn <- newton(yc, yg, h, 0.1, 100L, tc)
    tc <- tc + h; yc <- yn
    out[step + 1L, ] <- c(tc, yn)
  }
  out
}

yinit  <- c(2.0, 0.0)
tstart <- 0
tend   <- 500
h      <- 0.002

sol_cpp <- fcpp(yinit, tstart, tend, h)
sol_R   <- implicit_euler_R(yinit, tstart, tend, h)
stopifnot(isTRUE(all.equal(sol_cpp, sol_R, tolerance = 1e-6)))

microbenchmark::microbenchmark(
  ast2ast = fcpp(yinit, tstart, tend, h),
  plain_R = implicit_euler_R(yinit, tstart, tend, h),
  times = 50
)

if (interactive()) {
  plot(sol_cpp[, 1], sol_cpp[, 2], type = "l", xlab = "t", ylab = "y1",
       main = "Van der Pol, implicit Euler (ast2ast)")
}
