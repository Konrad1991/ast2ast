implicit_euler <- function(yinit, tstart, tend, h) {
  argtypes(
    yinit |> type(vec(double)),
    tstart |> type(double),
    tend |> type(double),
    h |> type(double)
  )
  n <- length(yinit)
  nsteps <- as.integer((tend - tstart) / h + 0.5)   # +0.5: 2/0.1 is 19.999..., round is not in the DSL
  tcurrent <- tstart
  ycurrent <- yinit
  yres <- matrix(0.0, nsteps + 1L, n + 1L)
  yres[1L, 1L] <- tstart
  for (k in seq_len(n)) yres[1L, k + 1L] <- yinit[[k]]

  # Van der Pol RHS, mu = 5. Fills `dy` in place -- no return-value allocation.
  ode <- fn(
    argtypes(
      dy |> type(vec(double)) |> ref(),
      y |> type(vec(double)) |> ref() |> const(),
      t |> type(double) |> const()
    ),
    return(void),
    {
      dy[[1L]] <- y[[2L]]
      dy[[2L]] <- 5.0 * (1.0 - y[[1L]] * y[[1L]]) * y[[2L]] - y[[1L]]
    }
  )

  # implicit-Euler residual  ynew - h*ode(ynew) - ycurrent  into `res`.
  # `dy` is caller-owned scratch for the ode() call.
  g <- fn(
    argtypes(
      res |> type(vec(double)) |> ref(),
      dy |> type(vec(double)) |> ref(),
      ycurrent |> type(vec(double)) |> ref() |> const(),
      ynew |> type(vec(double)) |> ref() |> const(),
      h |> type(double) |> const(),
      t |> type(double) |> const()
    ),
    return(void),
    {
      ode(dy, ynew, t)
      for (k in seq_len(length(ynew))) {
        res[[k]] <- ynew[[k]] - h * dy[[k]] - ycurrent[[k]]
      }
    }
  )

  # forward-difference Jacobian of g into `J` (mirrors the R fd_jac).
  # fx is g at the base point (already computed by the caller).
  finite_differences <- fn(
    argtypes(
      J |> type(mat(double)) |> ref(),
      fx |> type(vec(double)) |> ref() |> const(),
      fxh |> type(vec(double)) |> ref(),
      xh |> type(vec(double)) |> ref(),
      dy |> type(vec(double)) |> ref(),
      ycurrent |> type(vec(double)) |> ref() |> const(),
      ynew |> type(vec(double)) |> ref() |> const(),
      h |> type(double) |> const(),
      t |> type(double) |> const()
    ),
    return(void),
    {
      nn <- length(ynew)
      for (j in seq_len(nn)) {
        for (k in seq_len(nn)) xh[[k]] <- ynew[[k]]
        xh[[j]] <- xh[[j]] + h
        g(fxh, dy, ycurrent, xh, h, t)
        for (k in seq_len(nn)) J[k, j] <- (fxh[[k]] - fx[[k]]) / h
      }
    }
  )

  # Newton on g, LU solve via solve(). `xnew` updated in place to the solution.
  # fx / fxh / xh / dy / J are caller scratch.
  newton_raphson <- fn(
    argtypes(
      xnew |> type(vec(double)) |> ref(),
      ycurrent |> type(vec(double)) |> ref() |> const(),
      fx |> type(vec(double)) |> ref(),
      fxh |> type(vec(double)) |> ref(),
      xh |> type(vec(double)) |> ref(),
      dy |> type(vec(double)) |> ref(),
      J |> type(mat(double)) |> ref(),
      h |> type(double) |> const(),
      tol |> type(double) |> const(),
      max_iter |> type(int) |> const(),
      t |> type(double) |> const()
    ),
    return(void),
    {
      nn <- length(xnew)
      for (iter in seq_len(max_iter)) {
        g(fx, dy, ycurrent, xnew, h, t)
        if (sqrt(sum(fx * fx)) < tol) {
          break
        }
        finite_differences(J, fx, fxh, xh, dy, ycurrent, xnew, h, t)
        dx <- solve(J, fx)
        for (k in seq_len(nn)) xnew[[k]] <- xnew[[k]] - dx[[k]]
      }
    }
  )

  # step-loop scratch, allocated once
  dy <- numeric(n)
  fx <- numeric(n)
  fxh <- numeric(n)
  xh <- numeric(n)
  ynew <- numeric(n)
  J <- matrix(0.0, n, n)

  for (step in seq_len(nsteps)) {
    ode(dy, ycurrent, tcurrent)
    for (k in seq_len(n)) ynew[[k]] <- ycurrent[[k]] + h * dy[[k]]
    newton_raphson(ynew, ycurrent, fx, fxh, xh, dy, J, h, 1e-10, 100L, tcurrent)
    tcurrent <- tcurrent + h
    for (k in seq_len(n)) ycurrent[[k]] <- ynew[[k]]
    yres[step + 1L, 1L] <- tcurrent
    for (k in seq_len(n)) yres[step + 1L, k + 1L] <- ynew[[k]]
  }
  return(yres)
}

fcpp <- ast2ast::translate(implicit_euler)

# --- same fixed-step scheme in plain (idiomatic) R, for the baseline -------
implicit_euler_R <- function(yinit, tstart, tend, h) {
  n <- length(yinit)
  nsteps <- as.integer((tend - tstart) / h + 0.5)
  vdp <- function(y, t) c(y[2], 5 * (1 - y[1]^2) * y[2] - y[1])
  g <- function(yc, yn, h, t) yn - h * vdp(yn, t) - yc
  fd_jac <- function(yc, yn, fx, h, t) {
    J <- matrix(0, length(fx), length(yn))
    for (i in seq_along(yn)) {
      yh <- yn; yh[i] <- yh[i] + h
      J[, i] <- (g(yc, yh, h, t) - fx) / h
    }
    J
  }
  newton <- function(yc, yn, h, tol, maxit, t) {
    for (it in seq_len(maxit)) {
      fx <- g(yc, yn, h, t)
      if (sqrt(sum(fx^2)) < tol) return(yn)
      yn <- yn - solve(fd_jac(yc, yn, fx, h, t), fx)
    }
    stop("Did not converge")
  }
  out <- matrix(0, nsteps + 1L, n + 1L)
  out[1L, ] <- c(tstart, yinit)
  yc <- yinit; tc <- tstart
  for (step in seq_len(nsteps)) {
    yg <- yc + h * vdp(yc, tc)
    yn <- newton(yc, yg, h, 1e-10, 100L, tc)
    tc <- tc + h; yc <- yn
    out[step + 1L, ] <- c(tc, yn)
  }
  out
}

yinit <- c(2.0, 0.0)
tstart <- 0
tend <- 500
h <- 0.002              # 250000 implicit steps, a Newton solve at each

sol_cpp <- fcpp(yinit, tstart, tend, h)
sol_R <- implicit_euler_R(yinit, tstart, tend, h)

# same scheme, same result
cat("max abs diff ast2ast vs R:", max(abs(sol_cpp - sol_R)), "\n")
stopifnot(isTRUE(all.equal(sol_cpp, sol_R, tolerance = 1e-8)))

microbenchmark::microbenchmark(
  ast2ast = fcpp(yinit, tstart, tend, h),
  plain_R = implicit_euler_R(yinit, tstart, tend, h),
  times = 10
)
