# POLLU -- the air-pollution model from the CWI stiff-IVP test set
# (Verwer 1994): 20 species, 25 reactions, t in [0, 60], stiff.
# AD variant of pollu.R: the ast2ast path gets the Newton Jacobian from
# forward-mode AD of the RHS (jacobian(ode_vec, y) -> I - h*J), the plain-R
# path keeps its forward-difference Jacobian as the oracle. Same solver
# structure otherwise.

implicit_euler <- function(yinit, tstart, tend, h) {
  argtypes(
    yinit |> type(vec(double)),
    tstart |> type(double),
    tend |> type(double),
    h |> type(double)
  )
  n <- length(yinit)
  nsteps <- as.integer((tend - tstart) / h + 0.5)
  tcurrent <- tstart
  ycurrent <- yinit
  yres <- matrix(0.0, nsteps + 1L, n + 1L)
  yres[1L, 1L] <- tstart
  for (k in seq_len(n)) yres[1L, k + 1L] <- yinit[[k]]

  # POLLU right-hand side, k1..k25 as literals. Fills `dy` in place.
  ode <- fn(
    argtypes(
      dy |> type(vec(double)) |> ref(),
      y |> type(vec(double)) |> ref() |> const(),
      t |> type(double) |> const()
    ),
    return(void),
    {
      r1 <- 0.350e0 * y[[1L]]
      r2 <- 0.266e2 * y[[2L]] * y[[4L]]
      r3 <- 0.123e5 * y[[5L]] * y[[2L]]
      r4 <- 0.860e-3 * y[[7L]]
      r5 <- 0.820e-3 * y[[7L]]
      r6 <- 0.150e5 * y[[7L]] * y[[6L]]
      r7 <- 0.130e-3 * y[[9L]]
      r8 <- 0.240e5 * y[[9L]] * y[[6L]]
      r9 <- 0.165e5 * y[[11L]] * y[[2L]]
      r10 <- 0.900e4 * y[[11L]] * y[[1L]]
      r11 <- 0.220e-1 * y[[13L]]
      r12 <- 0.120e5 * y[[10L]] * y[[2L]]
      r13 <- 0.188e1 * y[[14L]]
      r14 <- 0.163e5 * y[[1L]] * y[[6L]]
      r15 <- 0.480e7 * y[[3L]]
      r16 <- 0.350e-3 * y[[4L]]
      r17 <- 0.175e-1 * y[[4L]]
      r18 <- 0.100e9 * y[[16L]]
      r19 <- 0.444e12 * y[[16L]]
      r20 <- 0.124e4 * y[[17L]] * y[[6L]]
      r21 <- 0.210e1 * y[[19L]]
      r22 <- 0.578e1 * y[[19L]]
      r23 <- 0.474e-1 * y[[1L]] * y[[4L]]
      r24 <- 0.178e4 * y[[19L]] * y[[1L]]
      r25 <- 0.312e1 * y[[20L]]

      dy[[1L]] <- -r1 - r10 - r14 - r23 - r24 + r2 + r3 + r9 + r11 + r12 + r22 + r25
      dy[[2L]] <- -r2 - r3 - r9 - r12 + r1 + r21
      dy[[3L]] <- -r15 + r1 + r17 + r19 + r22
      dy[[4L]] <- -r2 - r16 - r17 - r23 + r15
      dy[[5L]] <- -r3 + r4 + r4 + r6 + r7 + r13 + r20
      dy[[6L]] <- -r6 - r8 - r14 - r20 + r3 + r18 + r18
      dy[[7L]] <- -r4 - r5 - r6 + r13
      dy[[8L]] <- r4 + r5 + r6 + r7
      dy[[9L]] <- -r7 - r8
      dy[[10L]] <- -r12 + r7 + r9
      dy[[11L]] <- -r9 - r10 + r8 + r11
      dy[[12L]] <- r9
      dy[[13L]] <- -r11 + r10
      dy[[14L]] <- -r13 + r12
      dy[[15L]] <- r14
      dy[[16L]] <- -r18 - r19 + r16
      dy[[17L]] <- -r20
      dy[[18L]] <- r20
      dy[[19L]] <- -r21 - r22 - r24 + r23 + r25
      dy[[20L]] <- -r25 + r24
    }
  )

  # value-returning RHS wrapper for jacobian(). POLLU is autonomous, so the
  # fixed t is irrelevant here -- pass 0.
  ode_vec <- fn(
    argtypes(y |> type(vec(double))),
    return(vec(double)),
    {
      d <- numeric(length(y))
      ode(d, y, 0.0)
      return(d)
    }
  )

  # implicit-Euler residual  ynew - h*ode(ynew) - ycurrent  into `res`.
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

  # Newton on g. Jacobian is I - h * d(ode)/d(ynew) from forward-mode AD.
  newton_raphson <- fn(
    argtypes(
      xnew |> type(vec(double)) |> ref(),
      ycurrent |> type(vec(double)) |> ref() |> const(),
      fx |> type(vec(double)) |> ref(),
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
        Jode <- jacobian(ode_vec, xnew)
        for (j in seq_len(nn)) {
          for (k in seq_len(nn)) J[k, j] <- -h * Jode[k, j]
        }
        for (k in seq_len(nn)) J[k, k] <- J[k, k] + 1.0
        dx <- solve(J, fx)
        for (k in seq_len(nn)) xnew[[k]] <- xnew[[k]] - dx[[k]]
      }
    }
  )

  # step-loop scratch, allocated once
  dy <- numeric(n)
  fx <- numeric(n)
  ynew <- numeric(n)
  J <- matrix(0.0, n, n)

  for (step in seq_len(nsteps)) {
    ode(dy, ycurrent, tcurrent)
    for (k in seq_len(n)) ynew[[k]] <- ycurrent[[k]] + h * dy[[k]]
    newton_raphson(ynew, ycurrent, fx, dy, J, h, 1e-10, 100L, tcurrent)
    tcurrent <- tcurrent + h
    for (k in seq_len(n)) ycurrent[[k]] <- ynew[[k]]
    yres[step + 1L, 1L] <- tcurrent
    for (k in seq_len(n)) yres[step + 1L, k + 1L] <- ynew[[k]]
  }
  return(yres)
}

fcpp <- ast2ast::translate(implicit_euler, derivative = "forward")

# --- same scheme in plain (idiomatic) R, forward-difference Jacobian -------
implicit_euler_R <- function(yinit, tstart, tend, h) {
  n <- length(yinit)
  nsteps <- as.integer((tend - tstart) / h + 0.5)

  k <- c(0.350e0, 0.266e2, 0.123e5, 0.860e-3, 0.820e-3, 0.150e5, 0.130e-3,
    0.240e5, 0.165e5, 0.900e4, 0.220e-1, 0.120e5, 0.188e1, 0.163e5, 0.480e7,
    0.350e-3, 0.175e-1, 0.100e9, 0.444e12, 0.124e4, 0.210e1, 0.578e1,
    0.474e-1, 0.178e4, 0.312e1)

  rhs <- function(y, t) {
    r <- numeric(25)
    r[1] <- k[1] * y[1]
    r[2] <- k[2] * y[2] * y[4]
    r[3] <- k[3] * y[5] * y[2]
    r[4] <- k[4] * y[7]
    r[5] <- k[5] * y[7]
    r[6] <- k[6] * y[7] * y[6]
    r[7] <- k[7] * y[9]
    r[8] <- k[8] * y[9] * y[6]
    r[9] <- k[9] * y[11] * y[2]
    r[10] <- k[10] * y[11] * y[1]
    r[11] <- k[11] * y[13]
    r[12] <- k[12] * y[10] * y[2]
    r[13] <- k[13] * y[14]
    r[14] <- k[14] * y[1] * y[6]
    r[15] <- k[15] * y[3]
    r[16] <- k[16] * y[4]
    r[17] <- k[17] * y[4]
    r[18] <- k[18] * y[16]
    r[19] <- k[19] * y[16]
    r[20] <- k[20] * y[17] * y[6]
    r[21] <- k[21] * y[19]
    r[22] <- k[22] * y[19]
    r[23] <- k[23] * y[1] * y[4]
    r[24] <- k[24] * y[19] * y[1]
    r[25] <- k[25] * y[20]
    c(
      -r[1] - r[10] - r[14] - r[23] - r[24] + r[2] + r[3] + r[9] + r[11] + r[12] + r[22] + r[25],
      -r[2] - r[3] - r[9] - r[12] + r[1] + r[21],
      -r[15] + r[1] + r[17] + r[19] + r[22],
      -r[2] - r[16] - r[17] - r[23] + r[15],
      -r[3] + r[4] + r[4] + r[6] + r[7] + r[13] + r[20],
      -r[6] - r[8] - r[14] - r[20] + r[3] + r[18] + r[18],
      -r[4] - r[5] - r[6] + r[13],
      r[4] + r[5] + r[6] + r[7],
      -r[7] - r[8],
      -r[12] + r[7] + r[9],
      -r[9] - r[10] + r[8] + r[11],
      r[9],
      -r[11] + r[10],
      -r[13] + r[12],
      r[14],
      -r[18] - r[19] + r[16],
      -r[20],
      r[20],
      -r[21] - r[22] - r[24] + r[23] + r[25],
      -r[25] + r[24]
    )
  }
  g <- function(yc, yn, h, t) yn - h * rhs(yn, t) - yc
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
    yg <- yc + h * rhs(yc, tc)
    yn <- newton(yc, yg, h, 1e-10, 100L, tc)
    tc <- tc + h; yc <- yn
    out[step + 1L, ] <- c(tc, yn)
  }
  out
}

yinit <- c(0, 0.2, 0, 0.04, 0, 0, 0.1, 0.3, 0.01, 0,
  0, 0, 0, 0, 0, 0, 0.007, 0, 0, 0)
tstart <- 0
tend <- 60
h <- 0.01

sol_cpp <- fcpp(yinit, tstart, tend, h)
sol_R <- implicit_euler_R(yinit, tstart, tend, h)

# not bit-identical anymore: AD Jacobian vs forward-difference Jacobian, same root
cat("max abs diff ast2ast(AD) vs R(fd):", max(abs(sol_cpp - sol_R)), "\n")
stopifnot(isTRUE(all.equal(sol_cpp, sol_R, tolerance = 1e-6)))

microbenchmark::microbenchmark(
  ast2ast = fcpp(yinit, tstart, tend, h),
  plain_R = implicit_euler_R(yinit, tstart, tend, h),
  times = 5
)
