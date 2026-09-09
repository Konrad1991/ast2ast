# cvode() (ast2ast + sundials) vs deSolve on three systems:
#   Lotka-Volterra   -- non-stiff, 2 eq
#   Robertson        -- stiff, 3 eq
#   POLLU            -- stiff, 20 eq (CWI test set)
# Same rtol/atol and integration method on both sides; compares the state
# trajectories and times them.
library(deSolve)
library(microbenchmark)

# --- shared driver ---------------------------------------------------------
run_case <- function(name, dsl, y0, times, params, reltol, abstol, stiff,
                     de_func, de_method) {
  fcpp <- ast2ast::translate(dsl)
  cv <- fcpp(y0, times, params, reltol, abstol, stiff)
  de <- ode(y0, times, de_func, params, method = de_method,
            rtol = reltol, atol = abstol)[, -1L, drop = FALSE]
  md <- max(abs(cv - de))
  rel <- max(abs(cv - de) / (abs(de) + 1e-30))
  cat(sprintf("%-15s max|abs| = %.2e   max|rel| = %.2e\n", name, md, rel))
  print(microbenchmark(
    cvode   = fcpp(y0, times, params, reltol, abstol, stiff),
    deSolve = ode(y0, times, de_func, params, method = de_method,
                  rtol = reltol, atol = abstol),
    times = 10
  ))
  cat("\n")
  invisible(list(cvode = cv, deSolve = de, max_abs = md))
}

# =========================================================================
# 1. Lotka-Volterra  (non-stiff)
# =========================================================================
lv <- function(y0, times, params, reltol, abstol, stiff) {
  argtypes(
    y0 |> type(vec(double)),
    times |> type(vec(double)),
    params |> type(vec(double)),
    reltol |> type(double),
    abstol |> type(double),
    stiff |> type(logical)
  )
  ode <- fn(
    argtypes(
      t |> type(double),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      params |> type(vec(double)) |> ref()
    ),
    return(void),
    {
      ydot[[1L]] <- y[[1L]] * params[[1L]] - y[[2L]] * y[[1L]] * params[[2L]]
      ydot[[2L]] <- y[[1L]] * y[[2L]] * params[[3L]] - y[[2L]] * params[[4L]]
    }
  )
  cvode(ode, y0, times, params, reltol, abstol, stiff)
}
lv_de <- function(t, y, p) {
  list(c(y[1] * p[1] - y[2] * y[1] * p[2],
         y[1] * y[2] * p[3] - y[2] * p[4]))
}
run_case("lotka-volterra", lv,
         y0 = c(10.0, 10.0), times = seq(1, 50, 0.5),
         params = c(1.1, 0.4, 0.1, 0.4),
         reltol = 1e-8, abstol = 1e-10, stiff = FALSE,
         de_func = lv_de, de_method = "adams")

# =========================================================================
# 2. Robertson  (stiff)
# =========================================================================
robertson <- function(y0, times, params, reltol, abstol, stiff) {
  argtypes(
    y0 |> type(vec(double)),
    times |> type(vec(double)),
    params |> type(vec(double)),
    reltol |> type(double),
    abstol |> type(double),
    stiff |> type(logical)
  )
  ode <- fn(
    argtypes(
      t |> type(double),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      params |> type(vec(double)) |> ref()
    ),
    return(void),
    {
      k1 <- params[[1L]]
      k2 <- params[[2L]]
      k3 <- params[[3L]]
      ydot[[1L]] <- -k1 * y[[1L]] + k2 * y[[2L]] * y[[3L]]
      ydot[[2L]] <- k1 * y[[1L]] - k2 * y[[2L]] * y[[3L]] - k3 * y[[2L]] * y[[2L]]
      ydot[[3L]] <- k3 * y[[2L]] * y[[2L]]
    }
  )
  cvode(ode, y0, times, params, reltol, abstol, stiff)
}
robertson_de <- function(t, y, p) {
  list(c(-p[1] * y[1] + p[2] * y[2] * y[3],
          p[1] * y[1] - p[2] * y[2] * y[3] - p[3] * y[2]^2,
          p[3] * y[2]^2))
}
run_case("robertson", robertson,
         y0 = c(1.0, 0.0, 0.0),
         times = c(0, 10^seq(-3, 5, length.out = 50)),
         params = c(0.04, 1e4, 3e7),
         reltol = 1e-6, abstol = 1e-10, stiff = TRUE,
         de_func = robertson_de, de_method = "bdf")

# =========================================================================
# 3. POLLU  (stiff, 20 eq) -- rate constants k1..k25 passed as params
# =========================================================================
pollu <- function(y0, times, params, reltol, abstol, stiff) {
  argtypes(
    y0 |> type(vec(double)),
    times |> type(vec(double)),
    params |> type(vec(double)),
    reltol |> type(double),
    abstol |> type(double),
    stiff |> type(logical)
  )
  ode <- fn(
    argtypes(
      t |> type(double),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      params |> type(vec(double)) |> ref()
    ),
    return(void),
    {
      r1 <- params[[1L]] * y[[1L]]
      r2 <- params[[2L]] * y[[2L]] * y[[4L]]
      r3 <- params[[3L]] * y[[5L]] * y[[2L]]
      r4 <- params[[4L]] * y[[7L]]
      r5 <- params[[5L]] * y[[7L]]
      r6 <- params[[6L]] * y[[7L]] * y[[6L]]
      r7 <- params[[7L]] * y[[9L]]
      r8 <- params[[8L]] * y[[9L]] * y[[6L]]
      r9 <- params[[9L]] * y[[11L]] * y[[2L]]
      r10 <- params[[10L]] * y[[11L]] * y[[1L]]
      r11 <- params[[11L]] * y[[13L]]
      r12 <- params[[12L]] * y[[10L]] * y[[2L]]
      r13 <- params[[13L]] * y[[14L]]
      r14 <- params[[14L]] * y[[1L]] * y[[6L]]
      r15 <- params[[15L]] * y[[3L]]
      r16 <- params[[16L]] * y[[4L]]
      r17 <- params[[17L]] * y[[4L]]
      r18 <- params[[18L]] * y[[16L]]
      r19 <- params[[19L]] * y[[16L]]
      r20 <- params[[20L]] * y[[17L]] * y[[6L]]
      r21 <- params[[21L]] * y[[19L]]
      r22 <- params[[22L]] * y[[19L]]
      r23 <- params[[23L]] * y[[1L]] * y[[4L]]
      r24 <- params[[24L]] * y[[19L]] * y[[1L]]
      r25 <- params[[25L]] * y[[20L]]

      ydot[[1L]] <- -r1 - r10 - r14 - r23 - r24 + r2 + r3 + r9 + r11 + r12 + r22 + r25
      ydot[[2L]] <- -r2 - r3 - r9 - r12 + r1 + r21
      ydot[[3L]] <- -r15 + r1 + r17 + r19 + r22
      ydot[[4L]] <- -r2 - r16 - r17 - r23 + r15
      ydot[[5L]] <- -r3 + r4 + r4 + r6 + r7 + r13 + r20
      ydot[[6L]] <- -r6 - r8 - r14 - r20 + r3 + r18 + r18
      ydot[[7L]] <- -r4 - r5 - r6 + r13
      ydot[[8L]] <- r4 + r5 + r6 + r7
      ydot[[9L]] <- -r7 - r8
      ydot[[10L]] <- -r12 + r7 + r9
      ydot[[11L]] <- -r9 - r10 + r8 + r11
      ydot[[12L]] <- r9
      ydot[[13L]] <- -r11 + r10
      ydot[[14L]] <- -r13 + r12
      ydot[[15L]] <- r14
      ydot[[16L]] <- -r18 - r19 + r16
      ydot[[17L]] <- -r20
      ydot[[18L]] <- r20
      ydot[[19L]] <- -r21 - r22 - r24 + r23 + r25
      ydot[[20L]] <- -r25 + r24
    }
  )
  cvode(ode, y0, times, params, reltol, abstol, stiff)
}
k_pollu <- c(0.350e0, 0.266e2, 0.123e5, 0.860e-3, 0.820e-3, 0.150e5, 0.130e-3,
  0.240e5, 0.165e5, 0.900e4, 0.220e-1, 0.120e5, 0.188e1, 0.163e5, 0.480e7,
  0.350e-3, 0.175e-1, 0.100e9, 0.444e12, 0.124e4, 0.210e1, 0.578e1,
  0.474e-1, 0.178e4, 0.312e1)
pollu_de <- function(t, y, k) {
  r <- numeric(25)
  r[1] <- k[1] * y[1];              r[2] <- k[2] * y[2] * y[4]
  r[3] <- k[3] * y[5] * y[2];       r[4] <- k[4] * y[7]
  r[5] <- k[5] * y[7];              r[6] <- k[6] * y[7] * y[6]
  r[7] <- k[7] * y[9];              r[8] <- k[8] * y[9] * y[6]
  r[9] <- k[9] * y[11] * y[2];      r[10] <- k[10] * y[11] * y[1]
  r[11] <- k[11] * y[13];           r[12] <- k[12] * y[10] * y[2]
  r[13] <- k[13] * y[14];           r[14] <- k[14] * y[1] * y[6]
  r[15] <- k[15] * y[3];            r[16] <- k[16] * y[4]
  r[17] <- k[17] * y[4];            r[18] <- k[18] * y[16]
  r[19] <- k[19] * y[16];           r[20] <- k[20] * y[17] * y[6]
  r[21] <- k[21] * y[19];           r[22] <- k[22] * y[19]
  r[23] <- k[23] * y[1] * y[4];     r[24] <- k[24] * y[19] * y[1]
  r[25] <- k[25] * y[20]
  list(c(
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
  ))
}
y0_pollu <- c(0, 0.2, 0, 0.04, 0, 0, 0.1, 0.3, 0.01, 0,
  0, 0, 0, 0, 0, 0, 0.007, 0, 0, 0)
run_case("pollu", pollu,
         y0 = y0_pollu, times = seq(0, 60, by = 0.5),
         params = k_pollu,
         reltol = 1e-8, abstol = 1e-8, stiff = TRUE,
         de_func = pollu_de, de_method = "bdf")
