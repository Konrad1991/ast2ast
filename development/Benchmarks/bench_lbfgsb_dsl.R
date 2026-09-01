# =============================================================================
# R optim(L-BFGS-B) vs ast2ast lbfgsb() -- same optimiser, different eval cost.
#
# ast2ast's lbfgsb() DSL op calls R's own C L-BFGS-B routine (src/appl/lbfgsb.c),
# the exact routine stats::optim() uses. So the optimiser is byte-identical on
# every contender below: same iteration path, same convergence, same par. The
# only thing that differs is how one f / grad-f evaluation is produced.
#
#   c1  optim + pure-R loss            + optim's own finite-difference gradient
#   c2  optim + compiled loss (translate) + compiled reverse-mode gradient
#   c3  ast2ast lbfgsb()              -- loss + gradient + optimiser, no R in loop
#
# Workload: Lotka-Volterra fitted with a hand-written RK4 loop. R cannot
# vectorise a time-stepper, so per-eval cost -- and thus the whole benchmark --
# is set by nrec * substeps. The loss integrates the true system (literal true
# params) next to the candidate and sums the trajectory mismatch; minimum is at
# theta_true = (1.1, 0.4, 0.1, 0.4), loss 0.
# =============================================================================
library(ast2ast)
library(microbenchmark)
cmpfun <- compiler::cmpfun

# ---- shared RK4 body -------------------------------------------------------
# One block, valid as plain R and as DSL: v[[i]] indexing and 1L:<lit> ranges
# work in both. nrec / substeps are injected as integer literals because the
# lbfgsb() loss cannot take them as arguments.
rk4_block <- function(nrec, substeps) {
  n <- as.integer(nrec)
  s <- as.integer(substeps)
  bquote({
    a <- v[[1L]]; b <- v[[2L]]; d <- v[[3L]]; g <- v[[4L]]
    aT <- 1.1; bT <- 0.4; dT <- 0.1; gT <- 0.4
    dt <- 0.05
    u <- 10.0; w <- 5.0
    uT <- 10.0; wT <- 5.0
    L <- 0.0
    for (rec in 1L:.(n)) {
      for (st in 1L:.(s)) {
        k1u <- a * u - b * u * w
        k1w <- d * u * w - g * w
        u2 <- u + 0.5 * dt * k1u; w2 <- w + 0.5 * dt * k1w
        k2u <- a * u2 - b * u2 * w2
        k2w <- d * u2 * w2 - g * w2
        u3 <- u + 0.5 * dt * k2u; w3 <- w + 0.5 * dt * k2w
        k3u <- a * u3 - b * u3 * w3
        k3w <- d * u3 * w3 - g * w3
        u4 <- u + dt * k3u; w4 <- w + dt * k3w
        k4u <- a * u4 - b * u4 * w4
        k4w <- d * u4 * w4 - g * w4
        u <- u + (dt / 6.0) * (k1u + 2.0 * k2u + 2.0 * k3u + k4u)
        w <- w + (dt / 6.0) * (k1w + 2.0 * k2w + 2.0 * k3w + k4w)
        j1u <- aT * uT - bT * uT * wT
        j1w <- dT * uT * wT - gT * wT
        p2 <- uT + 0.5 * dt * j1u; q2 <- wT + 0.5 * dt * j1w
        j2u <- aT * p2 - bT * p2 * q2
        j2w <- dT * p2 * q2 - gT * q2
        p3 <- uT + 0.5 * dt * j2u; q3 <- wT + 0.5 * dt * j2w
        j3u <- aT * p3 - bT * p3 * q3
        j3w <- dT * p3 * q3 - gT * q3
        p4 <- uT + dt * j3u; q4 <- wT + dt * j3w
        j4u <- aT * p4 - bT * p4 * q4
        j4w <- dT * p4 * q4 - gT * q4
        uT <- uT + (dt / 6.0) * (j1u + 2.0 * j2u + 2.0 * j3u + j4u)
        wT <- wT + (dt / 6.0) * (j1w + 2.0 * j2w + 2.0 * j3w + j4w)
      }
      du <- u - uT
      dw <- w - wT
      L <- L + du * du + dw * dw
    }
  })
}

# ---- build the four callables for one problem size -----------------------
build <- function(nrec, substeps) {
  stmts <- as.list(rk4_block(nrec, substeps))[-1L]   # drop the `{`

  as_fn <- function(head, tail)
    as.function(c(alist(v = ),
                  list(as.call(c(as.name("{"), head, stmts, tail)))))

  r_loss   <- cmpfun(as_fn(NULL, quote(L)))
  dsl_loss <- as_fn(quote(argtypes(v |> type(vec(double)))), quote(return(L)))
  dsl_grad <- as_fn(quote(argtypes(v |> type(vec(double)))), quote(return(deriv(L, v))))

  # outer wrapper: the lbfgsb() loss is an inner fn(); splice the same body in
  wrapper <- function(x, lo, up, maxit, factr, pgtol, lmm) {
    argtypes(
      x |> type(vec(double)), lo |> type(vec(double)), up |> type(vec(double)),
      maxit |> type(integer), factr |> type(double),
      pgtol |> type(double), lmm |> type(integer)
    )
    loss <- fn(argtypes(v |> type(vec(double))), return(double), LOSSBODY)
    r <- lbfgsb(loss, x, lo, up, maxit, factr, pgtol, lmm)
    return(r$par)
  }
  body(wrapper) <- do.call(substitute, list(
    body(wrapper),
    list(LOSSBODY = as.call(c(as.name("{"), stmts, quote(return(L)))))
  ))

  list(
    r_loss     = r_loss,
    tr_loss    = ast2ast::translate(dsl_loss, verbose = FALSE),
    tr_grad    = ast2ast::translate(dsl_grad, derivative = "reverse", verbose = FALSE),
    tr_lbfgsb  = ast2ast::translate(wrapper,  derivative = "reverse", verbose = FALSE)
  )
}

# ---- fixed optimiser settings (identical for every contender) ------------
X0     <- c(0.9, 0.5, 0.15, 0.5)
LO     <- rep(1e-3, 4)
UP     <- rep(10, 4)
MAXIT  <- 200L
FACTR  <- 1e7
PGTOL  <- 0
LMM    <- 5L
TRUTH  <- c(1.1, 0.4, 0.1, 0.4)
CTRL   <- list(maxit = MAXIT, factr = FACTR, pgtol = PGTOL, lmm = LMM)

fit_c1 <- function(k) optim(X0, k$r_loss, method = "L-BFGS-B",
                            lower = LO, upper = UP, control = CTRL)
fit_c2 <- function(k) optim(X0, function(v) k$tr_loss(v),
                            function(v) as.vector(k$tr_grad(v)),
                            method = "L-BFGS-B", lower = LO, upper = UP, control = CTRL)
fit_c3 <- function(k) k$tr_lbfgsb(X0, LO, UP, MAXIT, FACTR, PGTOL, LMM)

# ---- problem sizes ------------------------------------------------------
# large is the "hours in R" demonstrator: pure-R optim on it is projected,
# not run. Crank $substeps if your machine is fast.
SIZES <- list(
  small  = c(nrec = 100L,  substeps = 4L),
  medium = c(nrec = 300L,  substeps = 60L),
  large  = c(nrec = 1000L, substeps = 1500L)
)

run_size <- function(tag, cfg, run_pure_r) {
  cat(sprintf("\n================ %s : nrec=%d substeps=%d (%d RK4 steps / eval) ================\n",
              tag, cfg[["nrec"]], cfg[["substeps"]],
              cfg[["nrec"]] * cfg[["substeps"]]))
  k <- build(cfg[["nrec"]], cfg[["substeps"]])

  f3 <- fit_c3(k)
  f2 <- fit_c2(k)
  cat(sprintf("c3 par : %s\n", paste(sprintf('%.4f', f3), collapse = ", ")))
  cat(sprintf("c2 par : %s   (converged=%d, f/g counts=%d/%d)\n",
              paste(sprintf('%.4f', f2$par), collapse = ", "),
              f2$convergence, f2$counts[1], f2$counts[2]))
  stopifnot(max(abs(f3 - TRUTH))    < 1e-2,
            max(abs(f2$par - TRUTH)) < 1e-2,
            max(abs(f3 - f2$par))    < 1e-4)

  if (run_pure_r) {
    f1 <- fit_c1(k)
    cat(sprintf("c1 par : %s   (converged=%d, f/g counts=%d/%d)\n",
                paste(sprintf('%.4f', f1$par), collapse = ", "),
                f1$convergence, f1$counts[1], f1$counts[2]))
    stopifnot(max(abs(f1$par - TRUTH)) < 1e-2)
    mb <- microbenchmark(
      c1_R_optim_fd   = fit_c1(k),
      c2_optim_cpp    = fit_c2(k),
      c3_ast2ast_lbfgsb = fit_c3(k),
      times = 5
    )
    print(mb)
    m <- summary(mb)$median
    cat(sprintf("\nspeedup vs c1 (pure R): c2 = %.1fx , c3 = %.1fx\n",
                m[1] / m[2], m[1] / m[3]))
  } else {
    mb <- microbenchmark(
      c2_optim_cpp    = fit_c2(k),
      c3_ast2ast_lbfgsb = fit_c3(k),
      times = 3
    )
    print(mb)

    # project pure-R optim: convergence path is size-independent, so reuse the
    # small-size f/g counts. optim(gr=NULL) L-BFGS-B spends ~(n+1) loss evals
    # per gradient (1 value + n forward differences).
    ks <- build(SIZES$small[["nrec"]], SIZES$small[["substeps"]])
    cnt <- fit_c1(ks)$counts
    n   <- length(X0)
    proj_evals   <- cnt[1] + cnt[2] * (n + 1L)
    one_r_eval_s <- median(microbenchmark(k$r_loss(X0), times = 3)$time) / 1e9
    proj_r_s     <- one_r_eval_s * proj_evals
    m3_s         <- summary(mb)$median[2] / 1e3
    cat(sprintf("\none pure-R loss eval    : %.3f s\n", one_r_eval_s))
    cat(sprintf("projected optim evals   : %d  (f=%d, g=%d x (n+1))\n",
                as.integer(proj_evals), cnt[1], cnt[2]))
    cat(sprintf("projected c1 (pure R)   : %.0f s  (~ %.2f h)\n", proj_r_s, proj_r_s / 3600))
    cat(sprintf("measured  c3 (ast2ast)  : %.2f s\n", m3_s))
    cat(sprintf("projected speedup       : %.0fx\n", proj_r_s / m3_s))
  }
}

run_size("small",  SIZES$small,  run_pure_r = TRUE)
run_size("medium", SIZES$medium, run_pure_r = TRUE)
run_size("large",  SIZES$large,  run_pure_r = FALSE)
