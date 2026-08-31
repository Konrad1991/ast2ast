library(ast2ast)

# Translation of equation_h_dba() + solve_h_dba() from the host-guest binding
# example. Two changes from the original, both forced by ast2ast's
# constraints, noted inline:
#   - Kd/h0[i]/d0[i] are packaged into a struct and passed as uniroot()'s
#     5th ("extra") argument, since inner fn()s cannot capture outer locals
#     (no closure capture) -- ast2ast's uniroot() supports exactly this via
#     a two-arg f(x, extra) form.
#   - tryCatch(..., error = ...) is dropped: ast2ast's uniroot() already
#     returns uniroot_result::NA() when the interval doesn't bracket a root,
#     which is exactly what the tryCatch handler was catching.
# h0/d0 are assumed equal length here (no rep_len-based recycling -- ast2ast
# has no vectorized recycling primitive; if that mattered for real use,
# recycle before calling in, or ask for a loop-based rewrite of it too).
#
# lossFctDBA() is NOT translated -- it relies on R environments as a
# parameter bag, lists (lapply), negative indexing (parameter[-1]), dynamic
# matrix(..., nrow=3) reshaping of a variable-length parameter vector,
# Reduce/cbind, data.frame, and Map with a function passed as data. None of
# these exist in ast2ast's type system; translating it would mean making
# real modeling decisions (fixed n_sigs? which error_calc_fct?) rather than
# just a mechanical port.

types_f_dba <- function() {
  new_type(HDBAParams, slots(
    Kd |> type(double),
    h0 |> type(double),
    d0 |> type(double)
  ))
  new_type(HDBAResult, slots(
    d |> type(vec(double)),
    hd |> type(vec(double))
  ))
}

solve_h_dba <- function(Kd, h0, d0) {
  args(
    Kd |> type(double),
    h0 |> type(vec(double)),
    d0 |> type(vec(double))
  )
  equation_h_dba <- fn(
    args(
      h |> type(double) |> const(),
      extra |> type(HDBAParams) |> const()
    ),
    return(double),
    {
      if (h <= 0) {
        return(Inf)
      }
      denom_Kd <- 1 + extra$Kd * h
      if (denom_Kd == 0) {
        return(Inf)
      }
      h_d <- (extra$Kd * h * extra$d0) / denom_Kd
      residual <- h + h_d - extra$h0
      return(residual)
    }
  )

  params |> type(HDBAParams)
  result |> type(HDBAResult)

  n <- length(h0)
  d <- numeric(n)
  hd <- numeric(n)
  for (i in 1L:n) {
    d[[i]] <- NA
    hd[[i]] <- NA
  }

  for (i in 1L:n) {
    hi <- h0[[i]]
    if (hi <= 0) {
      hi <- 1e-10
    }
    params$Kd <- Kd
    params$h0 <- hi
    params$d0 <- d0[[i]]

    res <- uniroot(equation_h_dba, c(1e-20, hi), 1e-14, 1000, params)
    h_sol <- res$root

    if (is.na(h_sol) || h_sol <= 0) {
      next
    }
    d_sol <- d0[[i]] / (1 + Kd * h_sol)
    if (d_sol < 0) {
      next
    }
    d[[i]] <- d_sol
    hd[[i]] <- Kd * h_sol * d_sol
  }

  result$d <- d
  result$hd <- hd
  return(result)
}

fcpp_dba <- ast2ast::translate(solve_h_dba, types_f = types_f_dba)

# --- plain-R reference (tryCatch/ifelse kept, for comparison only) --------
equation_h_dba_ref <- function(h, Kd, h0, d0) {
  if (h <= 0) return(Inf)
  denom_Kd <- 1 + Kd * h
  if (denom_Kd == 0) return(Inf)
  h_d <- (Kd * h * d0) / denom_Kd
  h + h_d - h0
}
solve_h_dba_ref <- function(Kd, h0, d0) {
  n <- max(length(h0), length(d0))
  h0 <- rep_len(h0, n)
  h0 <- ifelse(h0 <= 0, 1e-10, h0)
  d0 <- rep_len(d0, n)
  d <- rep(NA_real_, n)
  hd <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    h_sol <- tryCatch(
      stats::uniroot(
        f = equation_h_dba_ref, lower = 1e-20, upper = h0[i], tol = 1e-14,
        Kd = Kd, h0 = h0[i], d0 = d0[i]
      )$root,
      error = function(e) NA_real_
    )
    if (is.na(h_sol) || h_sol <= 0) next
    d_sol <- d0[i] / (1 + Kd * h_sol)
    if (d_sol < 0) next
    d[i] <- d_sol
    hd[i] <- Kd * h_sol * d_sol
  }
  list(d = d, hd = hd)
}

Kd <- 1e4
h0 <- c(1e-3, 1e-4, 1e-5)
d0 <- c(1e-4, 1e-4, 1e-4)

res_etr <- fcpp_dba(Kd, h0, d0)
res_ref <- solve_h_dba_ref(Kd, h0, d0)
print(res_etr$d)
print(res_ref$d)
print(res_etr$hd)
print(res_ref$hd)

# ---------------------------------------------------------------------------
# lossFctDBA -- attempt, per your answer (n_sigs = number of signals,
# error_calc_fct = relative error). Design choices/substitutions, all forced
# by missing constructs, not judgment calls about the model itself:
#
#   - env (an R environment) -> a LossEnv struct {host, d0, signal}. n_sigs
#     is dropped as its own field -- it's always == ncol(env$signal), so
#     using ncol(signal) directly removes a value that could go out of sync.
#   - solve_h_dba is inlined into lossFctDBA's body (same code as above)
#     instead of called as a separate fn(), since fn()-calling-a-sibling-
#     fn() isn't a pattern I've confirmed works -- only equation_h_dba as a
#     single inner fn() of the outer translated function is confirmed.
#   - parameter[-1] reshaped via matrix(..., nrow=3) -> replaced with direct
#     arithmetic indexing into `parameter` (no negative indexing, no
#     reshape primitive exist). Same column-major layout as the original:
#     column j of the conceptual 3xN matrix is parameter[[2+3(j-1)]],
#     [[3+3(j-1)]], [[4+3(j-1)]].
#   - the n_sigs==1 vs n_sigs>1 branch is removed -- env$signal is always a
#     matrix (ncol=1 for a single signal), so the matrix-indexed path
#     handles both uniformly.
#   - lapply/Map/Reduce/list -> explicit loops writing into an `insilico`
#     matrix (n x n_sigs) instead of a list of vectors.
#   - anyNA(sol$d) -> manual loop + flag.
#   - .Machine$double.xmax -> literal 1e300 sentinel (same role: tell the
#     optimizer "bad fit", not a value it should be sensitive to).
#   - error_calc_fct: hardcoded as abs(pred - obs) / abs(obs) (relative
#     error) per your answer -- flag if you meant signed, not abs.
#   - mean(..., na.rm = TRUE) -> manual accumulator + count, skipping NA
#     entries on either side.
#   - the `eval = TRUE` branch (Reduce(cbind, ...) + data.frame) is DROPPED
#     entirely -- data.frame has no ast2ast representation, and it's a
#     diagnostic/plotting path, not part of the optimization objective.
#     lossFctDBA below always behaves like eval = FALSE.
#
# Not yet compiled/run -- these are new patterns (struct-field matrix
# read/write with computed indices, a longer loop body) beyond what's
# confirmed elsewhere in this file; please run it and paste whatever the
# compiler says.

types_f_loss <- function() {
  new_type(HDBAParams, slots(
    Kd |> type(double),
    h0 |> type(double),
    d0 |> type(double)
  ))
  new_type(LossEnv, slots(
    host |> type(vec(double)),
    d0 |> type(vec(double)),
    signal |> type(matrix(double))
  ))
}

lossFctDBA <- function(parameter, env) {
  args(
    parameter |> type(vec(double)),
    env |> type(LossEnv)
  )
  equation_h_dba <- fn(
    args(
      h |> type(double) |> const(),
      extra |> type(HDBAParams) |> const()
    ),
    return(double),
    {
      if (h <= 0) {
        return(Inf)
      }
      denom_Kd <- 1 + extra$Kd * h
      if (denom_Kd == 0) {
        return(Inf)
      }
      h_d <- (extra$Kd * h * extra$d0) / denom_Kd
      residual <- h + h_d - extra$h0
      return(residual)
    }
  )

  n <- length(env$host)
  n_sigs <- ncol(env$signal)
  Kd <- parameter[[1L]]

  params |> type(HDBAParams)

  d <- numeric(n)
  hd <- numeric(n)
  for (i in 1L:n) {
    d[[i]] <- NA
    hd[[i]] <- NA
  }

  for (i in 1L:n) {
    hi <- env$host[[i]]
    if (hi <= 0) {
      hi <- 1e-10
    }
    params$Kd <- Kd
    params$h0 <- hi
    params$d0 <- env$d0[[i]]

    res <- uniroot(equation_h_dba, c(1e-20, hi), 1e-14, 1000, params)
    h_sol <- res$root

    if (is.na(h_sol) || h_sol <= 0) {
      next
    }
    d_sol <- env$d0[[i]] / (1 + Kd * h_sol)
    if (d_sol < 0) {
      next
    }
    d[[i]] <- d_sol
    hd[[i]] <- Kd * h_sol * d_sol
  }

  has_na <- FALSE
  for (i in 1L:n) {
    if (is.na(d[[i]])) {
      has_na <- TRUE
    }
  }
  if (has_na) {
    return(1e300)
  }

  insilico <- matrix(0.0, n, n_sigs)
  for (j in 1L:n_sigs) {
    base_idx <- 1L + 3L * (j - 1L)
    p1 <- parameter[[base_idx + 1L]]
    p2 <- parameter[[base_idx + 2L]]
    p3 <- parameter[[base_idx + 3L]]
    for (i in 1L:n) {
      insilico[i, j] <- p1 + p2 * hd[[i]] + p3 * d[[i]]
    }
  }

  total <- 0.0
  count <- 0L
  for (j in 1L:n_sigs) {
    for (i in 1L:n) {
      obs <- env$signal[i, j]
      pred <- insilico[i, j]
      if (!is.na(obs) && !is.na(pred)) {
        rel_err <- abs(pred - obs) / abs(obs)
        if (!is.na(rel_err)) {
          total <- total + rel_err
          count <- count + 1L
        }
      }
    }
  }
  if (count == 0L) {
    return(1e300)
  }
  return(total / count)
}

fcpp_loss <- ast2ast::translate(lossFctDBA, types_f = types_f_loss)

signal_mat <- matrix(c(0.1, 0.3, 0.5, 0.2, 0.4, 0.6), nrow = 3, ncol = 2)
env_in <- structure(
  list(host = h0, d0 = d0, signal = signal_mat),
  class = "LossEnv"
)
parameter_in <- c(Kd, 0.01, 1, 0.5, 0.02, 1.2, 0.4)
print(fcpp_loss(parameter_in, env_in))
