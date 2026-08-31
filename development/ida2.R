types_f_ida <- function() {
  new_type(
    SolveD_HD_Result,
    slots(
      d |> type(vec(double)),
      hd |> type(vec(double))
    )
  )
  new_type(
    EquationParamsIda,
    slots(
      Kd |> type(double),
      Kg |> type(double),
      h0 |> type(double),
      d0 |> type(double),
      g |> type(double)
    )
  )
  new_type(
    AddParamsIda,
    slots(
      kd |> type(double),
      h0 |> type(double),
      d0 |> type(double),
      ga |> type(vec(double)),
      n_sigs |> type(int),
      signal |> type(vec(double))
    )
  )
  new_type(
    LossEval,
    slots(
      total_err |> type(double),
      betas |> type(vec(double))
    )
  )
  new_type(
    GridResult,
    slots(
      param_hat |> type(double),
      loss_hat |> type(double),
      betas |> type(vec(double))
    )
  )
}

# The coarse exponential grid search (vapro_grid_search's grid part, not the
# stats::optimize() local refinement -- that stays in R for now, it's cheap
# since it only calls the loss a handful more times, not nGrid times) run
# entirely in C++: every grid-point evaluation is a call to a nested fn(),
# not a round trip back to R. loss_fn's body is the same as
# loss_fct_ida_a2a's, just taking Kga as a scalar directly instead of
# through a length-1 `parameter` vector (that vector only existed for R's
# own stats::optimize() calling convention).
grid_search_ida_a2a <- function(lowerBounds, upperBounds, nGrid, add_params) {
  argtypes(
    lowerBounds |> type(double),
    upperBounds |> type(double),
    nGrid |> type(int),
    add_params |> type(AddParamsIda)
  )
  loss_fn <- fn(
    argtypes(
      Kga |> type(double),
      add_params |> type(AddParamsIda)
    ),
    return(LossEval),
    {
      eval_out |> type(LossEval)
      eval_out$betas <- numeric(3L * add_params$n_sigs)
      solve_h_ida <- fn(
        argtypes(
          Kd |> type(double) |> const(),
          Kg |> type(double) |> const(),
          h0 |> type(double) |> const(),
          d0 |> type(double) |> const(),
          g |> type(vec(double)) |> const()
        ),
        return(SolveD_HD_Result),
        {
          equation_h_ida <- fn(
            argtypes(
              h |> type(double),
              params |> type(EquationParamsIda)
            ),
            return(double),
            {
              if (h <= 0) {
                return(1.797693e+308)
              }
              denom_Kd <- 1 + params$Kd * h
              h_d <- (params$Kd * h * params$d0) / denom_Kd
              h_g <- 0
              if (params$g > 0) {
                denom_Kg <- 1 + params$Kg * h
                h_g <- (params$Kg * h * params$g) / denom_Kg
              }
              return(h + h_d + h_g - params$h0)
            }
          )

          n <- length(g)
          out |> type(SolveD_HD_Result)
          out$d <- numeric(n)
          out$hd <- numeric(n)

          params |> type(EquationParamsIda)
          params$Kd <- Kd
          params$Kg <- Kg
          params$h0 <- h0
          params$d0 <- d0

          for (i in 1L:n) {
            params$g <- g[i]
            ur <- uniroot(equation_h_ida, c(1e-20, h0), 1e-14, 200, params)
            if (is.na(ur$root) || ur$root <= 0) {
              out$d[i] <- NA
              out$hd[i] <- NA
            } else {
              d_sol <- d0 / (1 + Kd * ur$root)
              if (d_sol < 0) {
                out$d[i] <- NA
                out$hd[i] <- NA
              } else {
                out$d[i] <- d_sol
                out$hd[i] <- Kd * ur$root * d_sol
              }
            }
          }
          return(out)
        }
      )

      sol <- solve_h_ida(add_params$kd, Kga, add_params$h0, add_params$d0, add_params$ga)
      if (is.na(sol$d[1L])) {
        # eval_out$total_err <- 1.797693e+308
        return(eval_out)
      }

      n <- length(sol$d)
      X |> type(mat(double))
      X <- matrix(numeric(n * 3L), n, 3L)
      for (i in 1L:n) {
        X[i, 1L] <- 1
        X[i, 2L] <- sol$hd[i]
        X[i, 3L] <- sol$d[i]
      }

      eps <- 1e-12
      total_err <- 0
      for (s in 1L:add_params$n_sigs) {
        Xw |> type(mat(double))
        Xw <- matrix(numeric(n * 3L), n, 3L)
        yw <- numeric(n)
        for (i in 1L:n) {
          y_i <- add_params$signal[(s - 1L) * n + i]
          ay_i <- abs(y_i)
          w_i <- 0
          if (ay_i > eps) {
            w_i <- 1 / ay_i
          } else {
            w_i <- 1 / eps
          }
          Xw[i, 1L] <- X[i, 1L] * w_i
          Xw[i, 2L] <- X[i, 2L] * w_i
          Xw[i, 3L] <- X[i, 3L] * w_i
          yw[i] <- y_i * w_i
        }
        beta <- nnls(Xw, yw)
        pred <- c(X %*% beta)

        beta_base <- (s - 1L) * 3L
        eval_out$betas[beta_base + 1L] <- beta[1L]
        eval_out$betas[beta_base + 2L] <- beta[2L]
        eval_out$betas[beta_base + 3L] <- beta[3L]

        sig_err <- 0
        for (i in 1L:n) {
          obs <- add_params$signal[(s - 1L) * n + i]
          sig_err <- sig_err + abs(obs - pred[i])
        }
        total_err <- total_err + sig_err / n
      }
      # eval_out$total_err <- total_err / add_params$n_sigs
      return(eval_out)
    }
  )

  log_lower <- log(lowerBounds)
  log_upper <- log(upperBounds)
  step <- (log_upper - log_lower) / (nGrid - 1L)

  best_param <- lowerBounds
  best_eval |> type(LossEval)
  bestval <- loss_fn(lowerBounds, add_params)

  for (i in 1L:nGrid) {
    candidate <- exp(log_lower + (i - 1L) * step)
    cand_eval <- loss_fn(candidate, add_params)
    if (cand_eval$total_err < best_eval$total_err) {
      best_eval$total_err <- cand_eval$total_err
      best_eval$betas <- cand_eval$betas
      best_param <- candidate
    }
  }

  res |> type(GridResult)
  res$param_hat <- best_param
  res$loss_hat <- best_eval$total_err
  res$betas <- best_eval$betas
  return(res)
}

loss_a2a <- ast2ast::translate(grid_search_ida_a2a, types_f = types_f_ida)
