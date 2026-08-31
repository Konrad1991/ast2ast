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
}

loss_fct_ida_a2a <- function(parameter, add_params) {
  args(
    parameter |> type(vec(double)),
    add_params |> type(AddParamsIda)
  )
  solve_h_ida <- fn(
    args(
      Kd |> type(double) |> const(),
      Kg |> type(double) |> const(),
      h0 |> type(double) |> const(),
      d0 |> type(double) |> const(),
      g |> type(vec(double)) |> const()
    ),
    return(SolveD_HD_Result),
    {
      equation_h_ida <- fn(
        args(
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

  sol <- solve_h_ida(add_params$kd, parameter[1L], add_params$h0, add_params$d0, add_params$ga)
  if (is.na(sol$d[1L])) {
    return(1.797693e+308)
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

    sig_err <- 0
    for (i in 1L:n) {
      obs <- add_params$signal[(s - 1L) * n + i]
      aobs <- abs(obs)
      denom <- eps
      if (aobs > eps) {
        denom <- aobs
      }
      sig_err <- sig_err + abs(obs - pred[i]) / denom
    }
    total_err <- total_err + sig_err
  }
  return(total_err / add_params$n_sigs)
}

# 1. installed ast2ast first
fcpp1 <- tryCatch(
  ast2ast::translate(f = loss_fct_ida_a2a, types_f = types_f_ida),
  error = function(e) conditionMessage(e)
)
print(fcpp1)

# 2. now shadow it with the dev-sourced version, same session
files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))
fcpp2 <- tryCatch(
  translate(f = loss_fct_ida_a2a, types_f = types_f_ida),
  error = function(e) conditionMessage(e)
)
print(fcpp2)

# 3. fails but why?
spec <- tsf:::vapro_a2a_spec("ida")
f <- spec$loss_fct
args_f <- spec$args_f
str(args_f) # is NULL
types_f <- spec$types_f
translate(
  f = f,
  types_f = types_f
)
