library(tinytest)

# --- cvode: y' = -k y --------------------------------------------------------

f <- function(y0, times, k) {
  argtypes(y0 |> type(vec(double)), times |> type(vec(double)), k |> type(vec(double)))
  decay <- function(t, y, ydot, k) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      k |> type(vec(double)) |> ref()
    )
    returntype(void)
    ydot[[1]] <- -k[[1]] * y[[1]]
  }
  res <- cvode(decay, y0, times, k, cvode_options(reltol = 1e-8, abstol = 1e-10))
  return(res)
}
fcpp <- ast2ast::translate(f)
times <- seq(0, 2, by = 0.5)
expect_equal(c(fcpp(1, times, 0.7)), exp(-0.7 * times), tolerance = 1e-6)

# --- cvode / ida options -------------------------------------------------------

f <- function(y0, times, k, test) {
  argtypes(y0 |> type(vec(double)), times |> type(vec(double)), k |> type(vec(double)),
           test |> type(int))
  decay <- function(t, y, ydot, k) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      k |> type(vec(double)) |> ref()
    )
    returntype(void)
    ydot[[1]] <- -k[[1]] * y[[1]]
  }
  if (test == 1L) return(cvode(decay, y0, times, k))
  if (test == 2L) return(cvode(decay, y0, times, k, cvode_options(stats = TRUE)))
  if (test == 3L) {
    opts <- cvode_options(1e-8, 1e-10, FALSE)
    return(cvode(decay, y0, times, k, opts))
  }
  if (test == 4L) return(cvode(decay, y0, times, k, cvode_options(max_steps = 2L)))
  return(cvode(decay, y0, times, k, cvode_options(reltol = -1)))
}
fcpp <- ast2ast::translate(f)
expect_equal(c(fcpp(1, times, 0.7, 1L)), exp(-0.7 * times), tolerance = 1e-4)
out <- capture.output(res <- fcpp(1, times, 0.7, 2L))
expect_true(any(grepl("cvode stats: steps", out)))
expect_equal(c(res), exp(-0.7 * times), tolerance = 1e-4)
expect_equal(c(fcpp(1, times, 0.7, 3L)), exp(-0.7 * times), tolerance = 1e-6)
expect_error(fcpp(1, times, 0.7, 4L), pattern = "integration failed")
expect_error(fcpp(1, times, 0.7, 5L), pattern = "reltol and abstol must be positive")
out <- capture.output(res <- fcpp(1, times, 0.7, 1L))
expect_identical(out, character()) # stats = FALSE by default

f <- function(y0) {
  argtypes(y0 |> type(vec(double)))
  opts <- cvode_options(stiff = c(TRUE, FALSE), linear_solver = "sundials")
  return(y0)
}
expect_error(ast2ast::translate(f), pattern = "cvode_options: stiff has to be a scalar logical")
f <- function(y0) {
  argtypes(y0 |> type(vec(double)))
  opts <- cvode_options(rtol = 1e-8)
  return(y0)
}
expect_error(ast2ast::translate(f), pattern = "Unknown argument 'rtol'")
f <- function(y0) {
  argtypes(y0 |> type(vec(double)))
  opts <- cvode_options(linear_solver = "klu")
  return(y0)
}
expect_error(ast2ast::translate(f), pattern = "linear_solver has to be \"sundials\" or \"lapack\"")
f <- function(y0) {
  argtypes(y0 |> type(vec(double)))
  opts <- ida_options(linear_solver = 2L)
  return(y0)
}
expect_error(ast2ast::translate(f), pattern = "linear_solver has to be \"sundials\" or \"lapack\"")

# --- params as an expression is rejected ------------------------------------------
f <- function(y0, times) {
  argtypes(y0 |> type(vec(double)), times |> type(vec(double)))
  decay <- function(t, y, ydot, k) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      k |> type(vec(double)) |> ref() |> const()
    )
    returntype(void)
    ydot[[1]] <- -k[[1]] * y[[1]]
  }
  return(cvode(decay, y0, times, c(0.7)))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "cvode: params \\(argument 4\\) has to be a variable")
f <- function(y0, times, id) {
  argtypes(y0 |> type(vec(double)), times |> type(vec(double)), id |> type(vec(double)))
  residual <- function(t, y, yp, r, k) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      k |> type(vec(double)) |> ref()
    )
    returntype(void)
    r[[1]] <- yp[[1]] + k[[1]] * y[[1]]
  }
  return(ida(residual, y0, y0, times, c(0.7), id))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "ida: params \\(argument 5\\) has to be a variable")

if (!at_home()) exit_file("slow: compiles; runs at home / CI only")

# --- cvode with jac: Robertson (stiff) ---------------------------------------

robertson <- function(y0, times, k, with_jac, lapack) {
  argtypes(
    y0 |> type(vec(double)), times |> type(vec(double)), k |> type(vec(double)),
    with_jac |> type(logical), lapack |> type(logical)
  )
  rhs <- function(t, y, ydot, k) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      k |> type(vec(double)) |> ref()
    )
    returntype(void)
    ydot[[1L]] <- -k[[1L]] * y[[1L]] + k[[2L]] * y[[2L]] * y[[3L]]
    ydot[[3L]] <- k[[3L]] * y[[2L]] * y[[2L]]
    ydot[[2L]] <- -ydot[[1L]] - ydot[[3L]]
  }
  jac <- function(t, y, J, k) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      J |> type(borrow_mat(double)) |> ref(),
      k |> type(vec(double)) |> ref()
    )
    returntype(void)
    print("jac")
    J[1L, 1L] <- -k[[1L]]
    J[1L, 2L] <- k[[2L]] * y[[3L]]
    J[1L, 3L] <- k[[2L]] * y[[2L]]
    J[3L, 2L] <- 2 * k[[3L]] * y[[2L]]
    J[2L, 1L] <- k[[1L]]
    J[2L, 2L] <- -k[[2L]] * y[[3L]] - 2 * k[[3L]] * y[[2L]]
    J[2L, 3L] <- -k[[2L]] * y[[2L]]
  }
  opts <- cvode_options(reltol = 1e-8, abstol = 1e-12)
  opts_lapack <- cvode_options(reltol = 1e-8, abstol = 1e-12, linear_solver = "lapack")
  if (lapack) return(cvode(rhs, y0, times, k, opts_lapack, jac))
  if (with_jac) return(cvode(rhs, y0, times, k, opts, jac))
  return(cvode(rhs, y0, times, k, opts))
}
rcpp <- ast2ast::translate(robertson)
k <- c(0.04, 1e4, 3e7)
times <- c(0, 0.4, 4, 40)
out <- capture.output(res_jac <- rcpp(c(1, 0, 0), times, k, TRUE, FALSE))
expect_true(length(out) > 0L) # jac was called
res_dq <- rcpp(c(1, 0, 0), times, k, FALSE, FALSE)
expect_equal(res_jac, res_dq, tolerance = 1e-6)
expect_equal(rowSums(res_jac), rep(1, 4), tolerance = 1e-8)
out <- capture.output(res_lapack <- rcpp(c(1, 0, 0), times, k, TRUE, TRUE))
expect_equal(res_lapack, res_jac, tolerance = 1e-6)

# --- cvode: jac translation errors -------------------------------------------

jac_case <- function(jac_args) {
  f <- function(y0, times) {
    argtypes(y0 |> type(vec(double)), times |> type(vec(double)))
    rhs <- function(t, y, ydot, p) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        ydot |> type(borrow_vec(double)) |> ref(),
        p |> type(double) |> ref()
      )
      returntype(void)
      ydot[[1L]] <- -y[[1L]]
    }
    jac <- JAC
    return(cvode(rhs, y0, times, 1.0, jac = jac))
  }
  body(f) <- do.call(substitute, list(body(f), list(JAC = jac_args)))
  f
}
jac_vec <- quote(function(t, y, J, p) {
  argtypes(
    t |> type(double) |> ref(),
    y |> type(borrow_vec(double)) |> ref(),
    J |> type(borrow_vec(double)) |> ref(),
    p |> type(double) |> ref()
  )
  returntype(void)
  J[[1L]] <- -1
})
expect_error(ast2ast::translate(jac_case(jac_vec), getsource = TRUE), pattern = "J has to be borrow_mat")
jac_three <- quote(function(t, y, J) {
  argtypes(
    t |> type(double) |> ref(),
    y |> type(borrow_vec(double)) |> ref(),
    J |> type(borrow_mat(double)) |> ref()
  )
  returntype(void)
  J[1L, 1L] <- -1
})
expect_error(ast2ast::translate(jac_case(jac_three), getsource = TRUE), pattern = "exactly 4 arguments")

# --- ida: y1' = -k y1, 0 = y1 + y2 - 1 ---------------------------------------
# y2 in y0 and yp0 are inconsistent on purpose: IDACalcIC has to correct them

f <- function(y0, yp0, times, k, id) {
  argtypes(
    y0 |> type(vec(double)), yp0 |> type(vec(double)), times |> type(vec(double)),
    k |> type(vec(double)), id |> type(vec(double))
  )
  residual <- function(t, y, yp, r, k) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      k |> type(vec(double)) |> ref()
    )
    returntype(void)
    r[[1]] <- yp[[1]] + k[[1]] * y[[1]]
    r[[2]] <- y[[1]] + y[[2]] - 1
  }
  res <- ida(residual, y0, yp0, times, k, id, ida_options(reltol = 1e-8, abstol = 1e-10))
  return(res)
}
fcpp <- ast2ast::translate(f)
res <- fcpp(c(1, 0.5), c(0, 0), times, 0.7, c(1, 0))
expect_equal(res[, 1], exp(-0.7 * times), tolerance = 1e-6)
expect_equal(res[, 2], 1 - exp(-0.7 * times), tolerance = 1e-6)

f_options <- function(y0, yp0, times, k, id, test) {
  argtypes(
    y0 |> type(vec(double)), yp0 |> type(vec(double)), times |> type(vec(double)),
    k |> type(vec(double)), id |> type(vec(double)), test |> type(int)
  )
  residual <- function(t, y, yp, r, k) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      k |> type(vec(double)) |> ref()
    )
    returntype(void)
    r[[1]] <- yp[[1]] + k[[1]] * y[[1]]
    r[[2]] <- y[[1]] + y[[2]] - 1
  }
  if (test == 1L) return(ida(residual, y0, yp0, times, k, id))
  if (test == 2L) return(ida(residual, y0, yp0, times, k, id, ida_options(stats = TRUE)))
  return(ida(residual, y0, yp0, times, k, id, ida_options(max_steps = 2L)))
}
fopt <- ast2ast::translate(f_options)
res <- fopt(c(1, 0.5), c(0, 0), times, 0.7, c(1, 0), 1L)
expect_equal(res[, 1], exp(-0.7 * times), tolerance = 1e-4)
out <- capture.output(fopt(c(1, 0.5), c(0, 0), times, 0.7, c(1, 0), 2L))
expect_true(any(grepl("ida stats: steps", out)))
expect_error(fopt(c(1, 0.5), c(0, 0), times, 0.7, c(1, 0), 3L), pattern = "integration failed")

expect_error(fcpp(c(1, 0), c(0, 0), times, 0.7, c(1, 0.5)), pattern = "id entries must be 0")
expect_error(fcpp(c(1, 0), c(0, 0), times, 0.7, c(1, 0, 1)), pattern = "same length")

# --- ida with jac: Robertson as DAE (y3 algebraic) ----------------------------

robertson_dae <- function(y0, yp0, times, k, id, with_jac, lapack) {
  argtypes(
    y0 |> type(vec(double)), yp0 |> type(vec(double)), times |> type(vec(double)),
    k |> type(vec(double)), id |> type(vec(double)), with_jac |> type(logical),
    lapack |> type(logical)
  )
  residual <- function(t, y, yp, r, k) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      k |> type(vec(double)) |> ref()
    )
    returntype(void)
    r[[1L]] <- yp[[1L]] + k[[1L]] * y[[1L]] - k[[2L]] * y[[2L]] * y[[3L]]
    r[[2L]] <- yp[[2L]] - k[[1L]] * y[[1L]] + k[[2L]] * y[[2L]] * y[[3L]] + k[[3L]] * y[[2L]] * y[[2L]]
    r[[3L]] <- y[[1L]] + y[[2L]] + y[[3L]] - 1
  }
  jac <- function(t, cj, y, yp, J, k) {
    argtypes(
      t |> type(double) |> ref(),
      cj |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      J |> type(borrow_mat(double)) |> ref(),
      k |> type(vec(double)) |> ref()
    )
    returntype(void)
    print("jac")
    J[1L, 1L] <- k[[1L]] + cj
    J[1L, 2L] <- -k[[2L]] * y[[3L]]
    J[1L, 3L] <- -k[[2L]] * y[[2L]]
    J[2L, 1L] <- -k[[1L]]
    J[2L, 2L] <- k[[2L]] * y[[3L]] + 2 * k[[3L]] * y[[2L]] + cj
    J[2L, 3L] <- k[[2L]] * y[[2L]]
    J[3L, 1L] <- 1
    J[3L, 2L] <- 1
    J[3L, 3L] <- 1
  }
  opts <- ida_options(reltol = 1e-8, abstol = 1e-12)
  opts_lapack <- ida_options(reltol = 1e-8, abstol = 1e-12, linear_solver = "lapack")
  if (lapack) return(ida(residual, y0, yp0, times, k, id, opts_lapack, jac))
  if (with_jac) return(ida(residual, y0, yp0, times, k, id, opts, jac))
  return(ida(residual, y0, yp0, times, k, id, opts))
}
rcpp <- ast2ast::translate(robertson_dae)
k <- c(0.04, 1e4, 3e7)
times <- c(0, 0.4, 4, 40)
args <- list(c(1, 0, 0), c(-0.04, 0.04, 0), times, k, c(1, 1, 0))
out <- capture.output(res_jac <- do.call(rcpp, c(args, TRUE, FALSE)))
expect_true(length(out) > 0L) # jac was called
res_dq <- do.call(rcpp, c(args, FALSE, FALSE))
expect_equal(res_jac, res_dq, tolerance = 1e-6)
expect_equal(rowSums(res_jac), rep(1, 4), tolerance = 1e-8)
out <- capture.output(res_lapack <- do.call(rcpp, c(args, TRUE, TRUE)))
expect_equal(res_lapack, res_jac, tolerance = 1e-6)

# --- ida: translation errors -------------------------------------------------

# jac without cj
f <- function(y0, times, id) {
  argtypes(y0 |> type(vec(double)), times |> type(vec(double)), id |> type(vec(double)))
  residual <- function(t, y, yp, r, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      p |> type(double) |> ref()
    )
    returntype(void)
    r[[1L]] <- yp[[1L]] + y[[1L]]
  }
  jac <- function(t, y, yp, J, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      J |> type(borrow_mat(double)) |> ref(),
      p |> type(double) |> ref()
    )
    returntype(void)
    J[1L, 1L] <- 1
  }
  res <- ida(residual, y0, y0, times, 1.0, id, jac = jac)
  return(res)
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "exactly 6 arguments \\(t, cj, y, yp, J, params\\)")

f <- function(y0, times, id) {
  argtypes(y0 |> type(vec(double)), times |> type(vec(double)), id |> type(vec(double)))
  residual <- function(t, y, yp, r) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref()
    )
    returntype(void)
    r[[1]] <- yp[[1]]
  }
  res <- ida(residual, y0, y0, times, 1.0, id)
  return(res)
}
expect_error(ast2ast::translate(f), pattern = "exactly 5 arguments")

f <- function(y0, times, id) {
  argtypes(y0 |> type(vec(double)), times |> type(vec(double)), id |> type(vec(double)))
  residual <- function(t, y, yp, r, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      p |> type(double) |> ref()
    )
    returntype(void)
    r[[1]] <- yp[[1]]
  }
  res <- ida(residual, y0, y0, times, 1.0, id)
  return(res)
}
expect_error(ast2ast::translate(f), pattern = "borrow_vec")

f <- function(y0, times) {
  argtypes(y0 |> type(vec(double)), times |> type(vec(double)))
  residual <- function(t, y, yp, r, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      p |> type(double) |> ref()
    )
    returntype(void)
    r[[1]] <- yp[[1]]
  }
  res <- ida(residual, y0, y0, times, 1.0, 1.0)
  return(res)
}
expect_error(ast2ast::translate(f), pattern = "id \\(argument 6\\) has to be a double vector")

# old positional tolerances are rejected
f <- function(y0, times, id) {
  argtypes(y0 |> type(vec(double)), times |> type(vec(double)), id |> type(vec(double)))
  residual <- function(t, y, yp, r, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      p |> type(double) |> ref()
    )
    returntype(void)
    r[[1]] <- yp[[1]]
  }
  res <- ida(residual, y0, y0, times, 1.0, id, 1e-8)
  return(res)
}
expect_error(ast2ast::translate(f), pattern = "options \\(argument 7\\) has to be created by ida_options")
