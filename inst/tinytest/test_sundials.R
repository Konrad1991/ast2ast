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
  res <- cvode(decay, y0, times, k, 1e-8, 1e-10, TRUE)
  return(res)
}
fcpp <- ast2ast::translate(f)
times <- seq(0, 2, by = 0.5)
expect_equal(c(fcpp(1, times, 0.7)), exp(-0.7 * times), tolerance = 1e-6)

# --- cvode with jac: Robertson (stiff) ---------------------------------------

robertson <- function(y0, times, k, with_jac) {
  argtypes(
    y0 |> type(vec(double)), times |> type(vec(double)), k |> type(vec(double)),
    with_jac |> type(logical)
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
  if (with_jac) return(cvode(rhs, y0, times, k, 1e-8, 1e-12, TRUE, jac))
  return(cvode(rhs, y0, times, k, 1e-8, 1e-12, TRUE))
}
rcpp <- ast2ast::translate(robertson)
k <- c(0.04, 1e4, 3e7)
times <- c(0, 0.4, 4, 40)
out <- capture.output(res_jac <- rcpp(c(1, 0, 0), times, k, TRUE))
expect_true(length(out) > 0L) # jac was called
res_dq <- rcpp(c(1, 0, 0), times, k, FALSE)
expect_equal(res_jac, res_dq, tolerance = 1e-6)
expect_equal(rowSums(res_jac), rep(1, 4), tolerance = 1e-8)

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
    return(cvode(rhs, y0, times, 1.0, 1e-8, 1e-10, TRUE, jac))
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
  res <- ida(residual, y0, yp0, times, k, 1e-8, 1e-10, id)
  return(res)
}
fcpp <- ast2ast::translate(f)
res <- fcpp(c(1, 0.5), c(0, 0), times, 0.7, c(1, 0))
expect_equal(res[, 1], exp(-0.7 * times), tolerance = 1e-6)
expect_equal(res[, 2], 1 - exp(-0.7 * times), tolerance = 1e-6)

expect_error(fcpp(c(1, 0), c(0, 0), times, 0.7, c(1, 0.5)), pattern = "id entries must be 0")
expect_error(fcpp(c(1, 0), c(0, 0), times, 0.7, c(1, 0, 1)), pattern = "same length")

# --- ida: translation errors -------------------------------------------------

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
  res <- ida(residual, y0, y0, times, 1.0, 1e-8, 1e-10, id)
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
  res <- ida(residual, y0, y0, times, 1.0, 1e-8, 1e-10, id)
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
  res <- ida(residual, y0, y0, times, 1.0, 1e-8, 1e-10, 1.0)
  return(res)
}
expect_error(ast2ast::translate(f), pattern = "id \\(argument 8\\) has to be a double vector")
