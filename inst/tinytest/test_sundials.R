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
