library(tinytest)

if (!at_home()) exit_file("slow: AD compiles; runs at home / CI only")

# --- y' = -k y, theta = c(k, y0): y = y0 exp(-k t) ----------------------------

times <- seq(0, 2, by = 0.5)
k <- 0.7
y0 <- 2
expected <- cbind(-times * y0 * exp(-k * times), exp(-k * times))

# stiff (BDF, Newton)
f <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  solution <- function(theta, times) {
    argtypes(theta |> type(vec(double)), times |> type(vec(double)))
    returntype(vec(double))
    decay <- function(t, y, ydot, k) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        ydot |> type(borrow_vec(double)) |> ref(),
        k |> type(vec(double)) |> ref()
      )
      returntype(void)
      ydot[[1L]] <- -k[[1L]] * y[[1L]]
    }
    k <- c(theta[[1L]])
    y0 <- c(theta[[2L]])
    c(cvode(decay, y0, times, k, cvode_options(reltol = 1e-10, abstol = 1e-12)))
  }
  jacobian(solution, theta, times)
}
fcpp <- ast2ast::translate(f, derivative = "forward")
expect_equal(unname(fcpp(c(k, y0), times)), expected, tolerance = 1e-6)

# non-stiff (ADAMS, fixed point)
f <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  solution <- function(theta, times) {
    argtypes(theta |> type(vec(double)), times |> type(vec(double)))
    returntype(vec(double))
    decay <- function(t, y, ydot, k) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        ydot |> type(borrow_vec(double)) |> ref(),
        k |> type(vec(double)) |> ref()
      )
      returntype(void)
      ydot[[1L]] <- -k[[1L]] * y[[1L]]
    }
    k <- c(theta[[1L]])
    y0 <- c(theta[[2L]])
    c(cvode(decay, y0, times, k, cvode_options(reltol = 1e-10, abstol = 1e-12, stiff = FALSE)))
  }
  jacobian(solution, theta, times)
}
fcpp <- ast2ast::translate(f, derivative = "forward")
expect_equal(unname(fcpp(c(k, y0), times)), expected, tolerance = 1e-6)

# stiff with user jac
f <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  solution <- function(theta, times) {
    argtypes(theta |> type(vec(double)), times |> type(vec(double)))
    returntype(vec(double))
    decay <- function(t, y, ydot, k) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        ydot |> type(borrow_vec(double)) |> ref(),
        k |> type(vec(double)) |> ref()
      )
      returntype(void)
      ydot[[1L]] <- -k[[1L]] * y[[1L]]
    }
    decay_jac <- function(t, y, J, k) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        J |> type(borrow_mat(double)) |> ref(),
        k |> type(vec(double)) |> ref()
      )
      returntype(void)
      J[1L, 1L] <- -k[[1L]]
    }
    k <- c(theta[[1L]])
    y0 <- c(theta[[2L]])
    c(cvode(decay, y0, times, k, cvode_options(reltol = 1e-10, abstol = 1e-12), decay_jac))
  }
  jacobian(solution, theta, times)
}
fcpp <- ast2ast::translate(f, derivative = "forward")
expect_equal(unname(fcpp(c(k, y0), times)), expected, tolerance = 1e-6)

# --- Lotka-Volterra, theta = c(a, b, c, d, x0, y0) vs finite differences -------

lv_plain <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  lv <- function(t, y, ydot, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      p |> type(vec(double)) |> ref()
    )
    returntype(void)
    ydot[[1L]] <- p[[1L]] * y[[1L]] - p[[2L]] * y[[1L]] * y[[2L]]
    ydot[[2L]] <- p[[4L]] * y[[1L]] * y[[2L]] - p[[3L]] * y[[2L]]
  }
  p <- theta[1L:4L]
  y0 <- theta[5L:6L]
  c(cvode(lv, y0, times, p, cvode_options(reltol = 1e-10, abstol = 1e-12)))
}
lv_plain_cpp <- ast2ast::translate(lv_plain)

lv_jac <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  solution <- function(theta, times) {
    argtypes(theta |> type(vec(double)), times |> type(vec(double)))
    returntype(vec(double))
    lv <- function(t, y, ydot, p) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        ydot |> type(borrow_vec(double)) |> ref(),
        p |> type(vec(double)) |> ref()
      )
      returntype(void)
      ydot[[1L]] <- p[[1L]] * y[[1L]] - p[[2L]] * y[[1L]] * y[[2L]]
      ydot[[2L]] <- p[[4L]] * y[[1L]] * y[[2L]] - p[[3L]] * y[[2L]]
    }
    p <- theta[1L:4L]
    y0 <- theta[5L:6L]
    c(cvode(lv, y0, times, p, cvode_options(reltol = 1e-10, abstol = 1e-12)))
  }
  jacobian(solution, theta, times)
}
lv_jac_cpp <- ast2ast::translate(lv_jac, derivative = "forward")

theta <- c(1.1, 0.4, 0.4, 0.1, 10, 5)
times <- seq(0, 5, by = 1)
h <- 1e-6
fd <- vapply(seq_along(theta), function(i) {
  tp <- theta
  tm <- theta
  tp[[i]] <- tp[[i]] + h
  tm[[i]] <- tm[[i]] - h
  (lv_plain_cpp(tp, times) - lv_plain_cpp(tm, times)) / (2 * h)
}, numeric(length(times) * 2L))
expect_equal(unname(lv_jac_cpp(theta, times)), fd, tolerance = 1e-5)

# --- reverse mode is not supported yet ------------------------------------------

expect_error(ast2ast::translate(lv_jac, derivative = "reverse"), "reverse")
