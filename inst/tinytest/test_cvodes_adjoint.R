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
    c(cvode(decay, y0, times, k, cvode_options(reltol = 1e-10, abstol = 1e-12, sensitivity = "adjoint")))
  }
  jacobian(solution, theta, times)
}
fcpp <- ast2ast::translate(f, derivative = "reverse")
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
    c(cvode(decay, y0, times, k, cvode_options(reltol = 1e-10, abstol = 1e-12, stiff = FALSE, sensitivity = "adjoint")))
  }
  jacobian(solution, theta, times)
}
fcpp <- ast2ast::translate(f, derivative = "reverse")
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
    c(cvode(decay, y0, times, k, cvode_options(reltol = 1e-10, abstol = 1e-12, sensitivity = "adjoint"), decay_jac))
  }
  jacobian(solution, theta, times)
}
fcpp <- ast2ast::translate(f, derivative = "reverse")
expect_equal(unname(fcpp(c(k, y0), times)), expected, tolerance = 1e-6)

# --- Lotka-Volterra: gradient of a least-squares loss vs finite differences ----

lv_loss <- function(theta, data) {
  argtypes(theta |> type(vec(double)), data |> type(vec(double)))
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
  times <- c(0, 1, 2, 3, 4, 5)
  sol <- c(cvode(lv, y0, times, p, cvode_options(reltol = 1e-10, abstol = 1e-12)))
  c(sum((sol - data)^2))
}
lv_loss_cpp <- ast2ast::translate(lv_loss)

lv_grad <- function(theta, data) {
  argtypes(theta |> type(vec(double)), data |> type(vec(double)))
  loss <- function(theta, data) {
    argtypes(theta |> type(vec(double)), data |> type(vec(double)))
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
    times <- c(0, 1, 2, 3, 4, 5)
    sol <- c(cvode(lv, y0, times, p, cvode_options(reltol = 1e-10, abstol = 1e-12, sensitivity = "adjoint")))
    c(sum((sol - data)^2))
  }
  jacobian(loss, theta, data)
}
lv_grad_cpp <- ast2ast::translate(lv_grad, derivative = "reverse")

theta <- c(1.1, 0.4, 0.4, 0.1, 10, 5)
data <- c(10, 4, 2, 1.5, 2, 5, 5, 9, 8, 4, 2, 1.5)
h <- 1e-6
fd <- vapply(seq_along(theta), function(i) {
  tp <- theta
  tm <- theta
  tp[[i]] <- tp[[i]] + h
  tm[[i]] <- tm[[i]] - h
  (lv_loss_cpp(tp, data) - lv_loss_cpp(tm, data)) / (2 * h)
}, numeric(1L))
expect_equal(c(lv_grad_cpp(theta, data)), fd, tolerance = 1e-5)

# --- errors ---------------------------------------------------------------------

# a fn with borrow args can only be passed to cvode in reverse mode
f <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- function(y) {
    argtypes(y |> type(borrow_vec(double)) |> ref())
    returntype(double)
    y[[1L]]
  }
  g(x)
}
expect_error(ast2ast::translate(f, derivative = "reverse"), "borrow arguments")

# --- hessian through cvode (second-order adjoint) --------------------------------

# L = sum_t y0 exp(-k t), theta = c(k, y0)
times <- c(0, 1, 2)
expected <- matrix(c(
  y0 * sum(times^2 * exp(-k * times)), -sum(times * exp(-k * times)),
  -sum(times * exp(-k * times)), 0
), 2L, 2L)

# stiff (BDF, Newton)
f <- function(theta) {
  argtypes(theta |> type(vec(double)))
  loss <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
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
    sum(cvode(decay, y0, c(0, 1, 2), k, cvode_options(reltol = 1e-10, abstol = 1e-12)))
  }
  hessian(loss, theta)
}
fcpp <- ast2ast::translate(f)
expect_equal(unname(fcpp(c(k, y0))), expected, tolerance = 1e-6)

# non-stiff (ADAMS, fixed point)
f <- function(theta) {
  argtypes(theta |> type(vec(double)))
  loss <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
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
    sum(cvode(decay, y0, c(0, 1, 2), k, cvode_options(reltol = 1e-10, abstol = 1e-12, stiff = FALSE)))
  }
  hessian(loss, theta)
}
fcpp <- ast2ast::translate(f)
expect_equal(unname(fcpp(c(k, y0))), expected, tolerance = 1e-6)

# Lotka-Volterra loss: hessian vs finite differences of the adjoint gradient
lv_hess <- function(theta, data) {
  argtypes(theta |> type(vec(double)), data |> type(vec(double)))
  loss <- function(theta, data) {
    argtypes(theta |> type(vec(double)), data |> type(vec(double)))
    returntype(double)
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
    times <- c(0, 1, 2, 3, 4, 5)
    sol <- c(cvode(lv, y0, times, p, cvode_options(reltol = 1e-10, abstol = 1e-12)))
    sum((sol - data)^2)
  }
  hessian(loss, theta, data)
}
lv_hess_cpp <- ast2ast::translate(lv_hess)

h <- 1e-5
fd <- vapply(seq_along(theta), function(i) {
  tp <- theta
  tm <- theta
  tp[[i]] <- tp[[i]] + h
  tm[[i]] <- tm[[i]] - h
  c(lv_grad_cpp(tp, data) - lv_grad_cpp(tm, data)) / (2 * h)
}, numeric(length(theta)))
# gradient() in a plain translation: sensitivity = "auto" -> forward sensitivities (6 inputs),
# compared with the adjoint gradient of lv_grad_cpp
lv_gradient <- function(theta, data) {
  argtypes(theta |> type(vec(double)), data |> type(vec(double)))
  loss <- function(theta, data) {
    argtypes(theta |> type(vec(double)), data |> type(vec(double)))
    returntype(double)
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
    times <- c(0, 1, 2, 3, 4, 5)
    sol <- c(cvode(lv, y0, times, p, cvode_options(reltol = 1e-10, abstol = 1e-12)))
    sum((sol - data)^2)
  }
  gradient(loss, theta, data)
}
lv_gradient_cpp <- ast2ast::translate(lv_gradient)
expect_equal(lv_gradient_cpp(theta, data), c(lv_grad_cpp(theta, data)), tolerance = 1e-6)

# sensitivity = "forward", non-stiff, y0 constant: only k is active
f <- function(theta) {
  argtypes(theta |> type(vec(double)))
  loss <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
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
    y0 <- c(2)
    sum(cvode(decay, y0, c(0, 1, 2), k,
              cvode_options(reltol = 1e-10, abstol = 1e-12, stiff = FALSE, sensitivity = "forward")))
  }
  gradient(loss, theta)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(0.7), -2 * sum(c(0, 1, 2) * exp(-0.7 * c(0, 1, 2))), tolerance = 1e-6)

# unknown sensitivity mode
f <- function() {
  opts <- cvode_options(sensitivity = "backward")
  opts$reltol
}
expect_error(ast2ast::translate(f), "sensitivity has to be \"auto\" or \"forward\" or \"adjoint\"")

H <- lv_hess_cpp(theta, data)
expect_equal(unname(H), fd, tolerance = 1e-4)
expect_equal(H, t(H), tolerance = 1e-6)
