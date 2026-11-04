library(tinytest)

if (!at_home()) exit_file("slow: AD compiles; runs at home / CI only")

# --- y1' = -k y1, 0 = y1 + y2 - 1, theta = c(k, y10) ---------------------------
# y1 = y10 exp(-k t), y2 = 1 - y1; y2 in y0 is inconsistent on purpose

times <- seq(0, 2, by = 0.5)
k <- 0.7
y10 <- 2
e <- exp(-k * times)
expected <- cbind(
  c(-times * y10 * e, times * y10 * e),
  c(e, -e)
)

decay_dae <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  solution <- function(theta, times) {
    argtypes(theta |> type(vec(double)), times |> type(vec(double)))
    returntype(vec(double))
    residual <- function(t, y, yp, r, k) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        yp |> type(borrow_vec(double)) |> ref(),
        r |> type(borrow_vec(double)) |> ref(),
        k |> type(vec(double)) |> ref()
      )
      returntype(void)
      r[[1L]] <- yp[[1L]] + k[[1L]] * y[[1L]]
      r[[2L]] <- y[[1L]] + y[[2L]] - 1
    }
    k <- c(theta[[1L]])
    y0 <- c(theta[[2L]], 0.3)
    yp0 <- c(0, 0)
    id <- c(1, 0)
    c(ida(residual, y0, yp0, times, k, id, ida_options(reltol = 1e-10, abstol = 1e-12)))
  }
  jacobian(solution, theta, times)
}
fcpp <- ast2ast::translate(decay_dae, derivative = "forward")
expect_equal(unname(fcpp(c(k, y10), times)), expected, tolerance = 1e-6)
fcpp <- ast2ast::translate(decay_dae, derivative = "reverse")
expect_equal(unname(fcpp(c(k, y10), times)), expected, tolerance = 1e-6)

# with jac
decay_dae_jac <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  solution <- function(theta, times) {
    argtypes(theta |> type(vec(double)), times |> type(vec(double)))
    returntype(vec(double))
    residual <- function(t, y, yp, r, k) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        yp |> type(borrow_vec(double)) |> ref(),
        r |> type(borrow_vec(double)) |> ref(),
        k |> type(vec(double)) |> ref()
      )
      returntype(void)
      r[[1L]] <- yp[[1L]] + k[[1L]] * y[[1L]]
      r[[2L]] <- y[[1L]] + y[[2L]] - 1
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
      J[1L, 1L] <- k[[1L]] + cj
      J[2L, 1L] <- 1
      J[2L, 2L] <- 1
    }
    k <- c(theta[[1L]])
    y0 <- c(theta[[2L]], 0.3)
    yp0 <- c(0, 0)
    id <- c(1, 0)
    c(ida(residual, y0, yp0, times, k, id, ida_options(reltol = 1e-10, abstol = 1e-12), jac))
  }
  jacobian(solution, theta, times)
}
fcpp <- ast2ast::translate(decay_dae_jac, derivative = "forward")
expect_equal(unname(fcpp(c(k, y10), times)), expected, tolerance = 1e-6)
fcpp <- ast2ast::translate(decay_dae_jac, derivative = "reverse")
expect_equal(unname(fcpp(c(k, y10), times)), expected, tolerance = 1e-6)

# --- Lotka-Volterra with an algebraic product z = x y, vs finite differences ----
# theta = c(a, b, c, d, x0, y0)

lv_plain <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  residual <- function(t, y, yp, r, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      p |> type(vec(double)) |> ref()
    )
    returntype(void)
    r[[1L]] <- yp[[1L]] - (p[[1L]] * y[[1L]] - p[[2L]] * y[[3L]])
    r[[2L]] <- yp[[2L]] - (p[[4L]] * y[[3L]] - p[[3L]] * y[[2L]])
    r[[3L]] <- y[[3L]] - y[[1L]] * y[[2L]]
  }
  p <- theta[1L:4L]
  y0 <- c(theta[[5L]], theta[[6L]], 0)
  yp0 <- c(0, 0, 0)
  id <- c(1, 1, 0)
  c(ida(residual, y0, yp0, times, p, id, ida_options(reltol = 1e-10, abstol = 1e-12)))
}
lv_plain_cpp <- ast2ast::translate(lv_plain)

lv_jac <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  solution <- function(theta, times) {
    argtypes(theta |> type(vec(double)), times |> type(vec(double)))
    returntype(vec(double))
    residual <- function(t, y, yp, r, p) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        yp |> type(borrow_vec(double)) |> ref(),
        r |> type(borrow_vec(double)) |> ref(),
        p |> type(vec(double)) |> ref()
      )
      returntype(void)
      r[[1L]] <- yp[[1L]] - (p[[1L]] * y[[1L]] - p[[2L]] * y[[3L]])
      r[[2L]] <- yp[[2L]] - (p[[4L]] * y[[3L]] - p[[3L]] * y[[2L]])
      r[[3L]] <- y[[3L]] - y[[1L]] * y[[2L]]
    }
    p <- theta[1L:4L]
    y0 <- c(theta[[5L]], theta[[6L]], 0)
    yp0 <- c(0, 0, 0)
    id <- c(1, 1, 0)
    c(ida(residual, y0, yp0, times, p, id, ida_options(reltol = 1e-10, abstol = 1e-12)))
  }
  jacobian(solution, theta, times)
}

theta <- c(1.1, 0.4, 0.4, 0.1, 10, 5)
times <- seq(0, 5, by = 1)
h <- 1e-6
fd <- vapply(seq_along(theta), function(i) {
  tp <- theta
  tm <- theta
  tp[[i]] <- tp[[i]] + h
  tm[[i]] <- tm[[i]] - h
  (lv_plain_cpp(tp, times) - lv_plain_cpp(tm, times)) / (2 * h)
}, numeric(length(times) * 3L))
lv_jac_forward <- ast2ast::translate(lv_jac, derivative = "forward")
expect_equal(unname(lv_jac_forward(theta, times)), fd, tolerance = 1e-5)
lv_jac_reverse <- ast2ast::translate(lv_jac, derivative = "reverse")
expect_equal(unname(lv_jac_reverse(theta, times)), fd, tolerance = 1e-5)

# --- gradient() of a least-squares loss in a plain translation ----------------

lv_loss <- function(theta, data) {
  argtypes(theta |> type(vec(double)), data |> type(vec(double)))
  residual <- function(t, y, yp, r, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      p |> type(vec(double)) |> ref()
    )
    returntype(void)
    r[[1L]] <- yp[[1L]] - (p[[1L]] * y[[1L]] - p[[2L]] * y[[3L]])
    r[[2L]] <- yp[[2L]] - (p[[4L]] * y[[3L]] - p[[3L]] * y[[2L]])
    r[[3L]] <- y[[3L]] - y[[1L]] * y[[2L]]
  }
  p <- theta[1L:4L]
  y0 <- c(theta[[5L]], theta[[6L]], 0)
  yp0 <- c(0, 0, 0)
  id <- c(1, 1, 0)
  times <- c(0, 1, 2, 3, 4, 5)
  sol <- c(ida(residual, y0, yp0, times, p, id, ida_options(reltol = 1e-10, abstol = 1e-12)))
  c(sum((sol - data)^2))
}
lv_loss_cpp <- ast2ast::translate(lv_loss)

lv_gradient <- function(theta, data) {
  argtypes(theta |> type(vec(double)), data |> type(vec(double)))
  loss <- function(theta, data) {
    argtypes(theta |> type(vec(double)), data |> type(vec(double)))
    returntype(double)
    residual <- function(t, y, yp, r, p) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        yp |> type(borrow_vec(double)) |> ref(),
        r |> type(borrow_vec(double)) |> ref(),
        p |> type(vec(double)) |> ref()
      )
      returntype(void)
      r[[1L]] <- yp[[1L]] - (p[[1L]] * y[[1L]] - p[[2L]] * y[[3L]])
      r[[2L]] <- yp[[2L]] - (p[[4L]] * y[[3L]] - p[[3L]] * y[[2L]])
      r[[3L]] <- y[[3L]] - y[[1L]] * y[[2L]]
    }
    p <- theta[1L:4L]
    y0 <- c(theta[[5L]], theta[[6L]], 0)
    yp0 <- c(0, 0, 0)
    id <- c(1, 1, 0)
    times <- c(0, 1, 2, 3, 4, 5)
    sol <- c(ida(residual, y0, yp0, times, p, id, ida_options(reltol = 1e-10, abstol = 1e-12)))
    sum((sol - data)^2)
  }
  gradient(loss, theta, data)
}
lv_gradient_cpp <- ast2ast::translate(lv_gradient)

data <- c(10, 4, 2, 1.5, 2, 5, 5, 9, 8, 4, 2, 1.5, 50, 36, 16, 6, 4, 25)
fd <- vapply(seq_along(theta), function(i) {
  tp <- theta
  tm <- theta
  tp[[i]] <- tp[[i]] + h
  tm[[i]] <- tm[[i]] - h
  (lv_loss_cpp(tp, data) - lv_loss_cpp(tm, data)) / (2 * h)
}, numeric(1L))
expect_equal(lv_gradient_cpp(theta, data), fd, tolerance = 1e-5)

# --- adjoint ----------------------------------------------------------------------

# gradient of the LV loss with the adjoint vs FD (fd from above)
lv_gradient_adjoint <- function(theta, data) {
  argtypes(theta |> type(vec(double)), data |> type(vec(double)))
  loss <- function(theta, data) {
    argtypes(theta |> type(vec(double)), data |> type(vec(double)))
    returntype(double)
    residual <- function(t, y, yp, r, p) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        yp |> type(borrow_vec(double)) |> ref(),
        r |> type(borrow_vec(double)) |> ref(),
        p |> type(vec(double)) |> ref()
      )
      returntype(void)
      r[[1L]] <- yp[[1L]] - (p[[1L]] * y[[1L]] - p[[2L]] * y[[3L]])
      r[[2L]] <- yp[[2L]] - (p[[4L]] * y[[3L]] - p[[3L]] * y[[2L]])
      r[[3L]] <- y[[3L]] - y[[1L]] * y[[2L]]
    }
    p <- theta[1L:4L]
    y0 <- c(theta[[5L]], theta[[6L]], 0)
    yp0 <- c(0, 0, 0)
    id <- c(1, 1, 0)
    times <- c(0, 1, 2, 3, 4, 5)
    opts <- ida_options(reltol = 1e-10, abstol = 1e-12, sensitivity = "adjoint")
    sol <- c(ida(residual, y0, yp0, times, p, id, opts))
    sum((sol - data)^2)
  }
  gradient(loss, theta, data)
}
lv_gradient_adjoint_cpp <- ast2ast::translate(lv_gradient_adjoint)
expect_equal(lv_gradient_adjoint_cpp(theta, data), fd, tolerance = 1e-5)
expect_equal(lv_gradient_adjoint_cpp(theta, data), lv_gradient_cpp(theta, data), tolerance = 1e-6)

# equations in a different order than the variables (algebraic equation first),
# M depends on y, an output at t0 with an algebraic component: whole jacobian vs FD
dae_order_plain <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  residual <- function(t, y, yp, r, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      p |> type(vec(double)) |> ref()
    )
    returntype(void)
    r[[1L]] <- y[[3L]] - p[[3L]] * y[[1L]] * y[[1L]] - y[[2L]]
    r[[2L]] <- (1 + 0.1 * y[[2L]] * y[[2L]]) * yp[[1L]] + p[[1L]] * y[[1L]] - y[[3L]] * 0.2
    r[[3L]] <- yp[[2L]] + p[[2L]] * y[[2L]] - sin(t) * y[[1L]]
  }
  p <- theta[1L:3L]
  y0 <- c(theta[[4L]], theta[[5L]], 0)
  yp0 <- c(0, 0, 0)
  id <- c(1, 1, 0)
  c(ida(residual, y0, yp0, times, p, id, ida_options(reltol = 1e-10, abstol = 1e-12)))
}
dae_order_plain_cpp <- ast2ast::translate(dae_order_plain)

dae_order_jac <- function(theta, times) {
  argtypes(theta |> type(vec(double)), times |> type(vec(double)))
  solution <- function(theta, times) {
    argtypes(theta |> type(vec(double)), times |> type(vec(double)))
    returntype(vec(double))
    residual <- function(t, y, yp, r, p) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        yp |> type(borrow_vec(double)) |> ref(),
        r |> type(borrow_vec(double)) |> ref(),
        p |> type(vec(double)) |> ref()
      )
      returntype(void)
      r[[1L]] <- y[[3L]] - p[[3L]] * y[[1L]] * y[[1L]] - y[[2L]]
      r[[2L]] <- (1 + 0.1 * y[[2L]] * y[[2L]]) * yp[[1L]] + p[[1L]] * y[[1L]] - y[[3L]] * 0.2
      r[[3L]] <- yp[[2L]] + p[[2L]] * y[[2L]] - sin(t) * y[[1L]]
    }
    p <- theta[1L:3L]
    y0 <- c(theta[[4L]], theta[[5L]], 0)
    yp0 <- c(0, 0, 0)
    id <- c(1, 1, 0)
    opts <- ida_options(reltol = 1e-10, abstol = 1e-12, sensitivity = "adjoint")
    c(ida(residual, y0, yp0, times, p, id, opts))
  }
  jacobian(solution, theta, times)
}
dae_order_jac_cpp <- ast2ast::translate(dae_order_jac, derivative = "reverse")

theta_order <- c(0.5, 0.3, 0.4, 1.2, 0.8)
times_order <- c(0, 0.5, 1, 2)
# NOTE: h = 1e-6 with reltol = 1e-10 gives FD noise ~5e-5 for values ~1
h_order <- 1e-4
fd_order <- vapply(seq_along(theta_order), function(i) {
  tp <- theta_order
  tm <- theta_order
  tp[[i]] <- tp[[i]] + h_order
  tm[[i]] <- tm[[i]] - h_order
  (dae_order_plain_cpp(tp, times_order) - dae_order_plain_cpp(tm, times_order)) / (2 * h_order)
}, numeric(length(times_order) * 3L))
adjoint_order <- unname(dae_order_jac_cpp(theta_order, times_order))
expect_equal(adjoint_order, fd_order, tolerance = 1e-5)
# exact reference: the Dual path uses forward sensitivities
dae_order_forward_cpp <- ast2ast::translate(dae_order_jac, derivative = "forward")
expect_equal(adjoint_order, unname(dae_order_forward_cpp(theta_order, times_order)), tolerance = 1e-6)

# adjoint: an algebraic variable must not appear with yp
f <- function(theta) {
  argtypes(theta |> type(vec(double)))
  loss <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
    residual <- function(t, y, yp, r, k) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        yp |> type(borrow_vec(double)) |> ref(),
        r |> type(borrow_vec(double)) |> ref(),
        k |> type(vec(double)) |> ref()
      )
      returntype(void)
      r[[1L]] <- yp[[1L]] + k[[1L]] * y[[1L]]
      r[[2L]] <- y[[1L]] + y[[2L]] - 1 + 1e-3 * yp[[2L]]
    }
    k <- c(theta[[1L]])
    y0 <- c(theta[[2L]], 0)
    yp0 <- c(0, 0)
    id <- c(1, 0)
    times <- c(0, 1, 2)
    sum(ida(residual, y0, yp0, times, k, id, ida_options(sensitivity = "adjoint")))
  }
  gradient(loss, theta)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(c(0.7, 2)), "algebraic variable \\(id = 0\\) appears with its derivative yp")


# no hessian through ida
f <- function(theta) {
  argtypes(theta |> type(vec(double)))
  loss <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
    residual <- function(t, y, yp, r, k) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        yp |> type(borrow_vec(double)) |> ref(),
        r |> type(borrow_vec(double)) |> ref(),
        k |> type(vec(double)) |> ref()
      )
      returntype(void)
      r[[1L]] <- yp[[1L]] + k[[1L]] * y[[1L]]
    }
    k <- c(theta[[1L]])
    y0 <- c(theta[[2L]])
    yp0 <- c(0)
    id <- c(1)
    times <- c(0, 1, 2)
    sum(ida(residual, y0, yp0, times, k, id))
  }
  hessian(loss, theta)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(c(0.7, 2)), "hessian: not supported through ida")

# reverse mode: params has to be a double vector
f <- function(theta) {
  argtypes(theta |> type(vec(double)))
  residual <- function(t, y, yp, r, k) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      yp |> type(borrow_vec(double)) |> ref(),
      r |> type(borrow_vec(double)) |> ref(),
      k |> type(double) |> ref()
    )
    returntype(void)
    r[[1L]] <- yp[[1L]] + k * y[[1L]]
  }
  k <- theta[[1L]]
  y0 <- c(theta[[2L]])
  yp0 <- c(0)
  id <- c(1)
  times <- c(0, 1, 2)
  ida(residual, y0, yp0, times, k, id)
}
expect_error(ast2ast::translate(f, derivative = "reverse"),
             "ida: in reverse mode params \\(argument 5\\) has to be a double vector")
