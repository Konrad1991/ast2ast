library(tinytest)

if (!at_home()) exit_file("slow: AD compiles; runs at home / CI only")

# --- independent normals, sd = c(1, 10, 0.1): moments + mass matrix -------------
gauss <- function(theta0, n_warmup, n_chains, n_threads) {
  argtypes(
    theta0 |> type(vec(double)),
    n_warmup |> type(int),
    n_chains |> type(int),
    n_threads |> type(int)
  )
  log_post <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
    sd <- c(1, 10, 0.1)
    -0.5 * sum((theta / sd)^2)
  }
  nuts(log_post, theta0, n_warmup, 2000L, n_chains, 42L, nuts_options(n_threads = n_threads))
}
gauss_cpp <- ast2ast::translate(gauss)
r <- gauss_cpp(c(1, 1, 0.1), 1000L, 4L, 1L)

expect_equal(class(r), "nuts_result")
expect_equal(dim(r$draws), c(2000L, 4L, 3L))
expect_equal(dim(r$lp), c(2000L, 4L))
expect_equal(dim(r$divergent), c(2000L, 4L))
expect_equal(length(r$step_size), 4L)
expect_equal(dim(r$inv_metric), c(3L, 4L))
expect_equal(sum(r$divergent), 0L)
expect_true(all(abs(apply(r$draws, 3, mean) / c(1, 10, 0.1)) < 0.1))
expect_equal(apply(r$draws, 3, sd), c(1, 10, 0.1), tolerance = 0.05)
# inverse mass matrix ~ posterior variances
expect_equal(rowMeans(r$inv_metric), c(1, 100, 0.01), tolerance = 0.15)
expect_equal(r$lp, -0.5 * apply(sweep(r$draws, 3, c(1, 10, 0.1), "/")^2, c(1, 2), sum))

# draws do not depend on the number of threads
expect_identical(gauss_cpp(c(1, 1, 0.1), 1000L, 4L, 3L), r)
# same seed -> same draws
expect_identical(gauss_cpp(c(1, 1, 0.1), 1000L, 4L, 1L), r)

# no warmup: the given step size is used as is
r0 <- gauss_cpp(c(1, 1, 0.1), 0L, 1L, 1L)
expect_equal(r0$step_size, 1)
expect_equal(c(r0$inv_metric), c(1, 1, 1))

# --- data argument: normal with mean from data --------------------------------
with_data <- function(theta0, data) {
  argtypes(theta0 |> type(vec(double)), data |> type(vec(double)))
  log_post <- function(theta, data) {
    argtypes(theta |> type(vec(double)), data |> type(vec(double)))
    returntype(double)
    -0.5 * sum((theta - data)^2)
  }
  nuts(log_post, theta0, 500L, 2000L, 2L, 1L, nuts_options(), data)
}
r <- ast2ast::translate(with_data)(c(0, 0), c(3, -2))
expect_equal(apply(r$draws, 3, mean), c(3, -2), tolerance = 0.05)

# --- cvode failure inside log_post counts as divergent ------------------------
# fixed-point ADAMS with 50 steps fails for large rates exp(theta)
ode_fail <- function(theta0) {
  argtypes(theta0 |> type(vec(double)))
  log_post <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
    rhs <- function(t, y, ydot, p) {
      argtypes(
        t |> type(double) |> ref(),
        y |> type(borrow_vec(double)) |> ref(),
        ydot |> type(borrow_vec(double)) |> ref(),
        p |> type(vec(double)) |> ref()
      )
      returntype(void)
      ydot[[1L]] <- -p[[1L]] * y[[1L]]
    }
    p <- exp(theta)
    sol <- cvode(rhs, c(1), c(0, 1), p, cvode_options(stiff = FALSE, max_steps = 50L))
    -0.5 * sum((theta / 3)^2) + 0 * sum(sol)
  }
  nuts(log_post, theta0, 200L, 500L, 1L, 1L)
}
r <- ast2ast::translate(ode_fail)(c(0))
expect_true(sum(r$divergent) > 0L)
expect_true(all(is.finite(r$draws)))

# --- errors -------------------------------------------------------------------
# other errors inside log_post stop the sampler
stop_inside <- function(theta0) {
  argtypes(theta0 |> type(vec(double)))
  log_post <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
    if (theta[[1L]] > 1) stop("theta too large")
    -0.5 * sum(theta^2)
  }
  nuts(log_post, theta0, 100L, 100L, 1L, 1L)
}
expect_error(ast2ast::translate(stop_inside)(c(0)), "theta too large")

not_finite <- function(theta0) {
  argtypes(theta0 |> type(vec(double)))
  log_post <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
    log(theta[[1L]])
  }
  nuts(log_post, theta0, 100L, 100L, 1L, 1L)
}
expect_error(ast2ast::translate(not_finite)(c(-1)), "not finite at theta0")

wrong_options <- function(theta0) {
  argtypes(theta0 |> type(vec(double)))
  log_post <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
    -0.5 * sum(theta^2)
  }
  nuts(log_post, theta0, 100L, 100L, 1L, 1L, cvode_options())
}
expect_error(ast2ast::translate(wrong_options), "created by nuts_options")

bad_options <- function(theta0) {
  argtypes(theta0 |> type(vec(double)))
  log_post <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
    -0.5 * sum(theta^2)
  }
  nuts(log_post, theta0, 100L, 100L, 1L, 1L, nuts_options(target_accept = 1.5))
}
expect_error(ast2ast::translate(bad_options)(c(0)), "target_accept has to be in \\(0, 1\\)")

returns_vector <- function(theta0) {
  argtypes(theta0 |> type(vec(double)))
  log_post <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(vec(double))
    theta^2
  }
  nuts(log_post, theta0, 100L, 100L, 1L, 1L)
}
expect_error(ast2ast::translate(returns_vector), "has to return a scalar double")

calls_lbfgsb <- function(theta0) {
  argtypes(theta0 |> type(vec(double)))
  inner <- function(x) {
    argtypes(x |> type(vec(double)))
    returntype(double)
    sum(x^2)
  }
  log_post <- function(theta) {
    argtypes(theta |> type(vec(double)))
    returntype(double)
    opt <- lbfgsb(inner, theta, -10, 10, 10L, 1e7, 0, 5L)
    -opt$value
  }
  nuts(log_post, theta0, 100L, 100L, 1L, 1L)
}
expect_error(ast2ast::translate(calls_lbfgsb), "cannot run in parallel")
