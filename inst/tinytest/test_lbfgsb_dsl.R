library(tinytest)
library(ast2ast)

# lbfgsb(f, x, lower, upper, maxit, factr, pgtol, lmm): bound-constrained
# L-BFGS-B via R's own C routine. f: fn() taking a double vector, returning a
# scalar double; its gradient comes from etr::jacobian (forward or reverse).
# Every case is checked against stats::optim(method = "L-BFGS-B") on the exact
# same problem -- same bounds, same factr/pgtol/lmm/maxit (which are optim's
# own defaults), same analytic gradient.

# ---- 1. quadratic bowl: f(x) = (x1 - 1)^2 + (x2 + 2)^2 --------------------
quad_r  <- function(p) (p[1] - 1)^2 + (p[2] + 2)^2
quad_gr <- function(p) c(2 * (p[1] - 1), 2 * (p[2] + 2))

quad <- function(x, lo, up) {
  argtypes(x |> type(vec(double)), lo |> type(vec(double)), up |> type(vec(double)))
  loss <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    { return((v[[1]] - 1) * (v[[1]] - 1) + (v[[2]] + 2) * (v[[2]] + 2)) }
  )
  r <- lbfgsb(loss, x, lo, up, 100L, 1e7, 0, 5L)
  return(r$par)
}

quad_val <- function(x, lo, up) {
  argtypes(x |> type(vec(double)), lo |> type(vec(double)), up |> type(vec(double)))
  loss <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    { return((v[[1]] - 1) * (v[[1]] - 1) + (v[[2]] + 2) * (v[[2]] + 2)) }
  )
  r <- lbfgsb(loss, x, lo, up, 100L, 1e7, 0, 5L)
  return(r$value)
}

lo <- c(-10, -10)
up <- c(10, 10)
ref <- optim(c(0, 0), quad_r, quad_gr, method = "L-BFGS-B", lower = lo, upper = up)

quad_fwd <- translate(quad, derivative = "forward", verbose = FALSE)
expect_equal(quad_fwd(c(0, 0), lo, up), ref$par, tolerance = 1e-5)

quad_rev <- translate(quad, derivative = "reverse", verbose = FALSE)
expect_equal(quad_rev(c(0, 0), lo, up), ref$par, tolerance = 1e-5)

quad_val_fwd <- translate(quad_val, derivative = "forward", verbose = FALSE)
expect_equal(quad_val_fwd(c(0, 0), lo, up), ref$value, tolerance = 1e-6)

# ---- 2. active bound: same bowl shifted, box clips the minimum -----------
# f(x) = (x1 - 3)^2 + (x2 - 3)^2, upper = 1 -> minimiser pinned at (1, 1)
box_r  <- function(p) (p[1] - 3)^2 + (p[2] - 3)^2
box_gr <- function(p) c(2 * (p[1] - 3), 2 * (p[2] - 3))

box <- function(x, lo, up) {
  argtypes(x |> type(vec(double)), lo |> type(vec(double)), up |> type(vec(double)))
  loss <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    { return((v[[1]] - 3) * (v[[1]] - 3) + (v[[2]] - 3) * (v[[2]] - 3)) }
  )
  r <- lbfgsb(loss, x, lo, up, 100L, 1e7, 0, 5L)
  return(r$par)
}

lo2 <- c(-5, -5)
up2 <- c(1, 1)
ref2 <- optim(c(0, 0), box_r, box_gr, method = "L-BFGS-B", lower = lo2, upper = up2)

box_fwd <- translate(box, derivative = "forward", verbose = FALSE)
expect_equal(box_fwd(c(0, 0), lo2, up2), ref2$par, tolerance = 1e-6)
expect_equal(box_fwd(c(0, 0), lo2, up2), c(1, 1), tolerance = 1e-6)

# ---- 3. Rosenbrock: nonlinear, exercises the gradient wiring ------------
rosen_r <- function(p) 100 * (p[2] - p[1]^2)^2 + (1 - p[1])^2
rosen_gr <- function(p) c(
  -400 * p[1] * (p[2] - p[1]^2) - 2 * (1 - p[1]),
   200 * (p[2] - p[1]^2)
)

rosen <- function(x, lo, up) {
  argtypes(x |> type(vec(double)), lo |> type(vec(double)), up |> type(vec(double)))
  loss <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    {
      return(
        100 * (v[[2]] - v[[1]] * v[[1]]) * (v[[2]] - v[[1]] * v[[1]]) +
          (1 - v[[1]]) * (1 - v[[1]])
      )
    }
  )
  r <- lbfgsb(loss, x, lo, up, 300L, 1e7, 0, 10L)
  return(r$par)
}

lo3 <- c(-5, -5)
up3 <- c(5, 5)
ref3 <- optim(c(-1.2, 1), rosen_r, rosen_gr, method = "L-BFGS-B",
              lower = lo3, upper = up3, control = list(maxit = 300, lmm = 10))

rosen_fwd <- translate(rosen, derivative = "forward", verbose = FALSE)
expect_equal(rosen_fwd(c(-1.2, 1), lo3, up3), ref3$par, tolerance = 1e-4)
expect_equal(rosen_fwd(c(-1.2, 1), lo3, up3), c(1, 1), tolerance = 1e-3)

rosen_rev <- translate(rosen, derivative = "reverse", verbose = FALSE)
expect_equal(rosen_rev(c(-1.2, 1), lo3, up3), ref3$par, tolerance = 1e-4)

# ---- 4. convergence flag ---------------------------------------------
conv <- function(x, lo, up) {
  argtypes(x |> type(vec(double)), lo |> type(vec(double)), up |> type(vec(double)))
  loss <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    { return((v[[1]] - 1) * (v[[1]] - 1) + (v[[2]] + 2) * (v[[2]] + 2)) }
  )
  r <- lbfgsb(loss, x, lo, up, 100L, 1e7, 0, 5L)
  return(r$convergence)
}
conv_fwd <- translate(conv, derivative = "forward", verbose = FALSE)
expect_equal(conv_fwd(c(0, 0), lo, up), 0L)

# ---- 5. rejected without a derivative mode --------------------------
expect_error(
  translate(quad, getsource = TRUE),
  pattern = "lbfgsb requires"
)
