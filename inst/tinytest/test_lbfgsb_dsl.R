library(tinytest)
library(ast2ast)

# lbfgsb(f, x, lower, upper, maxit, factr, pgtol, lmm): bound-constrained
# L-BFGS-B via R's own C routine. f: fn() taking a double vector, returning a
# scalar double; its gradient comes from etr::jacobian (forward or reverse).
# Every case is checked against stats::optim(method = "L-BFGS-B") on the exact
# same problem -- same bounds, same factr/pgtol/lmm/maxit (which are optim's
# own defaults), same analytic gradient.
#
# One dispatch TU holds all four loss problems; it is translated once per
# derivative mode (forward / reverse / default finite-difference) instead of
# once per (problem x mode) pair.
#   1  quadratic bowl        -> r$par
#   2  quadratic bowl        -> r$value
#   3  active bound (box)    -> r$par   (minimiser clipped to the upper bound)
#   4  Rosenbrock           -> r$par
#   5  quadratic bowl        -> r$convergence
TU <- function(test, x, lo, up) {
  argtypes(
    test |> type(int),
    x |> type(vec(double)),
    lo |> type(vec(double)),
    up |> type(vec(double))
  )
  if (test == 1L) {
    loss1 <- fn(
      argtypes(v |> type(vec(double))),
      return(double),
      { return((v[[1]] - 1) * (v[[1]] - 1) + (v[[2]] + 2) * (v[[2]] + 2)) }
    )
    r <- lbfgsb(loss1, x, lo, up, 100L, 1e7, 0, 5L)
    return(r$par)
  } else if (test == 2L) {
    loss2 <- fn(
      argtypes(v |> type(vec(double))),
      return(double),
      { return((v[[1]] - 1) * (v[[1]] - 1) + (v[[2]] + 2) * (v[[2]] + 2)) }
    )
    r <- lbfgsb(loss2, x, lo, up, 100L, 1e7, 0, 5L)
    return(r$value)
  } else if (test == 3L) {
    loss3 <- fn(
      argtypes(v |> type(vec(double))),
      return(double),
      { return((v[[1]] - 3) * (v[[1]] - 3) + (v[[2]] - 3) * (v[[2]] - 3)) }
    )
    r <- lbfgsb(loss3, x, lo, up, 100L, 1e7, 0, 5L)
    return(r$par)
  } else if (test == 4L) {
    loss4 <- fn(
      argtypes(v |> type(vec(double))),
      return(double),
      {
        return(
          100 * (v[[2]] - v[[1]] * v[[1]]) * (v[[2]] - v[[1]] * v[[1]]) +
            (1 - v[[1]]) * (1 - v[[1]])
        )
      }
    )
    r <- lbfgsb(loss4, x, lo, up, 300L, 1e7, 0, 10L)
    return(r$par)
  } else if (test == 5L) {
    loss5 <- fn(
      argtypes(v |> type(vec(double))),
      return(double),
      { return((v[[1]] - 1) * (v[[1]] - 1) + (v[[2]] + 2) * (v[[2]] + 2)) }
    )
    r <- lbfgsb(loss5, x, lo, up, 100L, 1e7, 0, 5L)
    return(r$convergence)
  } else {
    return(x)
  }
}
fwd <- translate(TU, derivative = "forward", verbose = FALSE)
rev <- translate(TU, derivative = "reverse", verbose = FALSE)
fd  <- translate(TU)

# ---- 1. quadratic bowl: f(x) = (x1 - 1)^2 + (x2 + 2)^2 --------------------
quad_r  <- function(p) (p[1] - 1)^2 + (p[2] + 2)^2
quad_gr <- function(p) c(2 * (p[1] - 1), 2 * (p[2] + 2))

lo <- c(-10, -10)
up <- c(10, 10)
ref <- optim(c(0, 0), quad_r, quad_gr, method = "L-BFGS-B", lower = lo, upper = up)

expect_equal(fwd(1L, c(0, 0), lo, up), ref$par, tolerance = 1e-5)
expect_equal(rev(1L, c(0, 0), lo, up), ref$par, tolerance = 1e-5)
expect_equal(fwd(2L, c(0, 0), lo, up), ref$value, tolerance = 1e-6)
expect_equal(fd(1L, c(0, 0), lo, up), ref$par, tolerance = 1e-6)

# ---- 2. active bound: same bowl shifted, box clips the minimum -----------
# f(x) = (x1 - 3)^2 + (x2 - 3)^2, upper = 1 -> minimiser pinned at (1, 1)
box_r  <- function(p) (p[1] - 3)^2 + (p[2] - 3)^2
box_gr <- function(p) c(2 * (p[1] - 3), 2 * (p[2] - 3))

lo2 <- c(-5, -5)
up2 <- c(1, 1)
ref2 <- optim(c(0, 0), box_r, box_gr, method = "L-BFGS-B", lower = lo2, upper = up2)

expect_equal(fwd(3L, c(0, 0), lo2, up2), ref2$par, tolerance = 1e-6)
expect_equal(fwd(3L, c(0, 0), lo2, up2), c(1, 1), tolerance = 1e-6)
expect_equal(fd(3L, c(0, 0), lo2, up2), ref2$par, tolerance = 1e-6)

# ---- 3. Rosenbrock: nonlinear, exercises the gradient wiring ------------
rosen_r <- function(p) 100 * (p[2] - p[1]^2)^2 + (1 - p[1])^2
rosen_gr <- function(p) c(
  -400 * p[1] * (p[2] - p[1]^2) - 2 * (1 - p[1]),
   200 * (p[2] - p[1]^2)
)

lo3 <- c(-5, -5)
up3 <- c(5, 5)
ref3 <- optim(c(-1.2, 1), rosen_r, rosen_gr, method = "L-BFGS-B",
              lower = lo3, upper = up3, control = list(maxit = 300, lmm = 10))

expect_equal(fwd(4L, c(-1.2, 1), lo3, up3), ref3$par, tolerance = 1e-4)
expect_equal(fwd(4L, c(-1.2, 1), lo3, up3), c(1, 1), tolerance = 1e-3)
expect_equal(rev(4L, c(-1.2, 1), lo3, up3), ref3$par, tolerance = 1e-4)
set.seed(1234)
expect_equal(fd(4L, c(-1.2, 1), lo3, up3), ref3$par, tolerance = 1e-2)

# ---- 4. convergence flag ---------------------------------------------
expect_equal(fwd(5L, c(0, 0), lo, up), 0L)
