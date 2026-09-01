library(tinytest)
library(ast2ast)

# pso(f, lower, upper, ngen, npop, error_threshold, global): derivative-free
# particle-swarm optimisation. f: fn() taking a double vector, returning a
# scalar double. Stochastic -- every case fixes the RNG with set.seed() and
# checks the objective at the returned point rather than exact coordinates.

# ---- 1. 2-D quadratic bowl, wide box -------------------------------------
# f(x) = (x1 - 3)^2 + (x2 + 1)^2  -> argmin (3, -1)
pso_quad <- function(lb, ub, ngen, npop, thr, glob) {
  argtypes(
    lb |> type(vec(double)), ub |> type(vec(double)),
    ngen |> type(integer), npop |> type(integer),
    thr |> type(double), glob |> type(logical)
  )
  loss <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    { return((v[[1]] - 3) * (v[[1]] - 3) + (v[[2]] + 1) * (v[[2]] + 1)) }
  )
  return(pso(loss, lb, ub, ngen, npop, thr, glob))
}
pso_quad_c <- translate(pso_quad, verbose = FALSE)

obj1 <- function(p) (p[1] - 3)^2 + (p[2] + 1)^2
lb <- c(-10, -10)
ub <- c(10, 10)

set.seed(42)
got <- pso_quad_c(lb, ub, 300L, 50L, 1e-12, FALSE)
expect_equal(length(got), 2L)
expect_true(all(got >= lb - 1e-8 & got <= ub + 1e-8))
expect_true(obj1(got) < 0.1)

# ---- 2. same, global = TRUE -------------------------------------------
set.seed(42)
got_g <- pso_quad_c(lb, ub, 300L, 50L, 1e-12, TRUE)
expect_true(obj1(got_g) < 0.1)

# ---- 3. bounded optimum: true min outside the box, answer clamped ------
# f(x) = (x1 - 10)^2 + (x2 - 10)^2, box [-5, 5]^2 -> minimiser at (5, 5)
pso_box <- function(lb, ub, ngen, npop, thr, glob) {
  argtypes(
    lb |> type(vec(double)), ub |> type(vec(double)),
    ngen |> type(integer), npop |> type(integer),
    thr |> type(double), glob |> type(logical)
  )
  loss <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    { return((v[[1]] - 10) * (v[[1]] - 10) + (v[[2]] - 10) * (v[[2]] - 10)) }
  )
  return(pso(loss, lb, ub, ngen, npop, thr, glob))
}
pso_box_c <- translate(pso_box, verbose = FALSE)

set.seed(7)
got_b <- pso_box_c(c(-5, -5), c(5, 5), 300L, 50L, 1e-12, FALSE)
expect_true(max(abs(got_b - c(5, 5))) < 0.1)

# ---- 4. shape follows length(lower) --------------------------------------
pso_nd <- function(lb, ub, ngen, npop, thr, glob) {
  argtypes(
    lb |> type(vec(double)), ub |> type(vec(double)),
    ngen |> type(integer), npop |> type(integer),
    thr |> type(double), glob |> type(logical)
  )
  loss <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    { return(v[[1]] * v[[1]] + v[[2]] * v[[2]] + v[[3]] * v[[3]] + v[[4]] * v[[4]]) }
  )
  return(pso(loss, lb, ub, ngen, npop, thr, glob))
}
pso_nd_c <- translate(pso_nd, verbose = FALSE)

set.seed(1)
got_nd <- pso_nd_c(rep(-3, 4), rep(3, 4), 200L, 60L, 1e-12, FALSE)
expect_equal(length(got_nd), 4L)
expect_true(sum(got_nd^2) < 0.2)

# ---- 5. derivative mode is rejected (pso is derivative-free) -----------
expect_error(
  translate(pso_quad, derivative = "forward", getsource = TRUE),
  pattern = "pso does not support automatic differentiation"
)
