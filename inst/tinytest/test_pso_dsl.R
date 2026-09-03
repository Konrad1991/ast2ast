library(tinytest)
library(ast2ast)

# pso(f, lower, upper, ngen, npop, error_threshold, global): derivative-free
# particle-swarm optimisation. f: fn() taking a double vector, returning a
# scalar double. Stochastic -- every case fixes the RNG with set.seed() and
# checks the objective at the returned point rather than exact coordinates.
#
# All three objectives share one signature, so one dispatch TU (one compile).
#   1  2-D quadratic bowl   f = (x1 - 3)^2 + (x2 + 1)^2   -> argmin (3, -1)
#   2  2-D bounded optimum  f = (x1 - 10)^2 + (x2 - 10)^2 -> min clamped to box
#   3  4-D sphere           f = sum(x^2)                  -> shape follows length(lower)
TU <- function(test, lb, ub, ngen, npop, thr, glob) {
  argtypes(
    test |> type(int),
    lb |> type(vec(double)),
    ub |> type(vec(double)),
    ngen |> type(integer),
    npop |> type(integer),
    thr |> type(double),
    glob |> type(logical)
  )
  if (test == 1L) {
    loss1 <- fn(
      argtypes(v |> type(vec(double))),
      return(double),
      { return((v[[1]] - 3) * (v[[1]] - 3) + (v[[2]] + 1) * (v[[2]] + 1)) }
    )
    return(pso(loss1, lb, ub, ngen, npop, thr, glob))
  } else if (test == 2L) {
    loss2 <- fn(
      argtypes(v |> type(vec(double))),
      return(double),
      { return((v[[1]] - 10) * (v[[1]] - 10) + (v[[2]] - 10) * (v[[2]] - 10)) }
    )
    return(pso(loss2, lb, ub, ngen, npop, thr, glob))
  } else if (test == 3L) {
    loss3 <- fn(
      argtypes(v |> type(vec(double))),
      return(double),
      { return(v[[1]] * v[[1]] + v[[2]] * v[[2]] + v[[3]] * v[[3]] + v[[4]] * v[[4]]) }
    )
    return(pso(loss3, lb, ub, ngen, npop, thr, glob))
  } else {
    return(lb)
  }
}
fcpp <- translate(TU, verbose = FALSE)

# ---- 1. 2-D quadratic bowl, wide box -----------------------------------
obj1 <- function(p) (p[1] - 3)^2 + (p[2] + 1)^2
lb <- c(-10, -10)
ub <- c(10, 10)

set.seed(42)
got <- fcpp(1L, lb, ub, 300L, 50L, 1e-12, FALSE)
expect_equal(length(got), 2L)
expect_true(all(got >= lb - 1e-8 & got <= ub + 1e-8))
expect_true(obj1(got) < 0.1)

# ---- 2. same, global = TRUE ------------------------------------------
set.seed(42)
got_g <- fcpp(1L, lb, ub, 300L, 50L, 1e-12, TRUE)
expect_true(obj1(got_g) < 0.1)

# ---- 3. bounded optimum: true min outside the box, answer clamped -----
set.seed(7)
got_b <- fcpp(2L, c(-5, -5), c(5, 5), 300L, 50L, 1e-12, FALSE)
expect_true(max(abs(got_b - c(5, 5))) < 0.1)

# ---- 4. shape follows length(lower) -----------------------------------
set.seed(1)
got_nd <- fcpp(3L, rep(-3, 4), rep(3, 4), 200L, 60L, 1e-12, FALSE)
expect_equal(length(got_nd), 4L)
expect_true(sum(got_nd^2) < 0.2)

# ---- 5. derivative mode is rejected (pso is derivative-free) ----------
expect_error(
  translate(TU, derivative = "forward", getsource = TRUE),
  pattern = "pso does not support automatic differentiation"
)
