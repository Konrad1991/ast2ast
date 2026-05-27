library(ast2ast)
library(microbenchmark)

set.seed(1234)

# ---------------------------------------------------------------------------
# 1. plain forward matmul: ast2ast vs base R
# ---------------------------------------------------------------------------
f_plain <- function(A, B) {
  res <- A %*% B
  return(res)
}
fp_args <- function(A, B) {
  A |> type(mat(double)) |> ref()
  B |> type(mat(double)) |> ref()
}
fp_cpp <- ast2ast::translate(f_plain, fp_args, verbose = TRUE)

# Correctness check at one size before benchmarking.
{
  A <- matrix(rnorm(50 * 30), 50, 30)
  B <- matrix(rnorm(30 * 40), 30, 40)
  Cref  <- A %*% B
  Cours <- fp_cpp(A, B)
  stopifnot(all.equal(as.numeric(Cref), as.numeric(Cours), tolerance = 1e-10))
  cat("plain matmul: results match base R\n")
}

bench_plain <- function(N) {
  A <- matrix(rnorm(N * N), N, N)
  B <- matrix(rnorm(N * N), N, N)
  microbenchmark::microbenchmark(
    base    = f_plain(A, B),
    ast2ast = fp_cpp(A, B),
    times = 50
  )
}

cat("\n=== plain forward matmul (ast2ast vs base R) ===\n")
for (N in c(10, 50, 200, 500, 1000)) {
  cat(sprintf("\n--- N = %d ---\n", N))
  print(bench_plain(N))
}

# ---------------------------------------------------------------------------
# 2. reverse-mode AD matmul: forward + tape + backward
# ---------------------------------------------------------------------------
#
# To exercise the full reverse pass we reduce A %*% B to a scalar (sum) and
# take the Jacobian w.r.t. A. The tape records one block-op leader + (M*N - 1)
# inert slots + a small reduction subtree, then `deriv` triggers reverse().
#
# Adjust the deriv call below to match the project's API if the signature
# differs from the back_deriv.R example.

f_rev <- function(A, B) {
  C <- c(A %*% B)
  s <- 0.0
  for(i in seq_len(length(C))) {
    s <- s + C[[i]]
  }
  res <- deriv(s, A)
  return(res)
}
fr_args <- function(A, B) {
  A |> type(mat(double))
  B |> type(mat(double))
}
fr_cpp <- ast2ast::translate(f_rev, fr_args, derivative = "reverse",
                             verbose = FALSE)

bench_reverse <- function(N) {
  A <- matrix(rnorm(N * N), N, N)
  B <- matrix(rnorm(N * N), N, N)
  microbenchmark::microbenchmark(
    reverse_ad = fr_cpp(A, B),
    times = 30
  )
}

cat("\n=== reverse-mode AD matmul (forward + backward) ===\n")
for (N in c(10, 50, 200, 500)) {
  cat(sprintf("\n--- N = %d ---\n", N))
  print(bench_reverse(N))
}

# ---------------------------------------------------------------------------
# 3. forward-mode AD matmul (Dual): three dgemms
# ---------------------------------------------------------------------------

f_fwd <- function(A, B) {
  C <- c(A %*% B)
  s <- 0.0
  for(i in seq_len(length(C))) {
    s <- s + C[[i]]
  }
  return(s)
}
ff_args <- function(A, B) {
  A |> type(mat(double))
  B |> type(mat(double))
}
ff_cpp <- ast2ast::translate(f_fwd, ff_args, derivative = "forward",
                             verbose = FALSE)

bench_forward <- function(N) {
  A <- matrix(rnorm(N * N), N, N)
  B <- matrix(rnorm(N * N), N, N)
  microbenchmark::microbenchmark(
    forward_ad = ff_cpp(A, B),
    times = 30
  )
}

cat("\n=== forward-mode AD matmul (Dual) ===\n")
for (N in c(10, 50, 200, 500)) {
  cat(sprintf("\n--- N = %d ---\n", N))
  print(bench_forward(N))
}

# ---------------------------------------------------------------------------
# 4. composite ratio: how much overhead does AD add over a plain matmul?
# ---------------------------------------------------------------------------

cat("\n=== AD overhead vs plain matmul (lower = better) ===\n")
for (N in c(50)) {
  A <- matrix(rnorm(N * N), N, N)
  B <- matrix(rnorm(N * N), N, N)
  res <- microbenchmark::microbenchmark(
    plain      = fp_cpp(A, B),
    forward_ad = ff_cpp(A, B),
    reverse_ad = fr_cpp(A, B),
    times = 10
  )
  print(res)
}
