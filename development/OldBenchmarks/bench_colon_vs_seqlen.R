# Follow-up to bench_ast2ast_vs_quickr.R workload 4 (manual triple-loop
# matmul), which showed ast2ast 20-28x slower than quickr there despite only
# ~6x slower on plain 1D numeric loops.
#
# Hypothesis (Konrad): not matrix indexing -- it's that `for (i in 1:n)`
# compiles to `for(const auto i : etr::colon(1, n))`, which materializes a
# full n-element array up front (R/Nodes.R:629-646, etr::colon in
# Allocation.hpp:135-163), while `for (i in seq_len(n))` is special-cased to
# a native counting loop with no allocation at all. In the nested matmul,
# the innermost `for (l in 1:k)` re-materializes a k-length array on every
# single (i, j) pair -- up to N*M unnecessary heap allocations.
#
# Three ast2ast variants of the same manual triple-loop matmul:
#   colon        - for (i in 1:n) for (j in 1:m) for (l in 1:k)   [baseline]
#   inner_seqlen - only the innermost (hottest) loop uses seq_len
#   all_seqlen   - all three loops use seq_len
# quickr and R included as reference points (same numbers as workload 4).

library(ast2ast)
library(quickr)
library(microbenchmark)

# keep the matmul_* sources as pure-R baselines; inject argtypes() only for translation
add_args <- function(f, arg_block) {
  body(f) <- as.call(c(as.name("{"), arg_block, as.list(body(f))[-1]))
  f
}

matmul_colon <- function(A, B) {
  n <- nrow(A)
  k <- ncol(A)
  m <- ncol(B)
  C <- matrix(0.0, n, m)
  for (i in 1:n) {
    for (j in 1:m) {
      s <- 0.0
      for (l in 1:k) {
        s <- s + A[i, l] * B[l, j]
      }
      C[i, j] <- s
    }
  }
  return(C)
}

matmul_inner_seqlen <- function(A, B) {
  n <- nrow(A)
  k <- ncol(A)
  m <- ncol(B)
  C <- matrix(0.0, n, m)
  for (i in 1:n) {
    for (j in 1:m) {
      s <- 0.0
      for (l in seq_len(k)) {
        s <- s + A[i, l] * B[l, j]
      }
      C[i, j] <- s
    }
  }
  return(C)
}

matmul_all_seqlen <- function(A, B) {
  n <- nrow(A)
  k <- ncol(A)
  m <- ncol(B)
  C <- matrix(0.0, n, m)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      s <- 0.0
      for (l in seq_len(k)) {
        s <- s + A[i, l] * B[l, j]
      }
      C[i, j] <- s
    }
  }
  return(C)
}

mm_args <- quote(argtypes(A |> type(mat(double)) |> ref(), B |> type(mat(double)) |> ref()))
mm_colon_ast2ast <- ast2ast::translate(add_args(matmul_colon, mm_args))
mm_inner_seqlen_ast2ast <- ast2ast::translate(add_args(matmul_inner_seqlen, mm_args))
mm_all_seqlen_ast2ast <- ast2ast::translate(add_args(matmul_all_seqlen, mm_args))

mm_quickr <- quickr::quick(function(A, B) {
  declare(type(A = double(NA, NA)), type(B = double(NA, NA)))
  n <- nrow(A)
  k <- ncol(A)
  m <- ncol(B)
  C <- matrix(0.0, n, m)
  for (i in 1:n) {
    for (j in 1:m) {
      s <- 0.0
      for (l in 1:k) {
        s <- s + A[i, l] * B[l, j]
      }
      C[i, j] <- s
    }
  }
  C
})

set.seed(1234)

bench_one <- function(N) {
  A <- matrix(rnorm(N * N), N, N)
  B <- matrix(rnorm(N * N), N, N)
  ref <- A %*% B
  stopifnot(all.equal(ref, mm_colon_ast2ast(A, B)))
  stopifnot(all.equal(ref, mm_inner_seqlen_ast2ast(A, B)))
  stopifnot(all.equal(ref, mm_all_seqlen_ast2ast(A, B)))
  stopifnot(all.equal(ref, mm_quickr(A, B)))
  microbenchmark(
    R                  = matmul_colon(A, B),
    ast2ast_colon      = mm_colon_ast2ast(A, B),
    ast2ast_innerseqlen = mm_inner_seqlen_ast2ast(A, B),
    ast2ast_allseqlen  = mm_all_seqlen_ast2ast(A, B),
    quickr             = mm_quickr(A, B),
    times = 15
  )
}

for (N in c(10, 50, 100, 200)) {
  cat(sprintf("\n--- N = %d ---\n", N))
  print(bench_one(N))
}
