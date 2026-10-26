# ast2ast vs quickr.
#
# armacmp is intentionally excluded here: last commit 2021-10-20, explicitly
# scoped to linear algebra only, unmaintained. quickr (Posit, CRAN, active)
# is the real ongoing competitor for the general transpile-R-code use case.
#
# Four workloads, chosen to separate "raw compiled-loop speed" from
# "BLAS/allocation overhead":
#   1. scalar loop            (leibniz series)
#   2. branchy scalar loop    (prime counting: while, %%, if)
#   3. vector reduction       (sqrt/sin/abs over a vector)
#   4. manual triple-loop matmul, no %*%  (isolates codegen, not BLAS)
#   5. A %*% B combined with elementwise ops (BLAS + allocation overhead)

library(ast2ast)

# the source functions below double as the plain-R baseline, so keep them
# free of DSL calls; inject the argtypes() prologue only for translation
add_args <- function(f, arg_block) {
  body(f) <- as.call(c(as.name("{"), arg_block, as.list(body(f))[-1]))
  f
}
library(quickr)
library(microbenchmark)

set.seed(1234)
# ---------------------------------------------------------------------------
# 1. scalar loop: leibniz series
# ---------------------------------------------------------------------------

leibniz <- function(rounds) {
  x <- 1.0
  pi <- 1.0
  i <- 2L
  while(i <= (rounds + 2)) {
    x <- x * -1
    pi <- pi + (x / (2 * i - 1))
    i <- i + 1L
  }
  pi <- pi * 4
  return(pi)
}
leibniz_ast2ast <- ast2ast::translate(add_args(leibniz, quote(argtypes(rounds |> type(int)))))

leibniz_quickr <- quickr::quick(function(rounds) {
  declare(type(rounds = integer(1)))
  x <- 1.0
  pi <- 1.0
  for (i in 2:(rounds + 2)) {
    x <- x * -1
    pi <- pi + (x / (2 * i - 1))
  }
  pi <- pi * 4
  pi
})

rounds <- 10000000L
cat("R        =", leibniz(rounds), "\n")
cat("ast2ast  =", leibniz_ast2ast(rounds), "\n")
cat("quickr   =", leibniz_quickr(rounds), "\n")
print(microbenchmark(
  R       = leibniz(rounds),
  ast2ast = leibniz_ast2ast(rounds),
  quickr  = leibniz_quickr(rounds),
  times = 10
))

# ---------------------------------------------------------------------------
# 2. branchy scalar loop: count primes below n (while, %%, if)
# ---------------------------------------------------------------------------
count_primes <- function(n) {
  count <- 0L
  for (i in 2L:n) {
    is_prime <- TRUE
    j <- 2L
    while (j * j <= i) {
      if (i %% j == 0L) {
        is_prime <- FALSE
      }
      j <- j + 1L
    }
    if (is_prime) {
      count <- count + 1L
    }
  }
  return(count)
}

count_primes_ast2ast <- ast2ast::translate(add_args(count_primes, quote(argtypes(n |> type(int)))))

count_primes_quickr <- quickr::quick(function(n) {
  declare(type(n = integer(1)))
  count <- 0L
  for (i in 2L:n) {
    is_prime <- TRUE
    j <- 2L
    while (j * j <= i) {
      if (i %% j == 0L) {
        is_prime <- FALSE
      }
      j <- j + 1L
    }
    if (is_prime) {
      count <- count + 1L
    }
  }
  count
})

n_primes <- 200000L
cat("R        =", count_primes(n_primes), "\n")
cat("ast2ast  =", count_primes_ast2ast(n_primes), "\n")
cat("quickr   =", count_primes_quickr(n_primes), "\n")
print(microbenchmark(
  # R       = count_primes(n_primes),
  ast2ast = count_primes_ast2ast(n_primes),
  quickr  = count_primes_quickr(n_primes),
  times = 100
))

# ---------------------------------------------------------------------------
# 3. vector reduction: sqrt/sin/abs over a vector
# ---------------------------------------------------------------------------
vecwork <- function(x) {
  n <- length(x)
  s <- 0.0
  for (i in seq_len(n)) {
    s <- s + sqrt(abs(x[i])) * sin(x[i])
  }
  return(s)
}

vecwork_ast2ast <- ast2ast::translate(add_args(vecwork, quote(argtypes(x |> type(borrow_vec(double)) ))))

vecwork_quickr <- quickr::quick(function(x) {
  declare(type(x = double(NA)))
  n <- length(x)
  s <- 0.0
  for (i in 1:n) {
    s <- s + sqrt(abs(x[i])) * sin(x[i])
  }
  s
})
x_vec <- rnorm(5000000)
cat("R        =", vecwork(x_vec), "\n")
cat("ast2ast  =", vecwork_ast2ast(x_vec), "\n")
cat("quickr   =", vecwork_quickr(x_vec), "\n")
print(microbenchmark(
  R       = vecwork(x_vec),
  ast2ast = vecwork_ast2ast(x_vec),
  quickr  = vecwork_quickr(x_vec),
  times = 10
))

# ---------------------------------------------------------------------------
# 4. manual triple-loop matmul (no %*%) -- isolates codegen from BLAS
# ---------------------------------------------------------------------------
matmul_manual <- function(A, B) {
  n <- nrow(A)
  k <- ncol(A)
  m <- ncol(B)
  C <- matrix(0.0, n, m)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      s <- 0.0
      for (l in seq_len(k)) {
        s <- s + A[[i, l]] * B[[l, j]]
      }
      C[[i, j]] <- s
    }
  }
  return(C)
}

matmul_manual_ast2ast <- ast2ast::translate(add_args(matmul_manual,
  quote(argtypes(A |> type(borrow_mat(double)), B |> type(borrow_mat(double))))))

matmul_manual_quickr <- quickr::quick(function(A, B) {
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
bench_matmul_manual <- function(N) {
  A <- matrix(rnorm(N * N), N, N)
  B <- matrix(rnorm(N * N), N, N)
  ref <- matmul_manual(A, B)
  stopifnot(all.equal(ref, matmul_manual_ast2ast(A, B)))
  stopifnot(all.equal(ref, matmul_manual_quickr(A, B)))
  microbenchmark(
    R       = matmul_manual(A, B),
    ast2ast = matmul_manual_ast2ast(A, B),
    quickr  = matmul_manual_quickr(A, B),
    times = 10
  )
}
for (N in c(10, 50, 100)) {
  cat(sprintf("\n--- N = %d ---\n", N))
  print(bench_matmul_manual(N))
}

# ---------------------------------------------------------------------------
# 5. A %*% B combined elementwise with C, then summed (BLAS + allocation)
# ---------------------------------------------------------------------------
linalg <- function(A, B, C) {
  D <- A %*% B
  E <- D + C - D
  sum(E)
}

linalg_ast2ast <- ast2ast::translate(add_args(linalg,
  quote(argtypes(A |> type(borrow_mat(double)), B |> type(borrow_mat(double)), C |> type(borrow_mat(double))))))

linalg_quickr <- quickr::quick(function(A, B, C) {
  declare(
    type(A = double(NA, NA)),
    type(B = double(NA, NA)),
    type(C = double(NA, NA))
  )
  D <- A %*% B
  E <- D + C - D
  s <- sum(E)
  s
})

bench_linalg <- function(N) {
  A <- matrix(rnorm(N * N), N, N)
  B <- matrix(rnorm(N * N), N, N)
  C <- matrix(rnorm(N * N), N, N)
  ref <- linalg(A, B, C)
  stopifnot(all.equal(ref, linalg_ast2ast(A, B, C), tolerance = 1e-8))
  stopifnot(all.equal(ref, linalg_quickr(A, B, C), tolerance = 1e-8))
  microbenchmark(
    R       = linalg(A, B, C),
    ast2ast = linalg_ast2ast(A, B, C),
    quickr  = linalg_quickr(A, B, C),
    times = 20
  )
}
for (N in c(10, 50, 200, 500)) {
  cat(sprintf("\n--- N = %d ---\n", N))
  print(bench_linalg(N))
}
