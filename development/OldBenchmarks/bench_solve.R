# Benchmark: solve(A, b) in ast2ast vs base R (value) and vs Stan (reverse AD).
Sys.setenv(PKG_CXXFLAGS = StanHeaders:::CxxFlags(as_character = TRUE))
SH <- system.file(ifelse(.Platform$OS.type == "windows", "libs", "lib"), .Platform$r_arch,
                  package = "StanHeaders", mustWork = TRUE)
Sys.setenv(PKG_LIBS = paste0(StanHeaders:::LdFlags(as_character = TRUE),
                             " -L", shQuote(SH), " -lStanHeaders"))
Rcpp::sourceCpp("/home/konrad/Documents/ast2ast/development/Benchmarks/stan_solve.cpp")

library(ast2ast)
library(microbenchmark)
set.seed(1234)

# A symmetric positive-definite, well-conditioned matrix (always invertible).
make_A <- function(n) crossprod(matrix(rnorm(n * n), n, n)) + diag(n)

# --- ast2ast: plain value  x = solve(A, b) ----------------------------------
f_plain <- function(A, b) {
  res <- solve(A, b)
  return(res)
}
fp_args <- function(A, b) {
  A |> type(mat(double)) |> ref()
  b |> type(vec(double)) |> ref()
}
fp_cpp <- ast2ast::translate(f_plain, fp_args, verbose = FALSE)

# --- ast2ast: reverse AD  d/dA of sum(solve(A, b)) --------------------------
f_rev <- function(A, b) {
  x <- solve(A, b)
  s <- 0.0
  for (i in seq_len(length(x))) {
    s <- s + x[[i]]
  }
  res <- matrix(deriv(s, A), nrow(A), ncol(A))
  return(res)
}
fr_args <- function(A, b) {
  A |> type(mat(double))
  b |> type(vec(double))
}
fr_cpp <- ast2ast::translate(f_rev, fr_args, derivative = "reverse", verbose = FALSE)

# Analytic gradient of s = 1^T A^-1 b:  dA = -(A^-T 1) (A^-1 b)^T.
grad_analytic <- function(A, b) {
  ones <- rep(1, nrow(A))
  -(solve(t(A), ones)) %*% t(solve(A, b))
}

check <- function(n, times = 50L) {
  cat(sprintf("\n===================== n = %d =====================\n", n))
  A <- make_A(n)
  b <- rnorm(n)
  TOL <- 1e-6

  # ---- value: ast2ast vs base R ----
  ref_val <- solve(A, b)
  dv <- max(abs(ref_val - as.numeric(fp_cpp(A, b))))
  cat(sprintf("value  ast2ast vs base R : match = %s (max|diff| = %.2e)\n", dv < TOL, dv))
  print(microbenchmark::microbenchmark(
    baseR   = solve(A, b),
    ast2ast = fp_cpp(A, b),
    times = times
  ))

  # ---- reverse-mode gradient: ast2ast vs Stan vs analytic ----
  g_ours <- fr_cpp(A, b)
  g_stan <- f_rev_stan_solve(A, b)
  g_ana  <- grad_analytic(A, b)
  cat(sprintf("grad   ast2ast vs Stan    : match = %s (max|diff| = %.2e)\n",
              max(abs(g_ours - g_stan)) < TOL, max(abs(g_ours - g_stan))))
  cat(sprintf("grad   ast2ast vs analytic: match = %s (max|diff| = %.2e)\n",
              max(abs(g_ours - g_ana)) < TOL, max(abs(g_ours - g_ana))))
  print(microbenchmark::microbenchmark(
    ast2ast = fr_cpp(A, b),
    stan    = f_rev_stan_solve(A, b),
    times = times
  ))
  invisible(NULL)
}

for (n in c(10, 50, 100, 250, 500)) check(n)
