Sys.setenv(PKG_CXXFLAGS = StanHeaders:::CxxFlags(as_character = TRUE))
SH <- system.file(ifelse(.Platform$OS.type == "windows", "libs", "lib"), .Platform$r_arch,
                  package = "StanHeaders", mustWork = TRUE)
Sys.setenv(PKG_LIBS = paste0(StanHeaders:::LdFlags(as_character = TRUE),
                             " -L", shQuote(SH), " -lStanHeaders"))
Rcpp::sourceCpp("development/stan_ad.cpp")

# Plain function
# ------------------------------------------------------------------------------
f_plain <- function(A, B) {
  C <- c(A %*% B)
  s <- 0.0
  for (i in seq_len(length(C))) {
    s <- s + C[[i]]
  }
  return(s)
}
fp_args <- function(A, B) {
  A |> type(mat(double)) |> ref()
  B |> type(mat(double)) |> ref()
}
fp_cpp <- ast2ast::translate(f_plain, fp_args, verbose = TRUE)

# Reverse AD
# ------------------------------------------------------------------------------
f_rev <- function(A, B) {
  C <- c(A %*% B)
  s <- 0.0
  for (i in seq_len(length(C))) {
    s <- s + C[[i]]
  }
  res <- matrix(deriv(s, A), nrow(A), ncol(B))
  return(res)
}
fr_args <- function(A, B) {
  A |> type(mat(double))
  B |> type(mat(double))
}
fr_cpp <- ast2ast::translate(f_rev, fr_args, derivative = "reverse", verbose = TRUE)

f_rev <- function(A, B) {
  f <- fn(
    f_args = function(A_inner, B_inner) {
      A_inner |> type(matrix(double))
      B_inner |> type(mat(double)) |> ref()
    },
    return_value = type(mat(double)),
    block = function(A_inner, B_inner) {
      C <- c(A_inner %*% B_inner)
      s <- 0.0
      for (i in seq_len(length(C))) {
        s <- s + C[[i]]
      }
      res <- matrix(deriv(s, A_inner), nrow(A_inner), ncol(B_inner))
      return(res)
    }
  )
  res <- f(A, B)
  return(res)
}
fr_args <- function(A, B) {
  A |> type(mat(double))
  B |> type(mat(double))
}
fr_cpp2 <- ast2ast::translate(f_rev, fr_args, derivative = "reverse", verbose = TRUE)

# Forward AD
# ------------------------------------------------------------------------------
f_for <- function(A, B) {
  res <- numeric(nrow(A) * ncol(B))
  for (i in seq_len(length(A))) {
    seed(A, i)
    C <- c(A %*% B)
    s <- 0.0
    for (i in seq_len(length(C))) {
      s <- s + C[[i]]
    }
    res[[i]] <- get_dot(s)
    unseed(A, i)
  }
  return(matrix(res, nrow(A), ncol(B)))
}
ff_cpp <- ast2ast::translate(f_for, fr_args, derivative = "forward", verbose = TRUE)

# Benchmark
# ------------------------------------------------------------------------------
check <- function(r_a, c_a, r_b, c_b) {
  A <- matrix(rnorm(r_a * c_a), r_a, c_a)
  B <- matrix(rnorm(r_b * c_b), r_b, c_b)
  TOL <- 1e-6
  cat("Results identical: ",
    any(abs(fr_cpp(A, B) - f_rev_stan(A, B)) < TOL) &&
    any(abs(fr_cpp(A, B) - ff_cpp(A, B)) < TOL) &&
    any(abs(fr_cpp(A, B) - fr_cpp2(A, B)) < TOL),
    "\n"
  )
  microbenchmark::microbenchmark(
    f_plain(A, B), fp_cpp(A, B),
    fr_cpp(A, B), fr_cpp2(A, B),
    ff_cpp(A, B), f_rev_stan(A, B),
    times = 100L
  )
}
n <- 2
check(n, n, n, n)
n <- 5
check(n, n, n, n)
n <- 25
check(n, n, n, n)
n <- 50
check(n, n, n, n)
