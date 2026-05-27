Sys.setenv(PKG_CXXFLAGS = StanHeaders:::CxxFlags(as_character = TRUE))
SH <- system.file(ifelse(.Platform$OS.type == "windows", "libs", "lib"), .Platform$r_arch,
                  package = "StanHeaders", mustWork = TRUE)
Sys.setenv(PKG_LIBS = paste0(StanHeaders:::LdFlags(as_character = TRUE),
                             " -L", shQuote(SH), " -lStanHeaders"))
Rcpp::sourceCpp("/home/konrad/Documents/ast2ast/development/Benchmarks/stan_ad.cpp")

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
fp_cpp <- ast2ast::translate(f_plain, fp_args, verbose = FALSE)

# Reverse AD
# ------------------------------------------------------------------------------
f_rev <- function(A, B) {
  C <- c(A %*% B)
  s <- 0.0
  for (i in seq_len(length(C))) {
    s <- s + C[[i]]
  }
  res <- matrix(deriv(s, A), nrow(A), ncol(A))
  return(res)
}
fr_args <- function(A, B) {
  A |> type(mat(double))
  B |> type(mat(double))
}
fr_cpp <- ast2ast::translate(f_rev, fr_args, derivative = "reverse", verbose = FALSE)

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
      res <- matrix(deriv(s, A_inner), nrow(A_inner), ncol(A_inner))
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
fr_cpp2 <- ast2ast::translate(f_rev, fr_args, derivative = "reverse", verbose = FALSE)

# Forward AD
# ------------------------------------------------------------------------------
f_for <- function(A, B) {
  res <- numeric(nrow(A) * ncol(A))
  for (i in seq_len(length(A))) {
    seed(A, i)
    C <- c(A %*% B)
    s <- 0.0
    for (j in seq_len(length(C))) {
      s <- s + C[[j]]
    }
    res[[i]] <- get_dot(s)
    unseed(A, i)
  }
  return(matrix(res, nrow(A), ncol(A)))
}
ff_cpp <- ast2ast::translate(f_for, fr_args, derivative = "forward", verbose = TRUE)

# Benchmark
# ------------------------------------------------------------------------------
check <- function(r_a, c_a, r_b, c_b) {
  max_finite_diff <- function(x, y) {
    finite <- is.finite(x) & is.finite(y)
    if (!any(finite)) return(0)
    max(abs(x[finite] - y[finite]))
  }
  stopifnot(c_a == r_b)
  A <- matrix(rnorm(r_a * c_a), r_a, c_a)
  B <- matrix(rnorm(r_b * c_b), r_b, c_b)

  # A[[1L]] <- NA
  # A[[2L]] <- NaN
  # A[[3L]] <- -Inf
  # A[[4L]] <- Inf
  # A[[5L]] <- 1e308

  TOL <- 1e-6

  cat("Dim(A): ", dim(A), " Dim(B): ", dim(B), "\n")

  ref <- f_plain(A, B)
  d <- max_finite_diff(ref, fp_cpp(A, B))
  cat("Results of %*% are identical: ", d < TOL, "\n")
  rb <- microbenchmark::microbenchmark(f_plain(A, B), fp_cpp(A, B))
  print(rb)
  cat("=========================================================\n")

  ref <- fr_cpp(A, B)
  d_stan <- max_finite_diff(ref, f_rev_stan(A, B))
  d_stanmul <- max_finite_diff(ref, f_rev_stan_mul(A, B))
  d_fwd <- max_finite_diff(ref, ff_cpp(A, B))
  d_rev2 <- max_finite_diff(ref, fr_cpp2(A, B))
  ok <- max(d_stan, d_stanmul, d_fwd, d_rev2) < TOL
  cat("Results of AD are identical: ", ok, "\n")

  res <- microbenchmark::microbenchmark(
    fr_cpp(A, B),
    f_rev_stan_mul(A, B),
    times = 10L
  )
  print(res)
  cat("=========================================================\n")

  cat("\n\n\n")
  res
}

res <- check(2, 3, 3, 2)
res <- check(5, 5, 5, 5)
res <- check(25, 25, 25, 25)

res <- check(150, 150, 150, 150)
res <- check(180, 180, 180, 180)
