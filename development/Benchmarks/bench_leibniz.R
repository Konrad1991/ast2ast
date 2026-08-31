# Revisits https://github.com/Konrad1991/ast2ast/issues/3 (2022): the naive
# translated leibniz() was *slower* than plain R (2.32s vs 1.17s) back then.
# Rerun the exact same naive code with today's ast2ast to see how much the
# type system / inference has improved since.

library(ast2ast)
library(microbenchmark)

leibniz <- function(rounds) {
  argtypes(rounds |> type(int))
  x <- 1.0
  pi <- 1.0
  for (i in 2:(rounds + 2)) {
    x <- x * -1
    pi <- pi + (x / (2 * i - 1))
  }
  pi <- pi * 4
  return(pi)
}

leibniz_shifted <- function(rounds) {
  argtypes(rounds |> type(int))
  x <- 1.0
  pi <- 1.0
  for (i in 1:(rounds + 2)) {
    if (i == 1) next
    x <- x * -1
    pi <- pi + (x / (2 * i - 1))
  }
  pi <- pi * 4
  return(pi)
}

leibniz_seqlen <- function(rounds) {
  argtypes(rounds |> type(int))
  x <- 1.0
  pi <- 1.0
  for (i in seq_len(rounds + 2)) {
    if (i == 1) next
    x <- x * -1
    pi <- pi + (x / (2 * i - 1))
  }
  pi <- pi * 4
  return(pi)
}

leibniz_vectorized <- function(rounds) {
  pi <- sum(4 / seq.int(-2 * rounds + 1, 2 * rounds, by = 4))
  return(pi)
}

# ast2ast has no seq.int(by=); the same arithmetic sequence is a step-1
# integer range (:), 0-indexed to match seq.int's start, then scaled/shifted
# -- same denominators as seq.int(-2*rounds+1, 2*rounds, by=4).
leibniz_vectorized_ast2ast <- function(rounds) {
  argtypes(rounds |> type(int))
  idx <- 0L:(rounds - 1L)
  denom <- -2.0 * rounds + 1.0 + 4.0 * idx
  pi <- sum(4.0 / denom)
  return(pi)
}

cat("Translating...\n")
leibniz_cpp <- ast2ast::translate(leibniz)
leibniz_shifted_cpp <- ast2ast::translate(leibniz_shifted)
leibniz_seqlen_cpp <- ast2ast::translate(leibniz_seqlen)
leibniz_vec_cpp <- ast2ast::translate(leibniz_vectorized_ast2ast)

# plain-R reference for the naive version (drop the argtypes() line)
leibniz_r <- leibniz; body(leibniz_r) <- as.call(as.list(body(leibniz))[-2])

rounds <- 10000000L

cat("\n--- correctness ---\n")
cat("leibniz_cpp(rounds)         =", leibniz_cpp(rounds), "\n")
cat("leibniz_shifted_cpp(rounds) =", leibniz_shifted_cpp(rounds), "\n")
cat("leibniz_seqlen_cpp(rounds)  =", leibniz_seqlen_cpp(rounds), "\n")
cat("leibniz_vec_cpp(rounds)     =", leibniz_vec_cpp(rounds), "\n")
cat("leibniz_r(rounds)             =", leibniz_r(rounds), "\n")

cat("\n--- system.time, single run each (matches the original issue) ---\n")
cat("R (naive):           "); print(system.time(leibniz_r(rounds)))
cat("R (vectorized):      "); print(system.time(leibniz_vectorized(rounds)))
cat("ast2ast (naive):     "); print(system.time(leibniz_cpp(rounds)))
cat("ast2ast (shifted):   "); print(system.time(leibniz_shifted_cpp(rounds)))
cat("ast2ast (seq_len):   "); print(system.time(leibniz_seqlen_cpp(rounds)))
cat("ast2ast (vectorized):"); print(system.time(leibniz_vec_cpp(rounds)))

cat("\n--- microbenchmark, 5 reps each ---\n")
print(microbenchmark(
  R_naive            = leibniz_r(rounds),
  R_vectorized       = leibniz_vectorized(rounds),
  ast2ast_naive      = leibniz_cpp(rounds),
  ast2ast_shifted    = leibniz_shifted_cpp(rounds),
  ast2ast_seqlen     = leibniz_seqlen_cpp(rounds),
  ast2ast_vectorized = leibniz_vec_cpp(rounds),
  times = 5
))
