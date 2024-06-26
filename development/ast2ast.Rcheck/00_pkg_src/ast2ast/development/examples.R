f <- function() {
  print("Hello World!")
}
fcpp <- ast2ast::translate(f)
fcpp()

# Translating to external pointer
# --------------------------------------------------------------------------
f <- function() {
  print("Hello World!")
}
pointer_to_f_cpp <- ast2ast::translate(f,
  output = "XPtr", verbose = TRUE
)

Rcpp::sourceCpp(code = "
#include <Rcpp.h>
typedef void (*fp)();

// [[Rcpp::export]]
void call_fct(Rcpp::XPtr<fp> inp) {
  fp f = *inp;
  f(); } ")

call_fct(pointer_to_f_cpp)



# Run sum example:
# ==========================================================================

# R version of run sum
# --------------------------------------------------------------------------
run_sum <- function(x, n) {
  sz <- length(x)

  ov <- vector(mode = "numeric", length = sz)

  ov[n] <- sum(x[1:n])
  for (i in (n + 1):sz) {
    ov[i] <- ov[i - 1] + x[i] - x[i - n]
  }

  ov[1:(n - 1)] <- NA

  return(ov)
}

# translated Version of R function
# --------------------------------------------------------------------------
run_sum_fast <- function(x, n) {
  sz <- length(x)
  ov <- vector(mode = "numeric", length = sz)

  sum_db <- 0
  for (i in 1:n) {
    sum_db <- sum_db + at(x, i)
  }
  ov[n] <- sum_db

  for (i in (n + 1):sz) {
    ov[i] <- at(ov, i - 1) + at(x, i) - at(x, i - at(n, 1))
  }

  ov[1:(n - 1)] <- NA

  return(ov)
}
run_sum_cpp <- ast2ast::translate(run_sum_fast, verbose = FALSE)
set.seed(42)
x <- rnorm(10000)
n <- 500
one <- run_sum(x, n)
two <- run_sum_cpp(x, n)


microbenchmark::microbenchmark(run_sum(x, n), run_sum_cpp(x, n))
