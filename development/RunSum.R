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
  ov <- vector(mode = "numeric", length = length(x))
  sum_db <- 0
  for (i in 1:n) {
    sum_db <- sum_db + at(x, i)
  }
  ov[n] <- sum_db

  for (i in (n + 1):length(x)) {
    ov[i] <- at(ov, i - 1) + at(x, i) - at(x, i - at(n, 1))
  }

  ov[1:(n - 1)] <- NA

  return(ov)
}
run_sum_cpp <- ast2ast::translate(run_sum_fast, verbose = TRUE)
set.seed(42)
x <- rnorm(10000)
n <- 500
one <- run_sum(x, n)
two <- run_sum_cpp(x, n)
plot(one)
plot(two, pch = 19, col = "darkred")
identical(one, two)
microbenchmark::microbenchmark(run_sum(x, n), run_sum_cpp(x, n))
tail(one, n = 50)
tail(two, n = 50)
