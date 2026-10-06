f <- function() {
  x <- 1:10
  # z <- x + "a"
  x <- c(1, 2, NA)
  return(x)
}
fcpp <- ast2ast::translate(f)
fcpp()
# Hello World
# ----------------------------------------------------------------
f <- function() {
  print("Hello World!")
}
f_cpp <- ast2ast::translate(f)
f_cpp()

# Derivatives
# ----------------------------------------------------------------
f <- function(y, x) {
  y[[1L]] <- x[[1L]] * x[[2L]]
  y[[2L]] <- x[[1L]] + x[[2L]]*x[[2L]]
  jac <- deriv(y, x)
  return(jac)
}
fcpp_reverse <- ast2ast::translate(f, derivative = "reverse", verbose = TRUE)
y <- c(0, 0)
x <- c(2, 3)
fcpp_reverse(y, x)

f <- function(y, x) {
  jac <- matrix(0.0, length(y), length(x))
  for (i in 1L:length(x)) {
    seed(x, i)
    y[[1L]] <- x[[1L]] * x[[2L]]
    y[[2L]] <- x[[1L]] + x[[2L]]*x[[2L]]
    d <- get_dot(y)
    jac[TRUE, i] <- d
    unseed(x, i)
  }
  return(jac)
}
fcpp_forward <- ast2ast::translate(f, derivative = "forward", verbose = TRUE)
fcpp_forward(y, x)

# Type system
# ----------------------------------------------------------------
f <- function(a, b, c) {
  x |> type(vec(int))
  l <- TRUE
  i <- 1L
  d <- c(3.1415)
  c[1] <- 100
}
f_args <- function(a, b, c) {
  a |> type(double)
  b |> type(matrix(double))
  c |> type(vec(double)) |> ref()
}
fcpp <- ast2ast::translate(f, f_args)
a <- 3.14
b <- matrix(1.1, 2, 2)
c <- c(1.1, 2.2, 3.3)
fcpp(a, b, c)
c

# External pointer interface
# ----------------------------------------------------------------
f <- function(a) {
  print("Hello World from C++")
}
ptr <- ast2ast::translate(f, output = "XPtr", verbose = TRUE)

# Call from C++ side
Rcpp::sourceCpp(code = "
  #include <Rcpp.h>
  typedef void (*fp)();
  // [[Rcpp::export]]
  void call_fct(Rcpp::XPtr<fp> inp) {
    fp f = *inp;
    f();
  }")
call_fct(ptr)

# Benchmark
# ----------------------------------------------------------------
a <- runif (100000)
b <- runif (100)
convolve <- function(a, b) {
  ab <- numeric(length(a) + length(b) - 1)
  for (i in 1L:length(a)) {
    for (j in 1L:length(b)) {
      ab[i+j-1] <- ab[i+j-1] + a[i] * b[j]
    }
  }
  return(ab)
}
convolve_cpp <- ast2ast::translate(convolve)
benchmark <- microbenchmark::microbenchmark(
  convolve(a, b), convolve_cpp(a, b),
  times = 100
)
benchmark
identical(convolve(a, b), convolve_cpp(a, b))
res_cpp <- convolve_cpp(a, b)
res_r <- convolve(a, b)
all(res_r == res_cpp)
ggplot2::autoplot(benchmark)

convolve2 <- function(a, b) {
  ab <- numeric(length(a) + length(b) - 1)
  i |> type(int) <- 1L
  j |> type(int) <- 1L
  while(i <= length(a)) {
    j <- 1L
    while(j <= length(b)) {
      ab[[i+j-1]] <- ab[[i+j-1]] + a[[i]] * b[[j]]
      j <- j + 1L
    }
    i <- i + 1L
  }
  return(ab)
}
convolve_cpp2 <- ast2ast::translate(convolve2)

convolve_c <- inline::cfunction(
  sig = c(a = "SEXP", b = "SEXP"), body = r"({
    int na, nb, nab;
    double *xa, *xb, *xab;
    SEXP ab;

    a = PROTECT(Rf_coerceVector(a, REALSXP));
    b = PROTECT(Rf_coerceVector(b, REALSXP));
    na = Rf_length(a); nb = Rf_length(b); nab = na + nb - 1;
    ab = PROTECT(Rf_allocVector(REALSXP, nab));
    xa = REAL(a); xb = REAL(b); xab = REAL(ab);
    for(int i = 0; i < nab; i++) xab[i] = 0.0;
    for(int i = 0; i < na; i++)
        for(int j = 0; j < nb; j++)
            xab[i + j] += xa[i] * xb[j];
    UNPROTECT(3);
    return ab;
})")


res <- microbenchmark::microbenchmark(
  convolve(a, b), convolve_cpp1(a, b), convolve_cpp2(a, b), convolve_c(a, b),
  times = 10
)
res
plot(res)

identical(
  convolve(a, b), c(convolve_cpp1(a, b)), convolve_cpp2(a, b), convolve_c(a, b)
)
