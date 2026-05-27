f1 <- function(a) {
  size <- length(a)
  for (i in 1L:size) {
    for (j in 1L:(size - 1L)) {
      if (a[j] > a[j + 1L]) {
        temp <- a[j]
        a[j] <- a[j + 1L]
        a[j + 1L] <- temp
      }
    }
  }
  return(a)
}
f2 <- function(a) {
  size <- length(a)
  for (i in 1:size) {
    for (j in 1:(size - 1)) {
      if (a[[j]] > a[[j + 1]]) {
        temp <- a[[j]]
        a[[j]] <- a[[j + 1]]
        a[[j + 1]] <- temp
      }
    }
  }
  return(a)
}
f3 <- function(a) {
  size <- length(a)
  for (i in seq_len(size)) {
    for (j in seq_len(size - 1L)) {
      if (a[[j]] > a[[j + 1]]) {
        temp <- a[[j]]
        a[[j]] <- a[[j + 1]]
        a[[j + 1]] <- temp
      }
    }
  }
  return(a)
}

 fcpp <- ast2ast::translate(f1, verbose = TRUE)
 fcpp2 <- ast2ast::translate(f2, verbose = TRUE)
 fcpp3 <- ast2ast::translate(f3, verbose = TRUE)
 a <- runif(1000)
 microbenchmark::microbenchmark(
   f1(a), f2(a), fcpp(a), fcpp2(a), fcpp3(a)
 )
all(f1(a) == f2(a))
all(f1(a) == fcpp(a))
all(f1(a) == fcpp2(a))
all(f1(a) == fcpp3(a))

convolve <- function(a, b) {
  ab <- numeric(length(a) + length(b) - 1)
  for (i in 1L:length(a)) {
    for (j in 1L:length(b)) {
      ab[i+j-1] <- ab[i+j-1] + a[i] * b[j]
    }
  }
  return(ab)
}
convolve_cpp1 <- ast2ast::translate(convolve, verbose = TRUE)
convolve2 <- function(a, b) {
  ab <- numeric(length(a) + length(b) - 1L)
  i |> type(int) <- 1L
  j |> type(int) <- 1L
  while(i <= length(a)) {
    j <- 1L
    while(j <= length(b)) {
      ab[[i+j-1L]] <- ab[[i+j-1L]] + a[[i]] * b[[j]]
      j <- j + 1L
    }
    i <- i + 1L
  }
  return(ab)
}
convolve_cpp2 <- ast2ast::translate(convolve2, verbose = TRUE)
convolve3 <- function(a, b) {
  ab <- numeric(length(a) + length(b) - 1L)
  for (i in seq_len(length(a))) {
    for (j in seq_len(length(b))) {
      ab[i+j-1L] <- ab[i+j-1L] + a[i] * b[j]
    }
  }
  return(ab)
}
f_args <- function(a, b) {
  a |> type(vec(double)) |> ref()
  b |> type(vec(double)) |> ref()
}
convolve_cpp3 <- ast2ast::translate(convolve3, f_args, verbose = TRUE)

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
})", verbose = TRUE)

a <- runif (100000)
b <- runif (100)
microbenchmark::microbenchmark(
  convolve(a, b),
  convolve_cpp1(a, b), convolve_cpp2(a, b), convolve_cpp3(a, b),
  convolve_c(a, b),
  times = 10
)

all(convolve(a, b) == convolve_cpp1(a, b))
all(convolve(a, b) == convolve_cpp2(a, b))
all(convolve(a, b) == convolve_cpp3(a, b))
all(convolve(a, b) == convolve(a, b))
