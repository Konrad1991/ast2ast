library(quickr)

convolve <- quick(function(a, b) {
  declare(type(a = double(NA)),
          type(b = double(NA)))
  ab <- double(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i+j-1] <- ab[i+j-1] + a[i] * b[j]
    }
  }
  ab
})

slow_convolve <- function(a, b) {
  declare(type(a = double(NA)),
          type(b = double(NA)))
  ab <- double(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i+j-1] <- ab[i+j-1] + a[i] * b[j]
    }
  }
  ab
}

library(quickr)
quick_convolve <- quick(slow_convolve)

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

ast2ast_convolve <- function(a, b) {
  argtypes(
    a |> type(borrow_vec(double)),
    b |> type(borrow_vec(double))
  )
  ab <- numeric(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i+j-1L] <- ab[i+j-1L] + a[i] * b[j]
    }
  }
  ab
}
fcpp <- ast2ast::translate(ast2ast_convolve, debug = FALSE, verbose = TRUE)

a <- runif(100000);
b <- runif(100)

identical(fcpp(a, b), slow_convolve(a, b))

microbenchmark::microbenchmark(
  ast2ast = fcpp(a, b),
  r = slow_convolve(a, b),
  quickr = quick_convolve(a, b),
  c = convolve_c(a, b),
  times = 10L
)
