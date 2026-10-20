library(ast2ast)

argtypes <- function(...) {}
returntype <- function(ReturnValue) {}

spectral_norm <- function(n) {
  argtypes(
    n |> type(int)
  )
  u <- rep(1.0, n)
  v <- numeric(n)
  tmp <- numeric(n)

  for (iter in 1L:10L) {
    # v <- t(A) %*% (A %*% u)
    for (i in 1L:n) {
      acc <- 0.0
      for (j in 1L:n) {
        s <- i + j - 2L
        acc <- acc + u[[j]] / ((s * (s + 1L)) %/% 2L + i)
      }
      tmp[[i]] <- acc
    }
    for (i in 1L:n) {
      acc <- 0.0
      for (j in 1L:n) {
        s <- i + j - 2L
        acc <- acc + tmp[[j]] / ((s * (s + 1L)) %/% 2L + j)
      }
      v[[i]] <- acc
    }

    # u <- t(A) %*% (A %*% v)
    for (i in 1L:n) {
      acc <- 0.0
      for (j in 1L:n) {
        s <- i + j - 2L
        acc <- acc + v[[j]] / ((s * (s + 1L)) %/% 2L + i)
      }
      tmp[[i]] <- acc
    }
    for (i in 1L:n) {
      acc <- 0.0
      for (j in 1L:n) {
        s <- i + j - 2L
        acc <- acc + tmp[[j]] / ((s * (s + 1L)) %/% 2L + j)
      }
      u[[i]] <- acc
    }
  }

  vBv <- 0.0
  vv <- 0.0
  for (i in 1L:n) {
    vBv <- vBv + u[[i]] * v[[i]]
    vv <- vv + v[[i]] * v[[i]]
  }

  return(sqrt(vBv / vv))
}

fcpp <- translate(spectral_norm, debug = FALSE)

Rcpp::cppFunction("
double spectral_norm_rcpp(int n) {
  NumericVector u(n, 1.0);
  NumericVector v(n);
  NumericVector tmp(n);
  for (int iter = 1; iter <= 10; iter++) {
    for (int i = 1; i <= n; i++) {
      double acc = 0.0;
      for (int j = 1; j <= n; j++) {
        int s = i + j - 2;
        acc += u[j - 1] / ((s * (s + 1)) / 2 + i);
      }
      tmp[i - 1] = acc;
    }
    for (int i = 1; i <= n; i++) {
      double acc = 0.0;
      for (int j = 1; j <= n; j++) {
        int s = i + j - 2;
        acc += tmp[j - 1] / ((s * (s + 1)) / 2 + j);
      }
      v[i - 1] = acc;
    }
    for (int i = 1; i <= n; i++) {
      double acc = 0.0;
      for (int j = 1; j <= n; j++) {
        int s = i + j - 2;
        acc += v[j - 1] / ((s * (s + 1)) / 2 + i);
      }
      tmp[i - 1] = acc;
    }
    for (int i = 1; i <= n; i++) {
      double acc = 0.0;
      for (int j = 1; j <= n; j++) {
        int s = i + j - 2;
        acc += tmp[j - 1] / ((s * (s + 1)) / 2 + j);
      }
      u[i - 1] = acc;
    }
  }
  double vBv = 0.0, vv = 0.0;
  for (int i = 0; i < n; i++) {
    vBv += u[i] * v[i];
    vv += v[i] * v[i];
  }
  return std::sqrt(vBv / vv);
}
")

n <- 100L
identical(fcpp(n), spectral_norm(n))
identical(fcpp(n), spectral_norm_rcpp(n))
res <- microbenchmark::microbenchmark(
  ast2ast = fcpp(n), R = spectral_norm(n), rcpp = spectral_norm_rcpp(n)
)
res
fold <- function(res) {
  temp <- aggregate(time ~ expr, res, mean)
  temp$time[[2]] / temp$time[[1]]
}
fold(res)
