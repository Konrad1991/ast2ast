# install.packages(".", types = "source", repo = NULL)
argtypes <- function(...) {}
returntype <- function(ReturnValue) {}
library(ast2ast)

heat <- function(n, steps) {
  argtypes(
    n |> type(int),
    steps |> type(int)
  )
  u <- matrix(0.0, n, n)
  v <- matrix(0.0, n, n)

  # boundary: top row is hot
  j <- 1L
  while (j <= n) {
    u[[1L, j]] <- 1.0
    v[[1L, j]] <- 1.0
    j <- j + 1L
  }

  s <- 1L
  while (s <= steps) {
    # sweep u -> v
    j <- 2L
    while (j < n) {
      i <- 2L
      while (i < n) {
        v[[i, j]] <- 0.25 * (
          u[[i - 1L, j]] + u[[i + 1L, j]] +
            u[[i, j - 1L]] + u[[i, j + 1L]]
        )
        i <- i + 1L
      }
      j <- j + 1L
    }
    # sweep v -> u
    j <- 2L
    while (j < n) {
      i <- 2L
      while (i < n) {
        u[[i, j]] <- 0.25 * (
          v[[i - 1L, j]] + v[[i + 1L, j]] +
            v[[i, j - 1L]] + v[[i, j + 1L]]
        )
        i <- i + 1L
      }
      j <- j + 1L
    }
    s <- s + 1L
  }

  return(u)
}

fcpp <- translate(heat, debug = FALSE)
Rcpp::cppFunction("
NumericMatrix heat_rcpp(int n, int steps) {
  NumericMatrix u(n, n);
  NumericMatrix v(n, n);

  for (int j = 0; j < n; j++) {
    u(0, j) = 1.0;
    v(0, j) = 1.0;
  }

  for (int s = 0; s < steps; s++) {
    for (int j = 1; j < n - 1; j++) {
      for (int i = 1; i < n - 1; i++) {
        v(i, j) = 0.25 * (u(i - 1, j) + u(i + 1, j) +
                          u(i, j - 1) + u(i, j + 1));
      }
    }
    for (int j = 1; j < n - 1; j++) {
      for (int i = 1; i < n - 1; i++) {
        u(i, j) = 0.25 * (v(i - 1, j) + v(i + 1, j) +
                          v(i, j - 1) + v(i, j + 1));
      }
    }
  }
  return u;
}
")

Rcpp::cppFunction(
  includes = "#include <vector>",
  code = "
NumericMatrix heat_checked(int n, int steps) {
  std::vector<double> u(n * n, 0.0);
  std::vector<double> v(n * n, 0.0);

  for (int j = 0; j < n; j++) {
    u.at(0 + j * n) = 1.0;
    v.at(0 + j * n) = 1.0;
  }

  for (int s = 0; s < steps; s++) {
    for (int j = 1; j < n - 1; j++) {
      for (int i = 1; i < n - 1; i++) {
        v.at(i + j * n) = 0.25 * (
          u.at((i - 1) + j * n) + u.at((i + 1) + j * n) +
          u.at(i + (j - 1) * n) + u.at(i + (j + 1) * n)
        );
      }
    }
    for (int j = 1; j < n - 1; j++) {
      for (int i = 1; i < n - 1; i++) {
        u.at(i + j * n) = 0.25 * (
          v.at((i - 1) + j * n) + v.at((i + 1) + j * n) +
          v.at(i + (j - 1) * n) + v.at(i + (j + 1) * n)
        );
      }
    }
  }

  NumericMatrix out(n, n);
  for (int k = 0; k < n * n; k++) {
    out[k] = u.at(k);
  }
  return out;
}
")

n <- 100L
steps <- 50L

res <- fcpp(n, steps)
identical(res, heat(n, steps))
identical(res, heat_rcpp(n, steps))
identical(res, heat_checked(n, steps))

microbenchmark::microbenchmark(
  heat(n, steps),
  fcpp(n, steps),
  heat_checked(n, steps),
  heat_rcpp(n, steps),
  times = 20
)
