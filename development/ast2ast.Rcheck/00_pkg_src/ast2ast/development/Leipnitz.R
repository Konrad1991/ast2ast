leibnizR <- function(rounds) {
  x_db <- 1.0
  pi_db <- 1.0
  v <- vector(mode = "numeric", length = rounds + 1)
  start_db <- 2.0
  for (i in 1:length(v)) {
    v[i] <- start_db
    start_db <- start_db + 1
  }
  for (i in v) {
    x_db <- x_db * (-1)
    pi_db <- pi_db + x_db / (2 * i - 1)
  }
  pi_db <- pi_db * 4
  return(pi_db)
}

leibnizCPP1 <- function(rounds_it) {
  x_db <- 1.0
  pi_db <- 1.0
  v <- vector(mode = "numeric", length = rounds_it + 1)
  start_db <- 2.0
  for (i in v) {
    i <- start_db
    start_db <- start_db + 1
  }
  for (i in v) {
    x_db <- x_db * (-1)
    pi_db <- pi_db + x_db / (2 * i - 1)
  }
  pi_db <- pi_db * 4
  return(pi_db)
}


leibnizCPP2 <- function(rounds_db) {
  x_db <- 1.0
  pi_db <- 1.0
  for (i in 2:(rounds_db + 2)) {
    x_db <- x_db * (-1)
    pi_db <- pi_db + x_db / (2 * i - 1)
  }
  pi_db <- pi_db * 4
  return(pi_db)
}

leibnizCPP3 <- function(rounds_it) {
  x_db <- 1
  pi_db <- 1
  i_it <- 2
  while (i_it <= (rounds_it + 2)) {
    x_db <- x_db * -1
    pi_db <- pi_db + (x_db / (2 * i_it - 1.0))
    i_it <- i_it + 1
  }

  pi_db <- pi_db * 4.0
  pi <- pi_db
  return(pi)
}

leibnizVectorized <- function(rounds) {
  pi <- sum(4 / seq.int(-2 * rounds + 1, 2 * rounds, by = 4))
  return(pi)
}

leibniz_cpp1 <- ast2ast::translate(leibnizCPP1,
  verbose = FALSE,
  reference = TRUE
)
leibniz_cpp2 <- ast2ast::translate(leibnizCPP2, verbose = FALSE, reference = TRUE)
leibniz_cpp3 <- ast2ast::translate(leibnizCPP3, verbose = FALSE, reference = TRUE)

Rcpp::sourceCpp(
  code =
    '
  // [[Rcpp::depends(ast2ast)]]
  // [[Rcpp::depends(RcppArmadillo)]]
  // [[Rcpp::plugins(cpp2a)]]
  #include "etr.hpp"

  // [[Rcpp::export]]
  double leibniz_cpp4(int rounds) {
      using namespace etr;
      double x = 1.0;
      double pi = 1.0;
      Vec<double> v = vector_numeric(rounds + 1);
      double start = 2.0;
      for (auto &i : v) {
        i = start;
        start = start + 1.0;
      }
      for (auto &i : v) {
        x = x * -1;
        pi = pi + x / (etr::i2d(2) * i - 1);
      }
      pi = pi * 4;
      return pi;
  }
  '
)
p <- function(text, timing) {
  cat(text)
  cat("\n")
  print(timing)
  cat("\n\n")
}

rounds <- 100000000
p("R", system.time(leibnizR(rounds)))
p("R CPP2", system.time(leibnizCPP2(rounds)))
p("vectorized", system.time(leibnizVectorized(rounds)))
p("cpp1", system.time(leibniz_cpp1(rounds)))
p("cpp2", system.time(leibniz_cpp2(rounds)))
p("cpp3", system.time(leibniz_cpp3(rounds)))
p("cpp4", system.time(leibniz_cpp4(rounds)))

leibnizR(rounds)
leibnizCPP2(rounds)
leibnizVectorized(rounds)
leibniz_cpp1(rounds)
leibniz_cpp2(rounds)
leibniz_cpp3(rounds)
leibniz_cpp4(rounds)

microbenchmark::microbenchmark(
  leibnizVectorized(rounds),
  leibniz_cpp1(rounds),
  leibniz_cpp2(rounds),
  leibniz_cpp3(rounds),
  leibniz_cpp4(rounds),
  times = 50
)
