f <- function(a, b) {
  a <- c(1, 2, 3)
  c <- a + b * 2
  d <- vector(1)
  print(d)
  m <- matrix(1, 2, 2)
  print(m)
  return(c)
}

fcpp <- ast2ast::translate(f, verbose = TRUE)
.traceback() |> print()
a <- c(1, 2, 3)
b <- 3.14
fcpp(a, b)

stop("bla")

Rcpp::sourceCpp(code = '
// [[Rcpp::depends(ast2ast)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp2a)]]
#include "etr.hpp"

// [[Rcpp::export]]
SEXP f(SEXP aSEXP) {
  using namespace etr;
  Vec<double, BorrowSEXP<double>> a;

  auto test = Rf_isReal(aSEXP);
  Rcpp::Rcout << test << std::endl;

  bool b = test;
  Rcpp::Rcout << b << std::endl;

  a = aSEXP;
  print(a);
  return(R_NilValue);
}')

f(1.1)
