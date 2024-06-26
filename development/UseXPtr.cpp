// [[Rcpp::depends(ast2ast, RcppArmadillo)]]
// [[Rcpp::plugins(cpp2a)]]
#include <etr.hpp>

// [[Rcpp::export]]
SEXP getXPtr();

sexp f(sexp a) {
  sexp b;
  b = a + etr::i2d(2);
  return (b);
}
SEXP getXPtr() {
  typedef sexp (*fct_ptr)(sexp a);
  return Rcpp::XPtr<fct_ptr>(new fct_ptr(&f));
}
