// [[Rcpp::depends(ast2ast, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp17)]]
using namespace arma;
#include <etr.hpp>

// [[Rcpp::export]]
void fct() {
  // vec to sexp
  arma::vec a(4, fill::value(30.0));
  sexp a_; // sexp a_ = a; Error!
  a_ = a;
  print(a_);
  
  // sexp to vec
  sexp b_ = coca(3, 4);
  vec b = b_;
  b.print();
  
  // mat to sexp
  mat c(3, 3, fill::value(31.0));
  sexp c_; // sexp c_ = c; Error!
  c_ = c;
  print(c_);
  
  // sexp to mat
  sexp d_ = matrix(colon(1, 16), 4, 4);
  mat d = d_;
  d.print();
}
