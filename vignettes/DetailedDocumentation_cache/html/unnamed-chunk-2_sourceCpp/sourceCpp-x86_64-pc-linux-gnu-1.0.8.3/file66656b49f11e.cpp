// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef void (*fp) (sexp&);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
void byref(XPtr<fp> fetr, NumericVector x) { // example 1
  fp Fct = *fetr;

  sexp a;
  a = x;
  
  print("x before call of function:");
  print(a);
  Fct(a);
  print("x after call of function:");
  print(a);  
  
  return Fct(a);
}
