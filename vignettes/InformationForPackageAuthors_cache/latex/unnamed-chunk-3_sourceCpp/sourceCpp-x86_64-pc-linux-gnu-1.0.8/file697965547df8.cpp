// [[Rcpp::depends(RcppArmadillo, ast2ast)]]
#include "etr.hpp"
// [[Rcpp::plugins("cpp17")]]


typedef sexp (*fp)(sexp a);


// [[Rcpp::export]]
void call_package(Rcpp::XPtr<fp> inp) {
  fp f = *inp;
  sexp a = coca(1, 2, 3);
  print(a);
  a = f(a);
  print("a is now:");
  print(a);
}
