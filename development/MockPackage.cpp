// [[Rcpp::depends(RcppArmadillo, ast2ast)]]
// [[Rcpp::plugins("cpp2a")]]
#include "etr.hpp"

typedef etr::Vec<double> (*FP)(etr::Vec<double> a);

// [[Rcpp::export]]
void call_package(Rcpp::XPtr<FP> inp) {
  FP f = *inp;
  sexp a = etr::coca(1, 2, 3);
  etr::print(a);
  a = f(a);
  etr::print("a is now:");
  etr::print(a);
}
