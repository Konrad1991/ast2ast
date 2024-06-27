#define DERIV_ETR

// [[Rcpp::depends(ast2ast)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp2a)]]
#include "etr.hpp"

// [[Rcpp::export]]
SEXP f(SEXP xSEXP) {
  etr::Vec<double> y;
  etr::Vec<double> dydx;
  etr::Vec<double> x;
  x = xSEXP;
  etr::set_indep(x);

  etr::assign_deriv(y, etr::get_deriv(x) * x + x * etr::get_deriv(x));
  y = x * x;
  dydx = etr::get_deriv(y);
  return (etr::cpp2R(dydx));
}
