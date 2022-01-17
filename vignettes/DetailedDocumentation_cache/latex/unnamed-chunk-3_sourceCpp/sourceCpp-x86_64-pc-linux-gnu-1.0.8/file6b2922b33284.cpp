// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef void (*fp) ();

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
void call_fct(XPtr<fp> fetr) { // example 3
  fp Fct = *fetr;
  Fct();
}
