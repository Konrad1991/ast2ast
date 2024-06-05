
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
  
  return(R_NilValue); 
}

/*** R
f(1.1)
*/
