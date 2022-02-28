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


#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// byref
void byref(XPtr<fp> fetr, NumericVector x);
RcppExport SEXP sourceCpp_1_byref(SEXP fetrSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<fp> >::type fetr(fetrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    byref(fetr, x);
    return R_NilValue;
END_RCPP
}
