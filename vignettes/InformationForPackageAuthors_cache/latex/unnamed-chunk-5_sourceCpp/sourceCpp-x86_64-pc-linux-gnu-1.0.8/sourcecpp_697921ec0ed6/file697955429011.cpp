// [[Rcpp::depends(ast2ast, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp17)]]
using namespace Rcpp;
#include <etr.hpp>

// [[Rcpp::export]]
void fct() {
  // NumericVector to sexp
  NumericVector a{1, 2};
  sexp a_; // sexp a_ = a; Error!
  a_ = a;
  print(a_);
  
  // sexp to NumericVector
  sexp b_ = coca(3, 4);
  NumericVector b = b_;
  Rcpp::Rcout << b << std::endl;
  
  // NumericMatrix to sexp
  NumericMatrix c(3, 3);
  sexp c_; // sexp c_ = c; Error!
  c_ = c;
  print(c_);
  
  // sexp to NumericMatrix
  sexp d_ = matrix(colon(1, 16), 4, 4);
  NumericMatrix d = d_;
  Rcpp::Rcout << d << std::endl;
}


#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// fct
void fct();
RcppExport SEXP sourceCpp_1_fct() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    fct();
    return R_NilValue;
END_RCPP
}
