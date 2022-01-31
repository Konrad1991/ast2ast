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
