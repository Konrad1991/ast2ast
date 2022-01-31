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


#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// call_package
void call_package(Rcpp::XPtr<fp> inp);
RcppExport SEXP sourceCpp_1_call_package(SEXP inpSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::XPtr<fp> >::type inp(inpSEXP);
    call_package(inp);
    return R_NilValue;
END_RCPP
}
