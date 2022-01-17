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


#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// call_fct
void call_fct(XPtr<fp> fetr);
RcppExport SEXP sourceCpp_9_call_fct(SEXP fetrSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<fp> >::type fetr(fetrSEXP);
    call_fct(fetr);
    return R_NilValue;
END_RCPP
}
