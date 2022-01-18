// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef void (*fp) ();

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
void vardec(XPtr<fp> fetr) { // example 2
  fp Fct = *fetr;
  Fct();
}


#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// vardec
void vardec(XPtr<fp> fetr);
RcppExport SEXP sourceCpp_1_vardec(SEXP fetrSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<fp> >::type fetr(fetrSEXP);
    vardec(fetr);
    return R_NilValue;
END_RCPP
}
