// [[Rcpp::depends(RcppArmadillo, r2sundials, rmumps)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <r2sundials.h>
#include <rmumps.h>

using namespace Rcpp;

using namespace arma;

          #include <r2sundials.h>
// [[Rcpp::export]]
SEXP getXPtr();

int rhs_exp(double t, const vec& y,
            vec& ydot,
            RObject& param,
            NumericVector& psens) {
            
  double a = 1;
  double nu = 2;
  ydot[0] = -nu*(y[0] - a);
  return(CV_SUCCESS);
}

SEXP getXPtr() {
  typedef int (*funcPtr)( double t, const vec& y,
            vec& ydot,
            RObject& param,
            NumericVector& psens );
  return XPtr<funcPtr>(new funcPtr(& rhs_exp ));
}


#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// getXPtr
SEXP getXPtr();
RcppExport SEXP sourceCpp_3_getXPtr() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getXPtr());
    return rcpp_result_gen;
END_RCPP
}
