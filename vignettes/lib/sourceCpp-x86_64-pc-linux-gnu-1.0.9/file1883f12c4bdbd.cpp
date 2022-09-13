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

int rhs_exp(double t, const vec& y, vec& ydot, RObject& param, NumericVector& psens) {
  NumericVector p(param);
  ydot[0] = -p["a"]*(y[0]-1);
  return(CV_SUCCESS);
}

SEXP getXPtr() {
  typedef int (*funcPtr)( double t, const vec& y, vec& ydot, RObject& param, NumericVector& psens );
  return XPtr<funcPtr>(new funcPtr(& rhs_exp ));
}
