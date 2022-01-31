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
            vec& ydot, RObject& param,
            NumericVector& psens) {
            
  double a = 1;
  double nu = 2;
  ydot[0] = -nu*(y[0] - a);
  return(CV_SUCCESS);
}

SEXP getXPtr() {
  typedef int (*funcPtr)( double t, const vec& y,
            vec& ydot, RObject& param,
            NumericVector& psens );
  return XPtr<funcPtr>(new funcPtr(& rhs_exp ));
}
