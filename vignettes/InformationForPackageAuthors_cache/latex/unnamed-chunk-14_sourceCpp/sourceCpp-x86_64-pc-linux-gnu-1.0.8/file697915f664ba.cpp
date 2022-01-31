// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(rmumps)]]
// [[Rcpp::depends(r2sundials)]]
// [[Rcpp::depends(ast2ast)]]
// [[Rcpp::plugins("cpp17")]]
#include "etr.hpp"
#include "RcppArmadillo.h"
#include "r2sundials.h"
using namespace arma;
using namespace Rcpp;

typedef int (*fp)(double t, const vec &y,
             vec &ydot, RObject &param,
             NumericVector &psens);

typedef void (*user_fct)(sexp& y_,
              sexp& ydot_);
user_fct Fct;

int rhs_exp_wrapper(double t, const vec &y,
                    vec &ydot, RObject &param,
                    NumericVector &psens) {
  NumericVector p(param);
  const int size = y.size();
  sexp ydot_(size, ydot.memptr(), 2);
  
  double* ptr = const_cast<double*>(
                y.memptr());
  sexp y_(size, ptr, 2);
  Fct(y_, ydot_);
  return(CV_SUCCESS);
}


// [[Rcpp::export]]
NumericVector solve_ode(XPtr<user_fct> inp,
                        NumericVector time,
                        NumericVector y) {
Fct = *inp;
XPtr<fp> ptr = XPtr<fp>(new fp(
            &rhs_exp_wrapper));

Environment pkg = 
  Environment::namespace_env("r2sundials");
Function solve = pkg["r2cvodes"];
NumericVector output = solve(y, time,
                             ptr, time);

return output;
}
