// [[Rcpp::depends(ast2ast, RcppArmadillo)]]
// [[Rcpp::plugins(cpp20)]]

#include "etr.hpp"
using namespace Rcpp;
using namespace etr;

static double parms[3];
#define a parms[0]
#define b parms[1]
#define c parms[2]

extern "C" {
void initmod_a2a(void (*odeparms)(int *, double *));
}

extern "C" {
void derivs_a2a(int *neq, double *t, double *y, double *ydot, double *yout,
                int *ip);
}

void initmod_a2a(void (*odeparms)(int *, double *)) {
  int N = 3;
  odeparms(&N, parms);
}

void user_fct(double t, BorrowPtr &y, BorrowPtr &ydot) {
  at(ydot, 1) = a * at(y, 1) + at(y, 2) * at(y, 3);
  at(ydot, 2) = b * (at(y, 2) - at(y, 3));
  at(ydot, 3) = -at(y, 1) * at(y, 2) + c * at(y, 2) - at(y, 3);
}

void derivs_a2a(int *neq, double *t, double *y, double *ydot, double *yout,
                int *ip) {
  BorrowPtr y_(y, *neq);
  BorrowPtr ydot_(ydot, *neq);
  double t_ = *t;
  user_fct(t_, y_, ydot_);
}
