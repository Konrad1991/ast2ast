
// [[Rcpp::depends(ast2ast)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp2a)]]
#include "etr.hpp"

typedef void (*fct_ptr)(double &c_db, etr::Vec<double> &r,
                        double *t_dvp_double_ptr, int t_dvp_int_size,
                        etr ::Vec<double> &z);

// [[Rcpp::export]]
void test(Rcpp::XPtr<fct_ptr> xptr) {
  fct_ptr f = *xptr;
  using namespace etr;
  double c = 3.14;
  Vec<double> r = coca(1, 2, 3);
  int size = 10000000;
  double *t = new double[size];
  Vec<double> z = coca(1, 2, 3);
  f(c, r, t, size, z);
  delete[] t;
}
