// [[Rcpp::depends(ast2ast)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp2a)]]
#include "etr.hpp"
using namespace etr;

typedef int (*fct_ptr)(Vec<double> &, Vec<int> &);

// [[Rcpp::export]]
int call_fcpp(Rcpp::XPtr<fct_ptr> xptr) {
  fct_ptr f = *xptr;
  Vec<double> a = coca(1, 2, 3);
  Vec<int> b = coca(1, 2, 3);
  int res = f(a, b);
  etr::print(b);
  return res;
}
