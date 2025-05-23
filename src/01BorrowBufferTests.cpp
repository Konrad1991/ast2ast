#include "etr.hpp"
using namespace etr;

auto test_borrow_internal(Vec<double> &a, Vec<double> &b,
                                 Vec<double, Borrow<double>> &c) {
  return (Cast(a + b + c));
}

// [[Rcpp::export]]
void test_borrow_r(Rcpp::NumericVector nv) {
  Vec<double> a;
  a = c(1, 2, 3);
  Vec<double> b;
  b = c(4, 5, 6);
  Vec<double, Borrow<double>> c(nv.begin(), nv.size());
  Vec<double> res;
  res = test_borrow_internal(a, b, c);
  ass<"length should be 3">(length(res) == 3);
  ass<"12">(res[0] == 12);
  ass<"15">(res[1] == 15);
  ass<"18">(res[2] == 18);
}
