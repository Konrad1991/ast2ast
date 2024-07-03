#include "etr.hpp"
using namespace etr;

Vec<double> test_borrow_internal(Vec<double> &a, Vec<double> &b,
                                 Vec<double, Borrow<double>> &c) {
  return (a + b + c);
}

// [[Rcpp::export]]
void test_borrow_r(Rcpp::NumericVector nv) {
  Vec<double> a = coca(1, 2, 3);
  Vec<double> b = coca(4, 5, 6);
  Vec<double, Borrow<double>> c(nv.begin(), nv.size());
  Vec<double> res;
  res = test_borrow_internal(a, b, c);
  ass<"length should be 3">(length(res) == 3);
  ass<"12">(res[0] == 12);
  ass<"15">(res[1] == 15);
  ass<"18">(res[2] == 18);
}

Vec<double> test_borrow_sexp_internal(Vec<double> &a, Vec<double> &b,
                                      Vec<double, BorrowSEXP<double>>& c) {
  return (a + b + c);
}

// [[Rcpp::export]]
void test_borrow_sexp_r(SEXP nv) {
  Vec<double> a = coca(1, 2, 3);
  Vec<double> b = coca(4, 5, 6);
  Vec<double, BorrowSEXP<double>> c(nv);
  Vec<double> res;
  res = test_borrow_sexp_internal(a, b, c);
  ass<"length should be 3">(length(res) == 3);
  ass<"12">(res[0] == 12);
  ass<"15">(res[1] == 15);
  ass<"18">(res[2] == 18);
}
