#include "./inst/include/etr.hpp"

// [[Rcpp::plugins("cpp20")]]
using namespace etr;
// [[Rcpp::export]]
SEXP f(SEXP aSEXP) {
  sexp a;
  a = aSEXP;
  sexp b;
  sexp c;
  a = etr::matrix(etr::colon(etr::i2d(1), 9), etr::i2d(3), 3);
  b = etr::coca(etr::i2d(0), 1);
  c = etr::coca(etr::i2d(1), 1);
  etr::print("=================");
  b = etr::subset(a, etr::coca(etr::i2d(1), 2), b == c); // TODO: ind is wrong!
  return (etr::cpp2R(b));
}

/*** R
f(1)
*/
