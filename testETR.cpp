#include "./inst/include/etr.hpp"

// [[Rcpp::plugins("cpp20")]]
//
using namespace etr;
// [[Rcpp::export]]
void f() {
  Vec<double> v;
  v = colon(1, 16);
  print(subset(v, true));
  print(subset(v, 1));
}

/*** R
f()
*/
