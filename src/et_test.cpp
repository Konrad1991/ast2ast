#include "ast2ast.hpp"


// [[Rcpp::export]]
void testfct() {

  vec a(10, 5.);
  vec b;
  b = a;
  b = 1.;
  b = subset(a, 1, 3);
  //print(b);

  subset_self(a, 1, 3) = subset(a, 1, 3) + 3.14;
  print(a);
  subset_self(a, 1, 8) = range(1, 8);
  print(a);
  subset_self(a, 1) = 1.3;
  print(a);
}




// [[Rcpp::export]]
void testfct2() {

  vec a(10, 10.);
  vec b(5, 2.5);

  vec c;

  c = subset(a, 1, 5)/2.0 + b;
  print(c);

}
