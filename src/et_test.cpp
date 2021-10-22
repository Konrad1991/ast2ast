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

  std::cout << a.size() << " " << a.subsetted << " " << a.d.sz << std::endl;
  subset_self(a, 1, 8) = range(1, 8);
  std::cout << a.size() << " " << a.subsetted << " " << a.d.sz << std::endl;
  print(a);
  subset_self(a, 1) = 1.3;
  print(a);
  

}
