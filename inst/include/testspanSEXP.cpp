#include <Rcpp.h>

#include <tidyCpp>
// [[Rcpp::depends(tidyCpp)]]

class SPAN {
  
  double* sp;
  int size;
  
public:
  SPAN(double* p, int size_) : size(size_), sp(p) {}
  
  double& operator[](int i) {
    return sp[i];
  }
  
};

// [[Rcpp::export]]
SEXP test(SEXP a)
{
  int na;
  double *xa;
  
  R::Protect pa(R::coerceVectorNumeric(a));
  na = R::length(pa); 
  xa = R::numericPointer(pa); 
  
  double* p;
  R::Protect ab(R::allocVectorNumeric(na));
  p = R::numericPointer(ab);
  
  SPAN s(xa, na);
  
  for(int i = 0; i < na; i++) {
    s[i] = static_cast<double>(i) + 20.;
    p[i] = s[i];
  }
  
  return ab;
}

/*** R
test(c(1,2,3))
*/
