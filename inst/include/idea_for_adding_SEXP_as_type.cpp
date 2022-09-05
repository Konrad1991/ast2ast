// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
// [[Rcpp::plugins(cpp17)]]

// class adding SEXP and VEC<double>
template<typename T, typename R>
class SEXPVPLUS {
  public:
  const double* l;
  const R& r;
  int size_l;
  int size_r;

  SEXPVPLUS(SEXP left, const R& r_, int size_l, int size_r) :
    l(REAL(left)), r(r_), size_l(size_l), size_r(size_r) {}
  
  T operator[](const int i) const {
    return l[i % size_l] + r[i % size_r];
  }
  
  int size() const {
    return size_l > size_r ? size_l : size_r;
  }
  
  bool im() const {
    return false;
  }
  
  int nc() const {
    return 0;
  }
  
  int nr() const {
    return 0;
  }
};


template<typename T, typename R>
VEC<T, SEXPVPLUS<T, R> > add(const SEXP l, const VEC<T, R>& r) {
  VEC<T, SEXPVPLUS<T, R> > ret(SEXPVPLUS<T, R>(l, r.data(), Rf_length(l), r.size() ));
  return ret;
}









// class adding VEC<double> and SEXP
template<typename T, typename L>
class VSEXPPLUS {
public:
  const L& l;
  const double* r;
  int size_l;
  int size_r;
  
  VSEXPPLUS(const L& l_, SEXP right, int size_l, int size_r) :
    l(l_), r(REAL(right)), size_l(size_l), size_r(size_r) {}
  
  T operator[](const int i) const {
    return l[i % size_l] + r[i % size_r];
  }
  
  int size() const {
    return size_l > size_r ? size_l : size_r;
  }
  
  bool im() const {
    return false;
  }
  
  int nc() const {
    return 0;
  }
  
  int nr() const {
    return 0;
  }
};


template<typename T, typename L>
VEC<T, VSEXPPLUS<T, L> > add(const VEC<T, L>& l, const SEXP r) {
  VEC<T, VSEXPPLUS<T, L> > ret(VSEXPPLUS<T, L>(l.data(), r, l.size(),  Rf_length(l) ));
  return ret;
}









// class adding SEXP and SEXP
template<typename T = double>
class SEXPSEXPPLUS {
public:
  const double* l;
  const double* r;
  int size_l;
  int size_r;
  
  SEXPSEXPPLUS(SEXP left, SEXP right, int size_l, int size_r) :
    l(REAL(left)), r(REAL(right)), size_l(size_l), size_r(size_r) {}
  
  T operator[](const int i) const {
    return l[i % size_l] + r[i % size_r];
  }
  
  int size() const {
    return size_l > size_r ? size_l : size_r;
  }
  
  bool im() const {
    return false;
  }
  
  int nc() const {
    return 0;
  }
  
  int nr() const {
    return 0;
  }
};


template<typename T = double>
VEC<T, SEXPSEXPPLUS<T> > add(const SEXP l, const SEXP r) {
  VEC<T, SEXPSEXPPLUS<T> > ret(SEXPSEXPPLUS<T>(l, r, Rf_length(l),  Rf_length(r) ));
  return ret;
}




















// class adding SEXP and double
class SEXPDOUBLEPLUS {
public:
  const double* l;
  const double r;
  int size_l;
  int size_r;
  
  SEXPDOUBLEPLUS(SEXP left, double right, int size_l, int size_r) :
    l(REAL(left)), r(right), size_l(size_l), size_r(size_r) {}
  
  double operator[](const int i) const {
    return l[i % size_l] + r;
  }
  
  int size() const {
    return size_l > size_r ? size_l : size_r;
  }
  
  bool im() const {
    return false;
  }
  
  int nc() const {
    return 0;
  }
  
  int nr() const {
    return 0;
  }
};



VEC<double, SEXPDOUBLEPLUS > add(const SEXP l, const double r) {
  VEC<double, SEXPDOUBLEPLUS > ret(SEXPDOUBLEPLUS(l, r, Rf_length(l),  0 ));
  return ret;
}















// class adding double to SEXP
class DOUBLESEXPPLUS {
public:
  const double l;
  const double* r;
  int size_l;
  int size_r;
  
  DOUBLESEXPPLUS(double left, SEXP right, int size_l, int size_r) :
    l(l), r(REAL(right)) , size_l(size_l), size_r(size_r) {}
  
  double operator[](const int i) const {
    return l + r[i % size_r];
  }
  
  int size() const {
    return size_l > size_r ? size_l : size_r;
  }
  
  bool im() const {
    return false;
  }
  
  int nc() const {
    return 0;
  }
  
  int nr() const {
    return 0;
  }
};



VEC<double, DOUBLESEXPPLUS > add(const double l, const SEXP r) {
  VEC<double, DOUBLESEXPPLUS > ret(DOUBLESEXPPLUS(l, r, 0, Rf_length(r) ));
  return ret;
}





// [[Rcpp::export]]
void test(SEXP a, SEXP b, SEXP c, SEXP l) {
  
  sexp r = coca(1, 2, 3);
  
  sexp out = add(l, r);
  print(out);
  print();
  print(add(l, r + 1));
  print();
  
  sexp ret = add(r + 2, l);
  print(ret);
  print();
  
  sexp ret2 = add(l, l);
  print(ret2);
  print();
  
  sexp ret3 = add(a, r) + add(b, c);
  print(ret3);
  print();
  
  sexp ret4 = add(b, add(a, 1) );
  print(ret4);
  print();
  
  sexp ret5 = add(1, c) + add(3, b);
  print(ret5);
  print();
  
}


/*** R
a <- 1
b <- 2
c <- 3
l <- c(1, 2, 3)
test(a , b, c, l)
*/
