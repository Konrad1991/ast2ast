#include <iostream>
#include <vector>


template<typename T>
struct EXPRESSION{

  inline const T& self() const {
        return *static_cast<const T*>(this);
  }

};

template<typename L, typename R>
struct PLUS: public EXPRESSION<PLUS<L, R> > {

  const L& l;
  const R& r;

  PLUS(const L& l, const R& r)
    : l(l), r(r) {
      //std::cout <<std::is_arithmetic<R>::value << std::endl;
      /*
      if constexpr(std::is_arithmetic<R>::value){
        std::cout << "test" << std::endl;
      }
      */
    }


  inline double EVALUATION(int i) const {
    if constexpr( (std::is_arithmetic<L>::value == false) & (std::is_arithmetic<R>::value == true) ) {
      return l.GET(i) + r;
    } else if constexpr( (std::is_arithmetic<L>::value == true) & (std::is_arithmetic<R>::value == false) ) {
      return l + r.GET(i);
    } else {
      return l.GET(i) + r.GET(i);
    }
  }
};

struct VECTOR: public EXPRESSION<VECTOR> {

  std::vector<double> dat;

  VECTOR(std::vector<double> inp) {
    dat.resize(inp.size());
    for(int i = 0; i < inp.size(); i++) {
      dat[i] = inp[i];
    }
  }

  template<typename EXPRESSION_TYPE>
  inline VECTOR& operator= (const EXPRESSION<EXPRESSION_TYPE>& inp) {
    const EXPRESSION_TYPE& ip = inp.self();
    for(int i = 0; i < dat.size(); i++) {
      dat[i] = ip.EVALUATION(i);
    }
    return *this;
  }

  inline double GET(int i) const {
    return dat[i];
  }
};


template<typename L, typename R>
inline PLUS<L, R> operator+(const EXPRESSION<L>& l, const EXPRESSION<R>& r) {
  return PLUS<L, R>(l.self(), r.self());
}

/*
template<typename L, typename R>
inline PLUS<L, R> operator+(const EXPRESSION<L>& l, const R& r) {
  return PLUS<L, R>(l.self(), r);
}
*/
/*
template<typename L, typename R>
inline PLUS<L, R> operator+(const L& l, const EXPRESSION<R>& r) {
  return PLUS<L, R>(l, r.self());
}
*/

/*
inline PLUS operator+(const EXPRESSION<VECTOR>& l, const EXPRESSION<VECTOR>& r) {
  return PLUS(l.self(), r.self());
}

inline PLUS operator+(const EXPRESSION<VECTOR>& l, const double& r) {
  return PLUS(l.self(), r);
}

inline PLUS operator+(const double& l, const EXPRESSION<VECTOR>& r) {
  return PLUS(l, r.self());
}
*/


int main(void) {

  std::vector<double> v1{1., 2., 3.};
  std::vector<double> v2{1., 2., 3.};
  std::vector<double> v3{1., 2., 3.};

  VECTOR A(v1);
  VECTOR B(v2);
  VECTOR C(v3);

  A = B + C; // using expression template
  for (int i = 0; i < 3; ++i) {
    //std::cout << A.dat[i] << std::endl;
  }

  // by hand

  // 1. operator+ --> returns struct PLUS; with type VECTOR, VECTOR and constructor called with B and C
  PLUS<VECTOR, VECTOR> P(B, C);
  // 2. operator= in struct VECTOR called --> loop über Zielvektor D und Aufruf von
  // der memberfunction EVALUATION der struct PLUS --> führt Addition der Einzelelemnte von B und C durch
  VECTOR D(v1); std::cout << std::endl;
  D = P;
  for (int i = 0; i < 3; ++i) {
    //std::cout << D.dat[i] << std::endl;
  }

  double test = 1.1;
  PLUS<VECTOR, double> TEST(A, test); // es ist möglich struct aus Vektor und double zu bilden

  D = TEST; // geht nicht, da double keine Funktion GET hat --> gelöst via if constexpr

  D = A + 5.; // geht nicht da man nur ein Object von PLUS von Hand definieren kann
  // der overloaded operator+ könnte mit dem normaln + operator der bereits für den Typ double definiert ist kollidieren
  // Der Operator+ erhält eine Expression. Wenn er allerdings ein double enthält ist das keine Expression!
  for (int i = 0; i < 3; ++i) {
    std::cout << D.dat[i] << std::endl;
  }
  std::cout << std::endl;
  //D = 3. + A;
  for (int i = 0; i < 3; ++i) {
    std::cout << D.dat[i] << std::endl;
  }

  EXPRESSION<double> E;
  //E.self(); // tut nicht richtigen cast finden
  EXPRESSION<VECTOR> EV;
  EV.self(); // liegt nicht daran, es muss eine Referenz drauf sein.

  for (int i = 0; i < 3; ++i) {
    std::cout << D.dat[i] << std::endl;
  }



  double a = 1.;
  const VECTOR AT(v1);
  *static_cast<const double*>(&a);
  *static_cast<const VECTOR*>(&AT);
}
