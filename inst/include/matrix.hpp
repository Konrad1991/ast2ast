#ifndef MATRIX
#define MATRIX

#include "vec.hpp"

template< typename T, typename R = std::vector<T> >
class MAT: public VEC<T> {

public:
  T operator()(int i, int j) const {
    return VEC<T>::d[i];
  }

  T& operator()(int i, int j) {
    return VEC<T>::d[i];
  }

  
};

#endif
