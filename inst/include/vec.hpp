#ifndef VECTOR
#define VECTOR

#include <iostream>
#include <vector>
#include <cassert>

#include "util.hpp"

template< typename T, typename R = std::vector<T> >
class VEC {

private:
  R d; // data
  bool subsetted;
  std::vector<int> indices;

public:
  VEC(const int n) : d(n), subsetted(0) {}
  VEC(const int n, const double value) : d(n, value), subsetted(0) {}
  VEC(const R& other_vec) : d(other_vec), subsetted(0) {}

  T& operator=(const T &other_vec) {
    for(int i = 0; i < d.size(); i++) {
      d[i] = other_vec[i];
    }
    return *this;
  }

  template<typename T2, typename R2>
  VEC& operator=(const VEC<T2, R2> &other_vec) {

    if(subsetted == false) {
      for(int i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
    } else {
      for(int i = 0; i < indices.size(); i++) {
        d[indices[i]] = other_vec[indices[i]];
      }
    }

    subsetted = false;

    return *this;
  }


  int size() const {
   return d.size();
 }

 T operator[](int i) const {
   return d[i];
 }

 T& operator[](int i) {
   return d[i];
 }

 const R& data() const {
   return d;
 }

 R& data() {
   return d;
 }

VEC& subset(std::vector<int> inp) {
   subsetted = true;
   indices.resize(inp.size());
   for(int i = 0; i < inp.size(); i++) {
     indices[i] = inp[i];
   }
   return *this;
}

VEC& subset(int start, int end) {
  subsetted = true;
  indices.resize(end - start);
  for(int i = 0; i < indices.size(); i++) {
    indices[i] = start + i;
  }
  return *this;
}

};

#endif
