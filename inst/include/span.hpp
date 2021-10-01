/*
R package ast2ast
Copyright (C) 2021 Konrad Krämer

This file is part of R package ast2ast


ast2ast is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with ast2ast
If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4
*/

#ifndef SPAN_H
#define SPAN_H

#include "util.hpp"
#include "vec.hpp"


void realloc(double* p, int size1, int size2) {
  if(size1 == size2) {
    return;
  }

  int min = (size1 > size2) ? size2 : size1;
  double* temp = new double[size2];

  std::copy(p, p + min, temp);
  p = temp;
  delete[] temp;
}

/*
span module
*/
template< typename T, typename R = T* >
class SPAN {

public:

  // Constructors
  // ================================================================
  R d;
  bool subsetted;
  std::vector<int> indices;
  int sz;

  SPAN(const R& other_vec) : sz(other_vec.size()), subsetted(0) {
    d = other_vec.pointer();
  }

  SPAN(VEC<double> v) {
    d = v.d.data();
    subsetted = false;
    sz = v.d.size();
  }
  // ================================================================


  // Operator=
  // ================================================================
  T& operator=(const T &other_SPAN) {

    if(other_SPAN.size() >= sz) {
      realloc(d, sz, other_SPAN.size());
    }

    if(subsetted == false) {
      for(int i = 0; i < d.size(); i++) {
        d[i] = other_SPAN[i];
      }
    } else {
      for(int i = 0; i < indices.size(); i++) {
        d[indices[i]] = other_SPAN[indices[i]];
      }
    }

    subsetted = false;
    return *this;
  }

  template<typename T2, typename R2>
  SPAN& operator=(const SPAN<T2, R2> &other_SPAN) {

    if(other_SPAN.size() >= sz) {
      realloc(d, sz, other_SPAN.size());
    }

    if(subsetted == false) {
      for(int i = 0; i < sz; i++) {
        d[i] = other_SPAN[i];
      }
    } else {
      for(int i = 0; i < indices.size(); i++) {
        d[indices[i]] = other_SPAN[i];
      }
    }

    subsetted = false;

    return *this;
  }


  template<typename T2, typename R2>
  SPAN& operator=(const VEC<T2, R2> &other_VEC) {

    if(other_VEC.size() >= sz) {
      //realloc(d, sz, other_VEC.size());
    }

    if(subsetted == false) {
      for(int i = 0; i < sz; i++) {
        d[i] = other_VEC[i];
      }
    } else {
      for(int i = 0; i < indices.size(); i++) {
        d[indices[i]] = other_VEC[i];
      }
    }

    subsetted = false;

    return *this;
  }
  // ================================================================


  // getter methods
  // ================================================================
  int size() const {
   return sz;
 }

 T operator[](int i) const {
   return d[i];
 }

 T& operator[](int i) {
   return d[i];
 }

 T operator()(int i) const {
   return d[i];
 }

 T& operator()(int i) {
   return d[i];
 }

 // access as R user
 T& ui_g(int i) {
   return d[i-1];
 }

 T& ui_g(int i) const {
   return d[i-1];
 }

 int ui_length() {
   return sz;
 }

 const R& data() const {
   return d;
 }

 R& data() {
   return d;
 }
// ================================================================


};

#endif
