/*
R package etr
Copyright (C) 2021 Konrad Krämer

This file is part of R package etr


etr is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with etr
If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4
*/
#ifndef SPAN_H
#define SPAN_H


#include "vec.hpp"

namespace etr {

/*
class which is only a copy of a pointer or used as vector storing data via pointer
*/
template<typename T>
class SPAN {

public:
  T* p;
  int* indices;
  int sz;


  // Constructors
  SPAN(T* ptr; int* indices_, int size_indices) {
    indices = new[size_indices];
    sz = size_indices;
    for(int i = 0; i < size_indices; i++) {
      indices[i] = indices_[i]:
    }

    p = ptr;
  }

  SPAN(const SPAN<T>& other) {
    indices = new[other.sz];
    sz = other.sz;
    for(int i = 0; i < sz; i++) {
      indices[i] = other.indices[i];
    }

    p = other.p;
  }


  // Destructor
  ~SPAN() {
    delete[] indices;
  }

  int size() const {
    return sz;
  }

  T* data() const {
    return p;
  }

  SPAN& operator=(const SPAN& other_SPAN) {

    indices = new[other.sz];
    sz = other.sz;
    for(int i = 0; i < sz; i++) {
      indices[i] = other.indices[i];
    }

    p = other.p;

    return *this;
  }

  // 1 indexed array
  T& operator[](int pos) const {
    int pos_ = indices[pos];
    if(pos_ < 0) {
      std::cerr << "Error: out of boundaries --> value below 1" << std::endl;

      #ifdef R
        Rcpp::stop("Error");
      #else
        std::terminate();
      #endif
    } else if(pos_ >= sz) {
      std::cerr << "Error: out of boundaries --> value beyond size of vector" << std::endl;
      #ifdef R
        Rcpp::stop("Error");
      #else
        std::terminate();
      #endif
    }
    return p[pos_];
  }

  auto begin() const {
    return It<T>{p};
  }

  auto end() const {
    return It<T>{p + sz};
  }

  T& back() {
    return p[sz];
  }

  };


}

#endif
