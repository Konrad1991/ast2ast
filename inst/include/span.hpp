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
  int size;
  SPAN(VEC v) {
    d = v.d.data();
    subsetted = false;
    size = v.d.size();
  }
  // ================================================================


  // Operator=
  // ================================================================
  T& operator=(const T &other_SPAN) {

    while(other_SPAN.size() >= d.size()) {
      d.push_back(0);
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

    while(other_SPAN.size() > d.size()) {
      d.push_back(0);
    }

    if(subsetted == false) {
      for(int i = 0; i < d.size(); i++) {
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
  // ================================================================


  // getter methods
  // ================================================================
  int size() const {
   return d.size();
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
   return d.size();
 }

 const R& data() const {
   return d;
 }

 R& data() {
   return d;
 }
// ================================================================


// subsetting
// ================================================================
 void data_sub(std::SPANtor<double>& temp) const{
    temp.resize(indices.size());
    for(int i = 0; i < temp.size(); i++) {
      temp[indices[i]] = d[indices[i]];
    }
}


SPAN& subset(std::SPANtor<int> inp) {
   subsetted = true;
   indices.resize(inp.size());
   for(int i = 0; i < inp.size(); i++) {
     indices[i] = inp[i];
   }
   return *this;
}

SPAN& subset(int start, int end) {
  subsetted = true;
  indices.resize(end - start + 1);
  for(int i = 0; i < indices.size(); i++) {
    indices[i] = start + i;
  }
  return *this;
}

// called by R User --> index start at 1 --> correct?
SPAN& ui_subset(std::SPANtor<int> inp) {
   subsetted = true;
   indices.resize(inp.size());
   for(int i = 0; i < inp.size(); i++) {
     indices[i] = inp[i] - 1;
   }
   return *this;
}

// called by R User --> index start at 1
SPAN& ui_subset(int start, int end) {
  subsetted = true;
  indices.resize((end -1) - (start -1) + 1);
  for(int i = 0; i < indices.size(); i++) {
    indices[i] = start + i -1;
  }
  return *this;
}

bool is_subsetted() const {
  return subsetted;
}

// ================================================================

// plotting
// ================================================================
friend std::ostream& operator<<(std::ostream& os, const SPAN& v) {
  if(v.subsetted == true) {
    for(int i = 0; i < v.indices.size(); i++) {
      os << v[v.indices[i]] << std::endl;
    }
  } else {
    for(int i = 0; i < v.size(); i++) {
      os << v[i] << std::endl;
    }
  }
      return os;
};
// ================================================================

};

// subsetting at RHS
// ================================================================
SPAN<double> subset(SPAN<double>& inp, int start, int end) {
  std::SPANtor<double> temp(end - start + 1);
  for(int i = 0; i < temp.size(); i++) {
    temp[i] = inp(i + start);
  }
  SPAN<double> t(temp);
  return t;
}

SPAN<double> ui_subset(SPAN<double>& inp, int start, int end) {
  std::SPANtor<double> temp( (end -1) - (start -1) + 1);
  for(int i = 0; i < temp.size(); i++) {
    temp[i] = inp(i + start) - 1;
  }
  SPAN<double> t(temp);
  return t;
}
// ================================================================

// print fct
// ================================================================
void print(const SPAN<double>& inp) { // const
  if(inp.subsetted == true) {
    for(int i = 0; i < inp.indices.size(); i++) {
      std::cout << inp[inp.indices[i]] << std::endl;
    }
  } else {
    for(int i = 0; i < inp.size(); i++) {
      std::cout << inp[i] << std::endl;
    }
  }
}


void print(const SPAN<double>& inp, std::string&& message) {

  std::cout << message << std::endl;

  if(inp.subsetted == true) {
    for(int i = 0; i < inp.indices.size(); i++) {
      std::cout << inp[inp.indices[i]] << std::endl;
    }
  } else {
    for(int i = 0; i < inp.size(); i++) {
      std::cout << inp[i] << std::endl;
    }
  }
}
// ================================================================

#endif
