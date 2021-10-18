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

You should have received a copy of the GNU General Public License along with pso
If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4
*/

#ifndef MATRIX_H
#define MATRIX_H

#include "util.hpp"
#include "vec.hpp"

/*
matrix module
*/
template< typename T, typename R = std::vector<T> >
class MAT {

public:

  // Constructors
  // ================================================================
  R d; // vector representation of matrix column wise
  int ncols;
  int nrows;
  bool subsetted;
  std::vector<int> indices;
  MAT(const int rows, const int cols) : d(rows*cols), subsetted(0), nrows(rows), ncols(cols) {}
  MAT(const int rows, const int cols, const double value) : d(rows*cols, value), subsetted(0), nrows(rows), ncols(cols) {}
  MAT(const R& other_MAT) : d(other_MAT), subsetted(0) {}
  MAT() : subsetted(0), nrows(0), ncols(0) {}
  MAT(SUBSET<T>&& inp) : d(inp.sub), subsetted(0) {}

  void matinit(SEXP2MAT input) {
    d.resize(input.v.size());
    for(int i = 0; i < d.size(); i++) {
      d[i] = input.v[i];
    }
    subsetted = false;
    ncols = input.ncols;
    nrows = input.nrows;
  }

  // ================================================================


  // Operator=
  // ================================================================
  MAT& operator=(const T &dob) {
      d.resize(1);
      d[0] = dob;
    subsetted = false;
    return *this;
  }

  MAT& operator=(const SUBSET<T> other_mat) {

    while(other_mat.sub.size() >= d.size()) {
      d.push_back(0);
    }

    if(subsetted == false) {
      for(int i = 0; i < d.size(); i++) {
        d[i] = other_mat.sub[i];
      }
    } else {

      for(int i = 0; i < indices.size(); i++) {
        d[indices[i]] = other_mat.sub[indices[i]];
      }
    }

    subsetted = false;
    return *this;
  }

  template<typename T2, typename R2>
  MAT& operator=(const VEC<T2, R2> &other_MAT) { // other mat stored as VEC
    ncols = other_MAT.ncols;
    nrows = other_MAT.nrows;
    while(other_MAT.size() > d.size()) {
      d.push_back(0);
    }

    if(subsetted == false) {
      for(int i = 0; i < d.size(); i++) {
        d[i] = other_MAT[i];
      }
    } else {
      for(int i = 0; i < indices.size(); i++) {
        d[indices[i]] = other_MAT[i];
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

 int ui_ncols() {
   return ncols;
 }

 int ui_nrows() {
   return nrows;
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

 // access from C++ start at 0
 T& g(int row, int col) {
   return d[ncols*(col) + (row) - (col) ];
 }

 T& g(int row, int col) const {
   return d[ncols*(col) + (row) - (col) ];
 }

 // access as R user start at 1
 T& ui_g(int row, int col) {
   return d[ncols*(col - 1) + (row -1) - (col -1) ];
 }

 T& ui_g(int row, int col) const {
   return d[ncols*(col - 1) + (row -1) - (col -1) ];
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

bool is_subsetted() const {
  return subsetted;
}

// ================================================================

// plotting
// ================================================================
friend std::ostream& operator<<(std::ostream& os, const MAT& v) {
  if(v.subsetted == true) {
    for(int i = 0; i < v.indices.size(); i++) {

      os << v[v.indices[i]] << "\t";
      if(i % v.ncols == v.nrows) {
        os << std::endl;
      }
    }
  } else {
    for(int i = 0; i < v.size(); i++) {
      os << v[i] << "\t";
      if(i % v.ncols == v.nrows) {
        os << std::endl;
      }
    }
  }
      return os;
};
// ================================================================

};

#endif
