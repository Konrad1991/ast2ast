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
  MAT() {}
  MAT(SUBSET<T>&& inp) : d(inp.sub), subsetted(0) {}

  MAT(VEC<T>& inp) {
    if(inp.size() > (ncols*nrows)) {
        Rcpp::stop("not compatible sizes detected");
    }

    for(int i = 0; i < inp.size(); i++) {
      d[i] = inp[i];
    }

  }


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
  T& operator=(const T &other_MAT) {

    while(other_MAT.size() >= d.size()) {
      d.push_back(0);
    }

    if(subsetted == false) {
      for(int i = 0; i < d.size(); i++) {
        d[i] = other_MAT[i];
      }
    } else {
      for(int i = 0; i < indices.size(); i++) {
        d[indices[i]] = other_MAT[indices[i]];
      }
    }

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


// subsetting
// ================================================================
MAT& subset(std::vector<int> rows, std::vector<int> cols) {
   subsetted = true;
   indices.resize(rows.size() + cols.size());
   for(int i = 0; i < indices.size(); i++) {
     indices[i] = ncols*(cols[i]) + (rows[i]) - (cols[i]);
   }
   return *this;
}

MAT& subset(int start_row, int end_row, int start_col, int end_col) {
  subsetted = true;
  indices.resize( (end_row - start_row + 1)*(end_col - start_col + 1) );
  for(int i = 0; i < indices.size(); i++) {
    indices[i] = start_row + start_col + i;
  }
  return *this;
}

// called by R User --> index start at 1
MAT& ui_subset(std::vector<int> rows, std::vector<int> cols) {
   subsetted = true;
   indices.resize(rows.size() + cols.size());
   for(int i = 0; i < indices.size(); i++) {
     indices[i] = ncols*(cols[i]) + (rows[i]) - (cols[i]) - 1;
   }
   return *this;
}

MAT& ui_subset(int start_row, int end_row, int start_col, int end_col) {
  subsetted = true;
  indices.resize( (end_row -1 - start_row -1 + 1)*(end_col -1 - start_col -1 + 1) );
  for(int i = 0; i < indices.size(); i++) {
    indices[i] = start_row + start_col + i -1;
  }
  return *this;
}

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

// subsetting at RHS
// ================================================================
SUBSET<double> subset(MAT<double>& inp, int start_row, int end_row, int start_col, int end_col) {
  SUBSET<double> t;
  t.sub.resize((end_row - start_row + 1)*(end_col - start_col + 1));

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp(i + start_row + start_col);
  }
  t.nrows = end_row - start_row;
  t.ncols = end_col - start_col;

  return t;
}


// subsetting at LHS
// ================================================================
MAT<double>& subset(MAT<double>& inp, int start_row, int end_row, int start_col, int end_col, std::string self) {

  inp.subsetted = true;
  inp.indices.resize((end_row - start_row + 1)*(end_col - start_col + 1) );
  for(int i = 0; i < inp.indices.size(); i++) {
    inp.indices[i] = start_row + start_col + i;
  }
  return inp;
}


MAT<double>& subset_self(MAT<double>& inp, int start_row, int end_row, int start_col, int end_col) {

  inp.subsetted = true;
  inp.indices.resize((end_row - start_row + 1)*(end_col - start_col + 1) );
  for(int i = 0; i < inp.indices.size(); i++) {
    inp.indices[i] = start_row + start_col + i;
  }
  return inp;
}



VEC<double> get_row(MAT<double>& inp, int row) {
  VEC<double> t(inp.ncols);
  for(int i = 0; i < t.size(); i++) {
    t[i] = inp(i + i*inp.ncols + 1);
  }
  return t;
}

VEC<double> get_col(MAT<double>& inp, int col) {
  VEC<double> t(inp.nrows);
  for(int i = 0; i < t.size(); i++) {
    t[i] = inp(i + i*inp.nrows + 1);
  }
  return t;
}

// R user
MAT<double> ui_subset(MAT<double>& inp, int start_row, int end_row, int start_col, int end_col) {
  std::vector<double> temp((end_row -1 - start_row -1 + 1)*(end_col -1 - start_col -1 + 1));
  for(int i = 0; i < temp.size(); i++) {
    temp[i] = inp(i + start_row + start_col) -1;
  }
  MAT<double> t(temp);
  return t;
}



// ================================================================

// print fct
// ================================================================
void print(const MAT<double>& inp) { // const
  if(inp.subsetted == true) {
    for(int i = 0; i < inp.indices.size(); i++) {
      std::cout << inp[inp.indices[i]] << "\t";
      if( (i % inp.ncols) == inp.nrows) {
        std::cout << std::endl;
      }
    }
  } else {

    for(int i = 0; i < inp.nrows; i++) {
      for(int j = 0; j < inp.ncols; j++) {
        std::cout << inp.d[i*inp.nrows + j] << "\t";
      }
        std::cout << std::endl;
    }




  }
}


void print(const MAT<double>& inp, std::string&& message) {

  std::cout << message << std::endl;

  if(inp.subsetted == true) {
    for(int i = 0; i < inp.indices.size(); i++) {
      std::cout << inp[inp.indices[i]] << "\t";
      if(i % inp.ncols == inp.nrows) {
        std::cout << std::endl;
      }
    }
  } else {
    for(int i = 0; i < inp.size(); i++) {
      std::cout << inp[i] << "\t";
      if(i % inp.ncols == inp.nrows) {
        std::cout << std::endl;
      }
    }
  }
}
// ================================================================

#endif
