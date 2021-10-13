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


#ifndef SUBSETTING
#define SUBSETTING

#include "matrix.hpp"



// Vector
// subsetting at RHS
// ================================================================


SUBSET<double> subset(VEC<double>& inp, int start, int end) {
  /*
  SUBSET<double> t;
  t.sub.resize(end - start + 1);

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp(i + start);
  }
  return t;

  */
  SUBSET<double> t;
  start--;
  end--;
  t.sub.resize(end - start + 1);

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp(i + start);
  }
  return t;
}



SUBSET<double> subset(VEC<double>& inp, std::vector<int>& ip) {

  SUBSET<double> t;
  int start = ip[0] - 1;
  int end = ip.back() - 1;
  t.sub.resize(end - start + 1);

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp(i + start);
  }
  return t;
}

SUBSET<double> subset(VEC<double>& inp, int pos) {

  SUBSET<double> t;
  int start = pos -1;
  int end = pos - 1;
  t.sub.resize(end - start + 1);

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp(i + start);
  }
  return t;
}

// subsetting at LHS
// ================================================================

VEC<double>& subset(VEC<double>& inp, int start, int end, std::string self) {

  inp.subsetted = true;
  inp.indices.resize((end -1) - (start -1) + 1);
  for(int i = 0; i < inp.indices.size(); i++) {
    inp.indices[i] = start + i -1;
  }
  return inp;
}

VEC<double>& subset_self(VEC<double>& inp, int start, int end) {

  inp.subsetted = true;
  inp.indices.resize((end -1) - (start -1) + 1);
  for(int i = 0; i < inp.indices.size(); i++) {
    inp.indices[i] = start + i -1;
  }
  return inp;
}

VEC<double>& subset_self(VEC<double>& inp, std::vector<double>& ip) {

  int start = ip[0] - 1;
  int end = ip.back() - 1;

  inp.subsetted = true;
  inp.indices.resize((end -1) - (start -1) + 1);
  for(int i = 0; i < inp.indices.size(); i++) {
    inp.indices[i] = start + i -1;
  }
  return inp;
}

VEC<double>& subset_self(VEC<double>& inp, int pos) {

  int start = pos - 1;
  int end = start;

  inp.subsetted = true;
  inp.indices.resize((end -1) - (start -1) + 1);
  for(int i = 0; i < inp.indices.size(); i++) {
    inp.indices[i] = start + i -1;
  }
  return inp;
}








// Matrix
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

double subset(MAT<double>& inp, int row, int col) {
  return inp(row + col);
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

MAT<double>& subset_self(MAT<double>& inp, int row, int col) {

  inp.subsetted = true;
  inp.indices.resize((row + 1)*(col + 1) );
  for(int i = 0; i < inp.indices.size(); i++) {
    inp.indices[i] = row + col + i;
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







#endif
