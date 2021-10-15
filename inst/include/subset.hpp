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

/*
start and end
*/
SUBSET<double> subset(VEC<double>& inp, int start, int end) {
  SUBSET<double> t;
  t.matrix = false;
  start--;
  end--;
  t.sub.resize(end - start + 1);

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp[i + start];
  }
  return t;
}

/*
desired positions 1
*/
SUBSET<double> subset(VEC<double>& inp, std::vector<int>& ip) {

  SUBSET<double> t;
  t.matrix = false;
  int start = ip[0] - 1;
  int end = ip.back() - 1;
  t.sub.resize(end - start + 1);

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp[i + start];
  }
  return t;
}

/*
desired positions 2
*/
SUBSET<double> subset(VEC<double>& inp, VEC<double>& ip) {

  SUBSET<double> t;
  t.matrix = false;
  int start = ip[0] - 1;
  int end = ip.d.back() - 1;
  t.sub.resize(end - start + 1);

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp[i + start];
  }
  return t;
}

/*
desired positions 3
*/
SUBSET<double> subset(VEC<double>& inp, std::vector<double>& ip) {

  SUBSET<double> t;
  t.matrix = false;
  int start = ip[0] - 1;
  int end = ip.back() - 1;
  t.sub.resize(end - start + 1);

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp[i + start];
  }
  return t;
}

/*
desired positions 4
*/
SUBSET<double> subset(VEC<double>& inp, SUBSET<double>& ip) {

  SUBSET<double> t;
  t.matrix = false;
  int start = ip.sub[0] - 1;
  int end = ip.sub.back() - 1;
  t.sub.resize(end - start + 1);

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp[i + start];
  }
  return t;
}

// matrix subsetting is trash

/*
one position
*/
double subset(VEC<double>& inp, int pos) {
  pos--;
  return inp[pos];
}

// subsetting at LHS
// ================================================================
/*
start and end
*/
VEC<double>& subset_self(VEC<double>& inp, int start, int end) {

  inp.subsetted = true;
  inp.indices.resize((end -1) - (start -1) + 1);
  for(int i = 0; i < inp.indices.size(); i++) {
    inp.indices[i] = start + i -1;
  }
  return inp;
}

/*
desired positions
*/
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

/*
desired positions
*/
VEC<double>& subset_self(VEC<double>& inp, VEC<double>& ip) {

  int start = ip[0] - 1;
  int end = ip.d.back() - 1;

  inp.subsetted = true;
  inp.indices.resize((end -1) - (start -1) + 1);
  for(int i = 0; i < inp.indices.size(); i++) {
    inp.indices[i] = start + i -1;
  }
  return inp;
}

/*
desired positions
*/
VEC<double>& subset_self(VEC<double>& inp, SUBSET<double>& ip) {

  int start = ip.sub[0] - 1;
  int end = ip.sub.back() - 1;

  inp.subsetted = true;
  inp.indices.resize((end -1) - (start -1) + 1);
  for(int i = 0; i < inp.indices.size(); i++) {
    inp.indices[i] = start + i -1;
  }
  return inp;
}


/*
one position
*/
VEC<double>& subset_self(VEC<double>& inp, int pos) {
  pos--;
  inp.subsetted = true;
  inp.indices.resize(1);
  inp.indices[0] = pos;
  return inp;
}


// Matrix
// subsetting at RHS
// ================================================================
SUBSET<double> subset(MAT<double>& inp, int start_row, int end_row, int start_col, int end_col) {
  SUBSET<double> t;
  t.matrix = true;
  t.sub.resize((end_row - start_row + 1)*(end_col - start_col + 1) );

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp(i + start_row + start_col);
  }
  t.nrows = end_row - start_row + 1;
  t.ncols = end_col - start_col + 1;

  return t;
}

double subset(MAT<double>& inp, int row, int col) {
  return inp(row + col);
}



// subsetting at LHS
// ================================================================

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







// Subset SUBSETTING

// ================================================================
// subsetting at RHS
// ================================================================
/*
start and end
*/
SUBSET<double> subset(SUBSET<double>& inp, int start, int end) {
  SUBSET<double> t;
  start--;
  end--;
  t.sub.resize(end - start + 1);

  if(inp.matrix == true) {
    t.matrix = true;
    t.ncols = inp.ncols;
    t.nrows = inp.nrows;
  } else {
    t.matrix = false;
  }

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp.sub[i + start];
  }
  return t;
}

/*
desired positions
*/
SUBSET<double> subset(SUBSET<double>& inp, std::vector<int>& ip) {

  SUBSET<double> t;
  int start = ip[0] - 1;
  int end = ip.back() - 1;
  t.sub.resize(end - start + 1);

  if(inp.matrix == true) {
    t.matrix = true;
    t.ncols = inp.ncols;
    t.nrows = inp.nrows;
  } else {
    t.matrix = false;
  }

  for(int i = 0; i < t.sub.size(); i++) {
    t.sub[i] = inp.sub[i + start];
  }
  return t;
}


/*
one position
*/
double subset(SUBSET<double>& inp, int pos) {
  pos--;
  return inp.sub[pos];
}




// subsetting at LHS --> ??????
// ================================================================
// is not needed i think


#endif
