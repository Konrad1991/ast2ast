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

#include "vec.hpp"



// Vector
// subsetting at RHS
// ================================================================

/*
start and end
*/
VEC<double> subset(VEC<double>& inp, int start, int end) {
  VEC<double> t;
  start--;
  end--;
  t.d.resize(end - start + 1);

  for(int i = 0; i < t.d.size(); i++) {
    t.d[i] = inp[i + start];
  }
  return t;
}


/*
desired positions 2
*/
SUBSET<double> subset(VEC<double>& inp, VEC<double>&& ip) {

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
one position
*/
VEC<double>& subset_self(VEC<double>& inp, int pos) {
  pos--;
  inp.subsetted = true;
  inp.indices.resize(1);
  inp.indices[0] = pos;
  return inp;
}

#endif
