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

#ifndef ALLOC
#define ALLOC

#include "vec.hpp"


namespace etr {

inline VEC<double> vector(int length) {
  VEC<double> ret(length); // + 1?
  for (int i = 0; i < ret.size(); i++) {
    ret[i] = 0.0;
  }
  return ret;
}

// issue: vector(1, 10) does not work
inline VEC<double> vector(double inp, int length) {
  VEC<double> ret(length, inp);
  return ret;
}

inline VEC<double> rep(double inp, int length) {
  VEC<double> ret(length);
  for(int i = 0; i < ret.size(); i++) {
   ret[i] = inp;
  }
  return ret; 
}

inline VEC<double> rep(double inp, double length) {
  VEC<double> ret(static_cast<int>(length));
  for(int i = 0; i < ret.size(); i++) {
   ret[i] = inp;
  }
  return ret; 
}

template<typename T>
requires std::is_same_v<T, double>
inline VEC<double> rep(T inp, const VEC<double>& length) {
  ass(length.size() == 1, "invalid 'times' argument");
  VEC<double> ret(static_cast<int>(length[0]));
  for(int i = 0; i < ret.size(); i++) {
   ret[i] = inp;
  }
  return ret; 
}

inline VEC<double> rep(const VEC<double>& inp, VEC<double>& length) {
  ass(length.size() == 1, "invalid 'times' argument");
  VEC<double> ret(static_cast<int>(length[0]) * inp.size() );
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = inp[i % inp.size()];
  }
  return ret; 
}

// not copy of vector!
inline VEC<double> vector(VEC<double> inp) {

  ass(inp.size() == 1, "invalid length argument");

  VEC<double> ret(static_cast<int>(inp.d[0])); // +1 ?
  return ret;
}

inline VEC<double> matrix(int nrows_, int ncols_) {
  VEC<double> ret(nrows_, ncols_);
  ret.set_matrix(true);
  ret.set_ncol(ncols_);
  ret.set_nrow(nrows_);
  return ret;
}

inline VEC<double> matrix(double inp, int nrows_, int ncols_) {
  VEC<double> ret(nrows_, ncols_, inp);
  ret.set_matrix(true);
  ret.set_ncol(ncols_);
  ret.set_nrow(nrows_);
  return ret;
}

inline VEC<double> matrix(VEC<double> inp, int nrows_, int ncols_) {
  VEC<double> ret(nrows_, ncols_);
  ret = inp;
  ret.set_matrix(true);
  ret.set_ncol(ncols_);
  ret.set_nrow(nrows_);
  return ret;
}

template <typename T2, typename R2>
inline VEC<double> matrix(const VEC<T2, R2> &inp, int nrows_, int ncols_) {
  VEC<double> ret(nrows_, ncols_);
  ret = inp;
  ret.set_matrix(true);
  ret.set_ncol(ncols_);
  ret.set_nrow(nrows_);
  return ret;
}

} // namespace etr

#endif
