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


#ifndef EXPONENT
#define EXPONENT

#include "vec.hpp"

namespace etr {

template<typename T, typename L, typename R>
class VVEXP {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;
  double exponent;

public:

  VVEXP(const L &a, bool r_ismatrix, int r_rows, int r_cols, double exponent_) : r(a), exponent(exponent_) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

   }

   T operator[](const int i) const {
     return pow(r[i% r.size()], exponent);
   }

   int size() const {
     return r.size();
   }

   bool im() const {
     return ismatrix;
   }

   int nc() const {
     return ncol_;
   }

   int nr() const {
     return nrow_;
   }

};


template<typename T, typename L, typename R>
inline VEC< T, VVEXP< T, L, R> > operator^(const VEC<T, L>& a, const R exponent) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VVEXP<T, L, R> > ret (VVEXP<T, L, R>(a.data(), a.im(), a.nrow(), a.ncol(), exponent) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}


template<typename T, typename L, typename R>
inline VEC< T, VVEXP< T, L, R> > exp(const VEC<T, L>& a, const R exponent) {

    bool ismatrix_ = false;
    int nrows_ = 0;
    int ncols_ = 0;

    if(a.im() == true) {
      ismatrix_ = true;
      nrows_ = a.nr();
      ncols_ = a.nc();
    }

    VEC<T, VVEXP<T, L, R> > ret (VVEXP<T, L, R>(a.data(), a.im(), a.nrow(), a.ncol(), exponent) );

    ret.ismatrix = ismatrix_;
    ret.ncols = ncols_;
    ret.nrows = nrows_;

    return ret;
}


inline double exp(double base, double exponent) {
  return pow(base, exponent);
}











template<typename T, typename L>
class VVlog {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVlog(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return log(r[i% r.size()]);
   }

   int size() const {
     return r.size();
   }

   bool im() const {
     return ismatrix;
   }

   int nc() const {
     return ncol_;
   }

   int nr() const {
     return nrow_;
   }

};


template<typename T, typename L>
inline VEC< T, VVlog< T, L> > ln(const VEC<T, L>& a) {

    bool ismatrix_ = false;
    int nrows_ = 0;
    int ncols_ = 0;

    if(a.im() == true) {
      ismatrix_ = true;
      nrows_ = a.nr();
      ncols_ = a.nc();
    }

    VEC<T, VVlog<T, L> > ret(VVlog<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

    ret.ismatrix = ismatrix_;
    ret.ncols = ncols_;
    ret.nrows = nrows_;

    return ret;
}


inline double ln(double base) {
  return log(base);
}

inline double ln(int base) {
  return log(static_cast<long double>(base));
}










template<typename T, typename L>
class VVsqrt {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVsqrt(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return sqrt(r[i% r.size()]);
   }

   int size() const {
     return r.size();
   }

   bool im() const {
     return ismatrix;
   }

   int nc() const {
     return ncol_;
   }

   int nr() const {
     return nrow_;
   }

};


template<typename T, typename L>
inline VEC< T, VVsqrt< T, L> > sqroot(const VEC<T, L>& a) {

    bool ismatrix_ = false;
    int nrows_ = 0;
    int ncols_ = 0;

    if(a.im() == true) {
      ismatrix_ = true;
      nrows_ = a.nr();
      ncols_ = a.nc();
    }

    VEC<T, VVsqrt<T, L> > ret(VVsqrt<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

    ret.ismatrix = ismatrix_;
    ret.ncols = ncols_;
    ret.nrows = nrows_;

    return ret;
}


inline double sqroot(double inp) {
  return sqrt(inp);
}

inline double sqroot(int inp) {
  return sqrt(static_cast<long double>(inp));
}





}




#endif
