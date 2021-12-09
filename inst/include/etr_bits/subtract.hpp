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

#ifndef MINUS
#define MINUS

#include "vec.hpp"

namespace etr {

template<typename T, typename L, typename R>
class VVMINUS {

private:
  const L& l; //const L& l;
  const R& r; //const R& r;
  std::vector<int> indices1;
  std::vector<int> indices2;
  int columns_;
  int rows_;
  bool ismatrix;

public:

  VVMINUS(const L &a, const R &b, bool l_ismatrix, bool r_ismatrix,
        int l_rows, int l_cols, int r_rows, int r_cols) : l(a), r(b) {

    bool _l_ismatrix = l_ismatrix;
    bool _r_ismatrix = r_ismatrix;
    int _l_nrow = l_rows;
    int _r_nrow = r_rows;
    int _l_ncol = l_cols;
    int _r_ncol = r_cols;

    if( ((_l_ismatrix == true) || (_r_ismatrix == true )) ||
        ((_l_ismatrix == true) && (_r_ismatrix == true )) ) {
        ismatrix = true;
      if( (_l_ismatrix == true) && (_r_ismatrix == false) ){
        columns_ = _l_ncol;
        rows_ = _l_nrow;
      } else if ((_l_ismatrix == false) && (_r_ismatrix == true)) {
        columns_ = _r_ncol;
        rows_ = _r_nrow;
      } else if((_l_ismatrix == true) && (_r_ismatrix == true)) {
        columns_ = (_l_ncol > _r_ncol) ? _l_ncol : _r_ncol;
        rows_ = (_l_nrow > _r_nrow) ? _l_nrow : _r_nrow;
      } else {
        exit(0);
      }
    }

       if(l.size() > r.size()) {
         //ass((l.size() % r.size()) == 0, "Vector is not multiple of other vector");
         indices1.resize(l.size());
         indices2.resize(l.size());
         for(int i = 0; i < indices2.size(); i++) {
           indices1[i] = i;

           int times = floor(i/r.size());
           indices2[i] =  i - times*r.size();
         }
       } else if(r.size() > l.size()) {
         //ass((l.size() % r.size()) == 0, "Vector is not multiple of other vector");
         indices1.resize(r.size());
         indices2.resize(r.size());
         for(int i = 0; i < indices2.size(); i++) {
           indices2[i] = i;

           int times = floor(i/l.size());
           indices1[i] =  i - times*l.size();
         }
     } else if(r.size() == l.size()) {
       indices1.resize(l.size());
       indices2.resize(r.size());
       for(int i = 0; i < indices2.size(); i++) {
         indices1[i] = i;
         indices2[i] = i;
       }
     }
   }

   T operator[](const int i) const {
     return l[indices1[i]] - r[indices2[i]];
   }

   int size() const {
     int sz = (l.size() > r.size()) ? l.size() : r.size();
     return   sz;      //l.size(); // correct?
   }

   bool im() const {
     return ismatrix;
   }

   int nc() const {
     return columns_;
   }

   int nr() const {
     return rows_;
   }

};

template<typename T, typename L, typename R>
VEC< T, VVMINUS< T, L, R > > operator-(const VEC<T, L>& a, const VEC<T, R>& b) {
    return VEC<T, VVMINUS<T, L, R> > (VVMINUS<T, L, R>(a.data(), b.data(),
                                     a.im(), b.im(),
                                     a.nrow(), a.ncol(), b.nrow(), b.ncol() ) );
}

template<typename T, typename L, typename R>
class VSMINUS {

private:
  const L& l;
  const R& r;
  bool ismatrix;
  int nrows;
  int ncols;

public:
  VSMINUS(const L &a, const R &b, bool ismatrix_, int nrows_, int ncols_ ) :
     l(a), r(b), ismatrix(ismatrix_), nrows(nrows_), ncols(ncols_) {}

   T operator[](const int i) const {
     return l[i] - r;
   }

   int size() const {
     return l.size();
   }


   bool im() const {
     return ismatrix;
   }

   int nc() const {
     return ncols;
   }

   int nr() const {
     return nrows;
   }

};

template<typename T, typename L, typename R>
VEC< T, VSMINUS< T, L, R > > operator-(const VEC<T, L>& a, const R& b) {
    return VEC<T, VSMINUS<T, L, R> > (VSMINUS<T, L, R>(a.data(), b, a.im(), a.nr(), a.nc() ) );
}


template<typename T, typename L, typename R>
class SVMINUS {

private:
  const L& l;
  const R& r;
  const bool ismatrix;
  const int nrows;
  const int ncols;

public:
  SVMINUS(const R& a, const L &b, bool ismatrix_, int nrows_, int ncols_ ) :
     r(a), l(b), ismatrix(ismatrix_), nrows(nrows_), ncols(ncols_) { }

   T operator[](const int i) const {
     return r - l[i];
   }

   int size() const {
     return l.size();
   }


   bool im() const {
     return ismatrix;
   }

   int nc() const {
     return ncols;
   }

   int nr() const {
     return nrows;
   }

};


template<typename T, typename L, typename R>
VEC< T, SVMINUS< T, L, R > > operator-(const R& a, const VEC<T, L>&  b) {
    return VEC<T, SVMINUS<T, L, R> > (SVMINUS<T, L, R>(a, b.data(), b.im(), b.nr(), b.nc() ) );
}

}

#endif
