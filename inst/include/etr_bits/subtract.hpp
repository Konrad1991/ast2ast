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

/*
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

  VVMINUS(const L &a, const R &b, bool ismatrix_, int rows, int cols) : l(a), r(b),
         ismatrix(ismatrix_), rows_(rows), columns_(cols) {

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
*/


template<typename T, typename L, typename R>
class VVMINUS {

private:
  const L& l; //const L& l;
  const R& r; //const R& r;
  bool ismatrix;
  int rows_;
  int columns_;

public:

  VVMINUS(const L &a, const R &b, bool ismatrix_, int rows, int cols) : l(a), r(b),
         ismatrix(ismatrix_), rows_(rows), columns_(cols) {}

   T operator[](const int i) const {
     return l[i % l.size()] - r[i % r.size()];
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
inline VEC< T, VVMINUS< T, L, R > > operator-(const VEC<T, L>& a, const VEC<T, R>& b) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;
  if( (a.im() == true) || (b.im() == true) || (a.im() == true && b.im() == true) ) {
    ismatrix_ = true;

    if( (a.im() == true) && (b.im() == true) ) {
      nrows_ = (a.nr() > b.nr()) ? a.nr() : b.nr();
      ncols_ = (a.nc() > b.nc()) ? a.nc() : b.nc();
    } else if( (a.im() == false) && (b.im() == true) ) {
      nrows_ = b.nr();
      ncols_ = b.nc();
    } else if( (a.im() == true) && (b.im() == false) ) {
      nrows_ = a.nr();
      ncols_ = a.nc();
    } else {
      #ifdef RLANG
        Rcpp::stop("Error");
      #else
        exit (EXIT_FAILURE);
      #endif
    }

  }


  VEC<T, VVMINUS<T, L, R> > ret(VVMINUS<T, L, R>(a.data(), b.data(), ismatrix_, nrows_, ncols_));

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
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
     return l[i % l.size()] - r;
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
inline VEC< T, VSMINUS< T, L, R > > operator-(const VEC<T, L>& a, const R& b) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VSMINUS<T, L, R> > ret (VSMINUS<T, L, R>(a.data(), b, a.im(), a.nr(), a.nc() ) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}


template<typename T, typename L, typename R>
class SVMINUS {

private:
  const R& r;
  const L& l;
  const bool ismatrix;
  const int nrows;
  const int ncols;

public:
  SVMINUS(const R& a, const L &b, bool ismatrix_, int nrows_, int ncols_ ) :
     r(a), l(b), ismatrix(ismatrix_), nrows(nrows_), ncols(ncols_) { }

   T operator[](const int i) const {
     return r - l[i % l.size()];
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
inline VEC< T, SVMINUS< T, L, R > > operator-(const R& a, const VEC<T, L>&  b) {
  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(b.im() == true) {
    ismatrix_ = true;
    nrows_ = b.nr();
    ncols_ = b.nc();
  }

  VEC<T, SVMINUS<T, L, R> > ret (SVMINUS<T, L, R>(a, b.data(), b.im(), b.nr(), b.nc() ) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}







// -SEXP_VARIABLE --> check whether this is necessary for other functions!!!!!!!
// this only holds true for +, -, * and :. Because other functions require both (or all) arguments to compile successfully
// in R /2 and *2 does not work! Only + and - work in this way!
// +1 does not compile!
// Thus bug should be removed!
template<typename T, typename L>
class VMINUS {
  
private:
  const L& l;
  bool ismatrix;
  int nrows;
  int ncols;
  
public:
  VMINUS(const L &a, bool ismatrix_, int nrows_, int ncols_ ) :
  l(a), ismatrix(ismatrix_), nrows(nrows_), ncols(ncols_) {}
  
  T operator[](const int i) const {
    return -l[i % l.size()];
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


template<typename T, typename L>
inline VEC< T, VMINUS< T, L> > operator-(const VEC<T, L>& a) {
  
  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;
  
  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }
  
  VEC<T, VMINUS<T, L> > ret (VMINUS<T, L>(a.data(), a.im(), a.nr(), a.nc() ) );
  
  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;
  
  return ret;
}


}

#endif
