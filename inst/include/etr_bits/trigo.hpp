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


#ifndef TRIGO
#define TRIGO

#include "vec.hpp"

namespace etr {

template<typename T, typename L>
class VVSIN {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVSIN(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return sin(r[i % r.size()]);
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
VEC< T, VVSIN< T, L> > sinus(const VEC<T, L>& a) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VVSIN<T, L> > ret (VVSIN<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}


double sinus(double base) {
  return sin(base);
}

double sinus(int base) {
  return sin(static_cast<double>(base));
}



template<typename T, typename L>
class VVsinh {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVsinh(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return sinh(r[i % r.size()]);
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
VEC< T, VVsinh< T, L> > sinush(const VEC<T, L>& a) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VVsinh<T, L> > ret (VVsinh<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}


double sinush(double base) {
  return sinh(base);
}

double sinush(int base) {
  return sinh(static_cast<double>(base));
}


template<typename T, typename L>
class VVasin {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVasin(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return asin(r[i % r.size()]);
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
VEC< T, VVasin< T, L> > asinus(const VEC<T, L>& a) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VVasin<T, L> > ret (VVasin<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}


double asinus(double base) {
  return asin(base);
}

double asinus(int base) {
  return asin(static_cast<double>(base));
}





template<typename T, typename L>
class VVCOS {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVCOS(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return cos(r[i% r.size()]);
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
VEC< T, VVCOS< T, L> > cosinus(const VEC<T, L>& a) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VVCOS<T, L> > ret (VVCOS<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}


double cosinus(double base) {
  return cos(base);
}

double cosinus(int base) {
  return cos(static_cast<double>(base));
}






template<typename T, typename L>
class VVacos {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVacos(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return acos(r[i% r.size()]);
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
VEC< T, VVacos< T, L> > acosinus(const VEC<T, L>& a) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VVacos<T, L> > ret (VVacos<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}


double acosinus(double base) {
  return acos(base);
}

double acosinus(int base) {
  return acos(static_cast<double>(base));
}





template<typename T, typename L>
class VVCOSH {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVCOSH(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return cosh(r[i% r.size()]);
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
VEC< T, VVCOSH< T, L> > cosinush(const VEC<T, L>& a) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VVCOSH<T, L> > ret (VVCOSH<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}


double cosinush(double base) {
  return cosh(base);
}

double cosinush(int base) {
  return cosh(static_cast<double>(base));
}







template<typename T, typename L>
class VVtan {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVtan(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return tan(r[i% r.size()]);
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
VEC< T, VVtan< T, L> > tangens(const VEC<T, L>& a) {
  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VVtan<T, L> > ret (VVtan<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;

}


double tangens(double base) {
  return tan(base);
}

double tangens(int base) {
  return tan(static_cast<double>(base));
}





template<typename T, typename L>
class VVatan {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVatan(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return atan(r[i% r.size()]);
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
VEC< T, VVatan< T, L> > atangens(const VEC<T, L>& a) {
  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VVatan<T, L> > ret (VVatan<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}


double atangens(double base) {
  return atan(base);
}

double atangens(int base) {
  return atan(static_cast<double>(base));
}





template<typename T, typename L>
class VVtanh {

private:
  const L& r; //const L& l;
  bool ismatrix;
  int nrow_;
  int ncol_;

public:

  VVtanh(const L &a, bool r_ismatrix, int r_rows, int r_cols) : r(a) {

    if( r_ismatrix == true) {
      ismatrix = r_ismatrix;
      nrow_ = r_rows;
      ncol_ = r_cols;
    }

  }

   T operator[](const int i) const {
     return tanh(r[i% r.size()]);
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
VEC< T, VVtanh< T, L> > tangensh(const VEC<T, L>& a) {
  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if(a.im() == true) {
    ismatrix_ = true;
    nrows_ = a.nr();
    ncols_ = a.nc();
  }

  VEC<T, VVtanh<T, L> > ret (VVtanh<T, L>(a.data(), a.im(), a.nrow(), a.ncol()) );

  ret.ismatrix = ismatrix_;
  ret.ncols = ncols_;
  ret.nrows = nrows_;

  return ret;
}


double tangensh(double base) {
  return tanh(base);
}

double tangensh(int base) {
  return tanh(static_cast<double>(base));
}

}

#endif
