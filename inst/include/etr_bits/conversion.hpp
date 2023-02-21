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
#ifndef CONVERT_H
#define CONVERT_H
  
#include "vec.hpp"
  
namespace etr {  
  
    inline SEXP cpp2R() {
      return R_NilValue;
    }
  
    inline SEXP cpp2R(int res) {
      return Rf_ScalarReal(etr::i2d(res));
    }
  
    inline SEXP cpp2R(bool res) {
      return Rf_ScalarReal(etr::i2d(static_cast<int>(res)));
    }
  
    inline SEXP cpp2R(double res) {
      return Rf_ScalarReal(res);
    }
    
    inline SEXP cpp2R(const VEC<double>& res) {
      SEXP ret = R_NilValue;
      
      if(res.im()) {
        ret = PROTECT(Rf_allocMatrix(REALSXP, res.nrow(), res.ncol() ) );
      } else {
        ret = PROTECT(Rf_allocVector(REALSXP, res.size() ) );
      }
      
      for(int i = 0; i < res.size(); i++) {
        REAL(ret)[i] = res[i];
      }
      
      UNPROTECT(1);
      
      return ret;
    }

    template<typename T2, typename R2>
    inline SEXP cpp2R(const VEC<T2, R2> &res) {
      SEXP ret = R_NilValue;
      
      if(res.im()) {
        ret = PROTECT(Rf_allocMatrix(REALSXP, res.nrow(), res.ncol() ) );
      } else {
        ret = PROTECT(Rf_allocVector(REALSXP, res.size() ) );
      }
      
      for(int i = 0; i < res.size(); i++) {
        REAL(ret)[i] = res[i];
      }
      
      UNPROTECT(1);
      
      return ret;
    }
    
}
  
#endif