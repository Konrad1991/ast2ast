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


#ifndef BETA
#define BETA

#include "vec.hpp"

namespace etr {

// betaorm distribution

// rbeta
// ===============================================================================
// first argument int
VEC<double> rbeta_etr(int x , double min = 0.0, double max = 1.0) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rbeta_etr(int x, VEC<double>& min, double max = 1.0) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rbeta_etr(int x , double min, VEC<double>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(int x , double min, const VEC<T2, R2>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(int x, const VEC<T2, R2>& min, double max = 1.0) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rbeta_etr(int x, VEC<double>& min, VEC<double>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> rbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> rbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}


// rbeta
// ===============================================================================
// first argument double
VEC<double> rbeta_etr(double x , double min = 0.0, double max = 1.0) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rbeta_etr(double x, VEC<double>& min, double max = 1.0) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rbeta_etr(double x , double min, VEC<double>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(double x , double min, const VEC<T2, R2>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(double x, const VEC<T2, R2>& min, double max = 1.0) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rbeta_etr(double x, VEC<double>& min, VEC<double>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> rbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> rbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

// rbeta
// ===============================================================================
// first argument VEC<double>
VEC<double> rbeta_etr(VEC<double>& x , double min = 0.0, double max = 1.0) {
            VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rbeta_etr(VEC<double>& x, VEC<double>& min, double max = 1.0) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rbeta_etr(VEC<double>& x , double min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max = 1.0) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> rbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> rbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

// rbeta
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> rbeta_etr(const VEC<T2, R2>& x , double min = 0.0, double max = 1.0) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max);
        PutRNGstate();
      }
      return ret;
}



template<typename T2, typename R2>
VEC<double> rbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max = 1.0) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> rbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> rbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max = 1.0) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> rbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> rbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> rbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rbeta(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}















// dbeta
// ===============================================================================
// first argument int
VEC<double> dbeta_etr(int x , double min = 0.0, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min, max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(int x, VEC<double>& min, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(int x , double min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(int x , double min, const VEC<T2, R2>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(int x, const VEC<T2, R2>& min, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(int x, VEC<double>& min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> dbeta_etr(int x , double min, double max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(int x, VEC<double>& min, double max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(int x , double min, VEC<double>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(int x , double min, double max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(int x, VEC<double>& min, double max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(int x , double min, VEC<double>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(int x , double min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(int x, const VEC<T2, R2>& min, double max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> dbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// dbeta
// ===============================================================================
// first argument double
VEC<double> dbeta_etr(double x , double min = 0.0, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min, max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(double x, VEC<double>& min, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(double x , double min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(double x , double min, const VEC<T2, R2>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(double x, const VEC<T2, R2>& min, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(double x, VEC<double>& min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> dbeta_etr(double x , double min, double max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(double x, VEC<double>& min, double max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(double x , double min, VEC<double>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(double x , double min, const VEC<T2, R2>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(double x, const VEC<T2, R2>& min, double max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(double x , double min, double max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(double x, VEC<double>& min, double max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(double x , double min, VEC<double>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(double x , double min, const VEC<T2, R2>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(double x, const VEC<T2, R2>& min, double max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> dbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// dbeta
// ===============================================================================
// first argument VEC<double>
VEC<double> dbeta_etr(VEC<double>& x , double min = 0.0, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }

      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()],min, max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(VEC<double>& x, VEC<double>& min, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(VEC<double>& x , double min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }       for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()],min, max, log);
        PutRNGstate();
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }       for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()],min, max, log);
        PutRNGstate();
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

//new
VEC<double> dbeta_etr(VEC<double>& x , double min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()],min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(VEC<double>& x , double min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()],min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  >
VEC<double> dbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// dbeta
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T2, R2>& x , double min = 0.0, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> dbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T2, R2>& x , double min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> dbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(const VEC<T2, R2>& x , double min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> dbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> dbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}























// pbeta
// ===============================================================================
// first argument int
VEC<double> pbeta_etr(int x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x , double min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> pbeta_etr(int x , double min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x, VEC<double>& min, double max,bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(int x , double min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(int x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(int x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(int x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(int x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// pbeta
// ===============================================================================
// first argument double
VEC<double> pbeta_etr(double x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x , double min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> pbeta_etr(double x , double min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(double x , double min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(double x, VEC<double>& min, double max,  bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(double x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(double x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(double x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pbeta
// ===============================================================================
// first argument VEC<double>
VEC<double> pbeta_etr(VEC<double>& x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()],min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x , double min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

//new
VEC<double> pbeta_etr(VEC<double>& x , double min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()],min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x , double min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()],min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  >
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pbeta
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}









// lower as VEC<double>
// pbeta
// ===============================================================================
// first argument int
VEC<double> pbeta_etr(int x , double min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> pbeta_etr(int x , double min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x, VEC<double>& min, double max,VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(int x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(int x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(int x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// pbeta
// ===============================================================================
// first argument double
VEC<double> pbeta_etr(double x , double min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> pbeta_etr(double x , double min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(double x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(double x, VEC<double>& min, double max,  VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(double x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pbeta
// ===============================================================================
// first argument VEC<double>
VEC<double> pbeta_etr(VEC<double>& x , double min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()],min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

//new
VEC<double> pbeta_etr(VEC<double>& x , double min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  >
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pbeta
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> pbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}




// lower as const VEC<A,B>&
// pbeta
// ===============================================================================
// first argument int
template<typename A, typename B>
VEC<double> pbeta_etr(int x , double min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(int x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename A, typename B>
VEC<double> pbeta_etr(int x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(int x, VEC<double>& min, double max,const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(int x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(int x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// pbeta
// ===============================================================================
// first argument double
template<typename A, typename B>
VEC<double> pbeta_etr(double x , double min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(double x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename A, typename B>
VEC<double> pbeta_etr(double x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(double x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(double x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(double x, VEC<double>& min, double max,  const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pbeta
// ===============================================================================
// first argument VEC<double>
template<typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()],min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

//new
template<typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  , typename A, typename B>
VEC<double> pbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pbeta
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> pbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}




































// qbeta
// ===============================================================================
// first argument int
VEC<double> qbeta_etr(int x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x , double min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> qbeta_etr(int x , double min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x, VEC<double>& min, double max,bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(int x , double min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(int x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(int x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(int x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(int x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// qbeta
// ===============================================================================
// first argument double
VEC<double> qbeta_etr(double x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x , double min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> qbeta_etr(double x , double min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(double x , double min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(double x, VEC<double>& min, double max,  bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(double x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(double x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(double x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qbeta
// ===============================================================================
// first argument VEC<double>
VEC<double> qbeta_etr(VEC<double>& x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()],min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x , double min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

//new
VEC<double> qbeta_etr(VEC<double>& x , double min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()],min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x , double min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()],min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  >
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qbeta
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}









// lower as VEC<double>
// qbeta
// ===============================================================================
// first argument int
VEC<double> qbeta_etr(int x , double min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> qbeta_etr(int x , double min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x, VEC<double>& min, double max,VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(int x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(int x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(int x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// qbeta
// ===============================================================================
// first argument double
VEC<double> qbeta_etr(double x , double min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> qbeta_etr(double x , double min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(double x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(double x, VEC<double>& min, double max,  VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(double x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qbeta
// ===============================================================================
// first argument VEC<double>
VEC<double> qbeta_etr(VEC<double>& x , double min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()],min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

//new
VEC<double> qbeta_etr(VEC<double>& x , double min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  >
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qbeta
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> qbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}




// lower as const VEC<A,B>&
// qbeta
// ===============================================================================
// first argument int
template<typename A, typename B>
VEC<double> qbeta_etr(int x , double min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(int x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename A, typename B>
VEC<double> qbeta_etr(int x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(int x, VEC<double>& min, double max,const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(int x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(int x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// qbeta
// ===============================================================================
// first argument double
template<typename A, typename B>
VEC<double> qbeta_etr(double x , double min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(double x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename A, typename B>
VEC<double> qbeta_etr(double x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(double x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(double x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(double x, VEC<double>& min, double max,  const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qbeta
// ===============================================================================
// first argument VEC<double>
template<typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()],min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

//new
template<typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  , typename A, typename B>
VEC<double> qbeta_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qbeta
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> qbeta_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qbeta(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}













} // end namespace


#endif