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


#ifndef NORMAL
#define NORMAL

#include "vec.hpp"

namespace etr {

// normorm distribution

// rnorm
// ===============================================================================
// first argument int
VEC<double> rnorm_etr(int x , double min = 0.0, double max = 1.0) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rnorm_etr(int x, VEC<double>& min, double max = 1.0) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rnorm_etr(int x , double min, VEC<double>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(int x , double min, const VEC<T2, R2>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(int x, const VEC<T2, R2>& min, double max = 1.0) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rnorm_etr(int x, VEC<double>& min, VEC<double>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> rnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> rnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max) {
      VEC<double> ret(x);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}


// rnorm
// ===============================================================================
// first argument double
VEC<double> rnorm_etr(double x , double min = 0.0, double max = 1.0) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rnorm_etr(double x, VEC<double>& min, double max = 1.0) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rnorm_etr(double x , double min, VEC<double>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(double x , double min, const VEC<T2, R2>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(double x, const VEC<T2, R2>& min, double max = 1.0) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rnorm_etr(double x, VEC<double>& min, VEC<double>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> rnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> rnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max) {
      VEC<double> ret(d2i(x));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

// rnorm
// ===============================================================================
// first argument VEC<double>
VEC<double> rnorm_etr(VEC<double>& x , double min = 0.0, double max = 1.0) {
            VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rnorm_etr(VEC<double>& x, VEC<double>& min, double max = 1.0) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rnorm_etr(VEC<double>& x , double min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max = 1.0) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

VEC<double> rnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> rnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> rnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

// rnorm
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> rnorm_etr(const VEC<T2, R2>& x , double min = 0.0, double max = 1.0) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max);
        PutRNGstate();
      }
      return ret;
}



template<typename T2, typename R2>
VEC<double> rnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max = 1.0) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> rnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min, max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> rnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max = 1.0) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> rnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> rnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> rnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> rnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = vector(at(x, 1));
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::rnorm(min[i % min.size()], max[i % max.size()]);
        PutRNGstate();
      }
      return ret;
}















// dnorm
// ===============================================================================
// first argument int
VEC<double> dnorm_etr(int x , double min = 0.0, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min, max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(int x, VEC<double>& min, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(int x , double min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(int x , double min, const VEC<T2, R2>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(int x, const VEC<T2, R2>& min, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(int x, VEC<double>& min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> dnorm_etr(int x , double min, double max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(int x, VEC<double>& min, double max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(int x , double min, VEC<double>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(int x , double min, double max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(int x, VEC<double>& min, double max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(int x , double min, VEC<double>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(int x , double min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(int x, const VEC<T2, R2>& min, double max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> dnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(i2d(x), min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// dnorm
// ===============================================================================
// first argument double
VEC<double> dnorm_etr(double x , double min = 0.0, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min, max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(double x, VEC<double>& min, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(double x , double min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(double x , double min, const VEC<T2, R2>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(double x, const VEC<T2, R2>& min, double max = 1.0, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(double x, VEC<double>& min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> dnorm_etr(double x , double min, double max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(double x, VEC<double>& min, double max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(double x , double min, VEC<double>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(double x , double min, const VEC<T2, R2>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(double x, const VEC<T2, R2>& min, double max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(double x , double min, double max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(double x, VEC<double>& min, double max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(double x , double min, VEC<double>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(double x , double min, const VEC<T2, R2>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(double x, const VEC<T2, R2>& min, double max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> dnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<U, Z> log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x, min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// dnorm
// ===============================================================================
// first argument VEC<double>
VEC<double> dnorm_etr(VEC<double>& x , double min = 0.0, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }

      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()],min, max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(VEC<double>& x, VEC<double>& min, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(VEC<double>& x , double min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }       for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()],min, max, log);
        PutRNGstate();
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }       for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()],min, max, log);
        PutRNGstate();
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

//new
VEC<double> dnorm_etr(VEC<double>& x , double min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()],min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> dnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> dnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> dnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(VEC<double>& x , double min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()],min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> dnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  >
VEC<double> dnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// dnorm
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T2, R2>& x , double min = 0.0, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max = 1.0, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> dnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T2, R2>& x , double min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> dnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> dnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double> log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(const VEC<T2, R2>& x , double min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min, max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> dnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> dnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::dnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}























// pnorm
// ===============================================================================
// first argument int
VEC<double> pnorm_etr(int x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x , double min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> pnorm_etr(int x , double min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x, VEC<double>& min, double max,bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(int x , double min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(int x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(int x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(int x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(int x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// pnorm
// ===============================================================================
// first argument double
VEC<double> pnorm_etr(double x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x , double min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> pnorm_etr(double x , double min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(double x , double min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(double x, VEC<double>& min, double max,  bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(double x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(double x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(double x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pnorm
// ===============================================================================
// first argument VEC<double>
VEC<double> pnorm_etr(VEC<double>& x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()],min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x , double min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

//new
VEC<double> pnorm_etr(VEC<double>& x , double min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()],min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x , double min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()],min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  >
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pnorm
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}









// lower as VEC<double>
// pnorm
// ===============================================================================
// first argument int
VEC<double> pnorm_etr(int x , double min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> pnorm_etr(int x , double min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x, VEC<double>& min, double max,VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(int x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(int x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(int x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// pnorm
// ===============================================================================
// first argument double
VEC<double> pnorm_etr(double x , double min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> pnorm_etr(double x , double min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(double x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(double x, VEC<double>& min, double max,  VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(double x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pnorm
// ===============================================================================
// first argument VEC<double>
VEC<double> pnorm_etr(VEC<double>& x , double min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()],min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

//new
VEC<double> pnorm_etr(VEC<double>& x , double min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  >
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pnorm
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> pnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> pnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}




// lower as const VEC<A,B>&
// pnorm
// ===============================================================================
// first argument int
template<typename A, typename B>
VEC<double> pnorm_etr(int x , double min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(int x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename A, typename B>
VEC<double> pnorm_etr(int x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(int x, VEC<double>& min, double max,const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(int x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(int x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// pnorm
// ===============================================================================
// first argument double
template<typename A, typename B>
VEC<double> pnorm_etr(double x , double min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(double x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename A, typename B>
VEC<double> pnorm_etr(double x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(double x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(double x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(double x, VEC<double>& min, double max,  const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pnorm
// ===============================================================================
// first argument VEC<double>
template<typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()],min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

//new
template<typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  , typename A, typename B>
VEC<double> pnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// pnorm
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> pnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::pnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}




































// qnorm
// ===============================================================================
// first argument int
VEC<double> qnorm_etr(int x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x , double min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> qnorm_etr(int x , double min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x, VEC<double>& min, double max,bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(int x , double min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(int x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(int x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(int x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(int x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// qnorm
// ===============================================================================
// first argument double
VEC<double> qnorm_etr(double x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x , double min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> qnorm_etr(double x , double min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(double x , double min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(double x, VEC<double>& min, double max,  bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(double x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(double x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(double x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qnorm
// ===============================================================================
// first argument VEC<double>
VEC<double> qnorm_etr(VEC<double>& x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()],min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x , double min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

//new
VEC<double> qnorm_etr(VEC<double>& x , double min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()],min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x , double min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()],min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  >
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qnorm
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min = 0.0, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max = 1.0, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower = true, bool log = false) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, bool lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower, log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}









// lower as VEC<double>
// qnorm
// ===============================================================================
// first argument int
VEC<double> qnorm_etr(int x , double min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> qnorm_etr(int x , double min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x, VEC<double>& min, double max,VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(int x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(int x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(int x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(int x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(int x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// qnorm
// ===============================================================================
// first argument double
VEC<double> qnorm_etr(double x , double min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
VEC<double> qnorm_etr(double x , double min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(double x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(double x, VEC<double>& min, double max,  VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(double x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(double x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(double x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qnorm
// ===============================================================================
// first argument VEC<double>
VEC<double> qnorm_etr(VEC<double>& x , double min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()],min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

//new
VEC<double> qnorm_etr(VEC<double>& x , double min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  >
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qnorm
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3>
VEC<double> qnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, VEC<double>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z>
VEC<double> qnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, VEC<double>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}




// lower as const VEC<A,B>&
// qnorm
// ===============================================================================
// first argument int
template<typename A, typename B>
VEC<double> qnorm_etr(int x , double min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(int x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename A, typename B>
VEC<double> qnorm_etr(int x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(int x, VEC<double>& min, double max,const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(int x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(int x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(int x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(int x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(int x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(int x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(int x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(i2d(x), min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}



// qnorm
// ===============================================================================
// first argument double
template<typename A, typename B>
VEC<double> qnorm_etr(double x , double min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(double x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename A, typename B>
VEC<double> qnorm_etr(double x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(double x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(double x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(double x, VEC<double>& min, double max,  const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(double x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(double x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(double x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(double x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(double x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
      VEC<double> ret; ret = x;
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x, min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qnorm
// ===============================================================================
// first argument VEC<double>
template<typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()],min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

//new
template<typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()],min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename T3, typename R3, typename U, typename Z  , typename A, typename B>
VEC<double> qnorm_etr(VEC<double>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}


// qnorm
// ===============================================================================
// first argument const VEC<T2, R2>&

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, bool log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log);
        PutRNGstate();
      }
      return ret;
}

// new
template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, VEC<double>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z  , typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, VEC<double>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x , double min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x , double min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min, max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, double max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max,lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T2, R2>& x, VEC<double>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
} 

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, const VEC<T2, R2>& min, VEC<double>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T3, typename R3, typename T2, typename R2, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T3, R3>& x, VEC<double>& min, const VEC<T2, R2>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                   VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}

template<typename T4, typename R4, typename T2, typename R2, typename T3, typename R3, typename U, typename Z, typename A, typename B>
VEC<double> qnorm_etr(const VEC<T4, R4>& x, const VEC<T2, R2>& min, const VEC<T3, R3>& max, const VEC<A, B>& lower, const VEC<U, Z>& log) {
                  VEC<double> ret;
      if(length(x) > 1) {
            ret = vector(length(x));
      } else if(length(x) == 1){
            ret = x;
      }
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::qnorm(x[i % x.size()], min[i % min.size()], max[i % max.size()],lower[i % lower.size()], log[i % log.size()]);
        PutRNGstate();
      }
      return ret;
}













} // end namespace


#endif