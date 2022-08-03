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


#ifndef DISTRI
#define DISTRI

#include "vec.hpp"

namespace etr {

VEC<double> norm_rand_etr(int length) {
      VEC<double> ret(length);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::norm_rand();
        PutRNGstate();
      }
      return ret;
}

VEC<double> norm_rand_etr(double length) {
      VEC<double> ret(static_cast<int>(length));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::norm_rand();
        PutRNGstate();
      }
      return ret;
}


 VEC<double> norm_rand_etr(VEC<double> inp) {

       ass(length(inp) == 1, "Input to norm_rand has length != 1");
       VEC<double> ret(static_cast<int>(inp[0]));
       for(int i = 0; i < ret.size(); i++) {
         GetRNGstate();
         ret[i] = R::norm_rand();
         PutRNGstate();
       }
       return ret;
 }


 template<typename T2, typename R2>
 VEC<double> norm_rand_etr(const VEC<T2, R2>& inp) {

       ass(length(inp) == 1, "Input to norm_rand has length != 1");
       VEC<double> ret(static_cast<int>(inp[0]));

       for(int i = 0; i < ret.size(); i++) {
         GetRNGstate();
         ret[i] = R::norm_rand();
         PutRNGstate();
       }
       return ret;
 }







VEC<double> unif_rand_etr(int length) {
      VEC<double> ret(length);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::unif_rand();
        PutRNGstate();
      }
      return ret;
}

VEC<double> unif_rand_etr(double length) {
      VEC<double> ret(static_cast<int>(length));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::unif_rand();
        PutRNGstate();
      }
      return ret;
}


 VEC<double> unif_rand_etr(VEC<double> inp) {

       ass(length(inp) == 1, "Input to norm_rand has length != 1");
       VEC<double> ret(static_cast<int>(inp[0]));
       for(int i = 0; i < ret.size(); i++) {
         GetRNGstate();
         ret[i] = R::unif_rand();
         PutRNGstate();
       }
       return ret;
 }


 template<typename T2, typename R2>
 VEC<double> unif_rand_etr(const VEC<T2, R2>& inp) {

       ass(length(inp) == 1, "Input to norm_rand has length != 1");
       VEC<double> ret(static_cast<int>(inp[0]));

       for(int i = 0; i < ret.size(); i++) {
         GetRNGstate();
         ret[i] = R::unif_rand();
         PutRNGstate();
       }
       return ret;
 }









VEC<double> exp_rand_etr(int length) {
      VEC<double> ret(length);
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::exp_rand();
        PutRNGstate();
      }
      return ret;
}

VEC<double> exp_rand_etr(double length) {
      VEC<double> ret(static_cast<int>(length));
      for(int i = 0; i < ret.size(); i++) {
        GetRNGstate();
        ret[i] = R::exp_rand();
        PutRNGstate();
      }
      return ret;
}


 VEC<double> exp_rand_etr(VEC<double> inp) {

       ass(length(inp) == 1, "Input to norm_rand has length != 1");
       VEC<double> ret(static_cast<int>(inp[0]));
       for(int i = 0; i < ret.size(); i++) {
         GetRNGstate();
         ret[i] = R::exp_rand();
         PutRNGstate();
       }
       return ret;
 }


 template<typename T2, typename R2>
 VEC<double> exp_rand_etr(const VEC<T2, R2>& inp) {

       ass(length(inp) == 1, "Input to norm_rand has length != 1");
       VEC<double> ret(static_cast<int>(inp[0]));

       for(int i = 0; i < ret.size(); i++) {
         GetRNGstate();
         ret[i] = R::exp_rand();
         PutRNGstate();
       }
       return ret;
 }


} // end namespace


#endif