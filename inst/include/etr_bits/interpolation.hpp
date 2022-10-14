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


#ifndef INTERPOL
#define INTERPOL

#include "vec.hpp"

namespace etr {

template<typename T>
inline double li(double t, VEC<T> &time_vec, VEC<T> &par_vec) {
  double t0, t1;
  double y0, y1, delta_par, delta_t, m;
  double ret;


  // not in boundaries
  if(t >= time_vec[time_vec.size()]) {
    ret = par_vec[par_vec.size()];
    return ret;
  } else if(t <= time_vec[0]) {
    ret = par_vec[0];
    return ret;
  }

  // in boundaries
  for(size_t i = 0; i < time_vec.size(); i++) {
      if(t>=time_vec[i] && t<time_vec[i+1]) {
        t0 = time_vec[i];
        t1 = time_vec[i + 1];
        y0 = par_vec[i];
        y1 = par_vec[i + 1];
        delta_par = y1 - y0;
        delta_t = t1 - t0;
        ret = m*(t - t0) + y0;
      }
  }

  return(ret);
}

template<typename T>
inline double cmr(double t, VEC<T> &time_vec, VEC<T> &par_vec){
  int idx0, idx1, idx2, idx3;
  double t0, t1, t2, t3;
  double y0, y1, y2, y3;

  idx0 = 0;
  idx1 = 0;
  idx2 = 0;
  idx3 = 0;
  t0 = t1 = t2 = t3 = 0.;
  y0 = y1 = y2 = y3 = 0.;

  ass(time_vec.size() == par_vec.size(), "x and y differ in length");

  if(t < time_vec[0]) {
    return par_vec[0];
  } else if(t > time_vec[time_vec.size() - 1]) {
    return par_vec[par_vec.size() - 1];
  }


  for(size_t i = 0; i <= time_vec.size(); i++)  {

    if (i == (time_vec.size()-1)) {

      idx0 = time_vec.size() - 2;
      t0 = time_vec[idx0];
      y0 = par_vec[idx0];

      idx1 = time_vec.size() - 1;
      t1 = time_vec[idx1];
      y1 = par_vec[idx1];

      idx2 = time_vec.size()  - time_vec.size();
      t2 = time_vec[idx2];
      y2 = par_vec[idx2];

      idx3 = time_vec.size() + 1 - time_vec.size();
      t3 = time_vec[idx3];
      y3 = par_vec[idx3];
      break;

    } else if (t>=time_vec[i] && t<time_vec[i+1]){

      if (i==0) {
        idx0 = time_vec.size() -1;
        t0 = time_vec[idx0];
      } else {
        idx0 = i-1;
        t0 = time_vec[idx0];
      }

      y0 = par_vec[idx0];
      idx1 = i;
      t1 = time_vec[idx1];
      y1 = par_vec[idx1];


      if ( i == time_vec.size()-1 ) {
        idx2 = 0;
        t2 = time_vec[idx2]+time_vec.back();
      } else {
        idx2 = i+1;
        t2 = time_vec[idx2];
      }
      y2 = par_vec[idx2];


      if ( i == time_vec.size()-2 ) {
        idx3 = 0;
        t3 = time_vec[idx3] + time_vec.back();
      } else if ( i == time_vec.size()-1 ) {
        idx3 = 1;
        t3 = time_vec[idx3]+time_vec.back();
      } else {
        idx3 = i+2;
        t3 = time_vec[idx3];
      }
      y3 = par_vec[idx3];
      break;

    }

  } // search for the beginning of the interpolation intervall

  double x = (t -t1) / (t2 -t1);
  double m1 = (y2 -y0) / (t2 -t0);
  double m2 = (y3 -y1) / (t3 -t1);

  double res = (
    (2.*pow(x,3) -3.*pow(x,2) +1.) *y1
    + (pow(x,3) -2.*pow(x,2) +x) *(t2-t1) *m1
    + (-2.*pow(x,3) +3.*pow(x,2)) *y2
    + (pow(x,3) -pow(x,2)) *(t2-t1) *m2
  );
  return res;
}

inline double cmr(double t, const VEC<double>& time_vec, const VEC<double>& par_vec){
  int idx0, idx1, idx2, idx3;
  double t0, t1, t2, t3;
  double y0, y1, y2, y3;

  idx0 = 0;
  idx1 = 0;
  idx2 = 0;
  idx3 = 0;
  t0 = t1 = t2 = t3 = 0.;
  y0 = y1 = y2 = y3 = 0.;

  ass(time_vec.size() == par_vec.size(), "x and y differ in length");

  if(t < time_vec[0]) {
    return par_vec[0];
  } else if(t > time_vec[time_vec.size() - 1]) {
    return par_vec[par_vec.size() - 1];
  }

  for(size_t i = 0; i <= time_vec.size(); i++)  {

    if (i == (time_vec.size()-1)) {
      idx0 = time_vec.size() - 2;
      t0 = time_vec[idx0];
      y0 = par_vec[idx0];

      idx1 = time_vec.size() - 1;
      t1 = time_vec[idx1];
      y1 = par_vec[idx1];

      idx2 = time_vec.size()  - time_vec.size();
      t2 = time_vec[idx2];
      y2 = par_vec[idx2];

      idx3 = time_vec.size() + 1 - time_vec.size();
      t3 = time_vec[idx3];
      y3 = par_vec[idx3];
      break;
    } else if (t>=time_vec[i] && t<time_vec[i+1]){

      if (i==0) {
        idx0 = time_vec.size() -1;
        t0 = time_vec[idx0];
      } else {
        idx0 = i-1;
        t0 = time_vec[idx0];
      }

      y0 = par_vec[idx0];
      idx1 = i;
      t1 = time_vec[idx1];
      y1 = par_vec[idx1];

      if ( i == time_vec.size()-1 ) {
        idx2 = 0;
        t2 = time_vec[idx2]+time_vec.back();
      } else {
        idx2 = i+1;
        t2 = time_vec[idx2];
      }
      y2 = par_vec[idx2];

      if ( i == time_vec.size()-2 ) {
        idx3 = 0;
        t3 = time_vec[idx3] + time_vec.back();
      } else if ( i == time_vec.size()-1 ) {
        idx3 = 1;
        t3 = time_vec[idx3]+time_vec.back();
      } else {
        idx3 = i+2;
        t3 = time_vec[idx3];
      }
      y3 = par_vec[idx3];
      break;
    }

  } // search for the beginning of the interpolation intervall

  double x = (t -t1) / (t2 -t1);
  double m1 = (y2 -y0) / (t2 -t0);
  double m2 = (y3 -y1) / (t3 -t1);

  double res = (
    (2.*pow(x,3) -3.*pow(x,2) +1.) *y1
    + (pow(x,3) -2.*pow(x,2) +x) *(t2-t1) *m1
    + (-2.*pow(x,3) +3.*pow(x,2)) *y2
    + (pow(x,3) -pow(x,2)) *(t2-t1) *m2
  );
  return res;
}

}

#endif
