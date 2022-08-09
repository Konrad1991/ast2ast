
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


#ifndef COLON_H
#define COLON_H

#include "vec.hpp"

namespace etr {

VEC<double> colon(int start, int end) {
  VEC<double> ret(end - start + 1);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}

VEC<double> colon(double start, double end) {

  int size = static_cast<int>(floor(end - start +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}

VEC<double> colon(int start, double end) {

  int size = static_cast<int>(floor(end - static_cast<double>(start) +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}

VEC<double> colon(double start, int end) {

  int size = static_cast<int>(floor(static_cast<double>(end) - start +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}


VEC<double> colon(int start, VEC<double> end) {

  int size = static_cast<int>(floor(end[0] - static_cast<double>(start) +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}

VEC<double> colon(double start, VEC<double> end) {

  int size = static_cast<int>(floor(end[0] - (start) +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}

VEC<double> colon(VEC<double> start, int end) {

  int size = static_cast<int>(floor(static_cast<double>(end) - (start[0]) +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start[0] + static_cast<double>(i);
  }
  return ret;
}

VEC<double> colon(VEC<double> start, double end) {

  int size = static_cast<int>(floor(end - (start[0]) +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start[0] + static_cast<double>(i);
  }
  return ret;
}


VEC<double> colon(VEC<double> start, VEC<double> end) {

  int size = static_cast<int>(floor(end[0] - start[0] +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start[0] + static_cast<double>(i);
  }
  return ret;
}


template<typename T2, typename R2>
VEC<double> colon(int start, const VEC<T2, R2>& end) {

  int size = static_cast<int>(floor(end[0] - static_cast<double>(start) +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}

template<typename T2, typename R2>
VEC<double> colon(double start, const VEC<T2, R2>& end) {

  int size = static_cast<int>(floor(end[0] - (start) +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}

template<typename T2, typename R2>
VEC<double> colon(const VEC<T2, R2>& start, int end) {

  int size = static_cast<int>(floor(static_cast<double>(end) - (start[0]) +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start[0] + static_cast<double>(i);
  }
  return ret;
}

template<typename T2, typename R2>
VEC<double> colon(const VEC<T2, R2>& start, double end) {

  int size = static_cast<int>(floor(end - (start[0]) +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start[0] + static_cast<double>(i);
  }
  return ret;
}

template<typename T2, typename R2, typename T3, typename R3>
VEC<double> colon(const VEC<T2, R2>& start, const VEC<T3, R3>& end) {

  int size = static_cast<int>(floor(end[0] - start[0] +1));
  VEC<double> ret(size);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start[0] + static_cast<double>(i);
  }
  return ret;
}


int length(double inp) {
  return 1;
}

int length(int inp) {
  return 1;
}

int length(bool inp) {
  return 1;
}

template<typename T>
int length(VEC<T>& inp) {
  return inp.size();
}

template<typename T2, typename R2> 
int length(const VEC<T2, R2>& inp) {
  return inp.size();
}

template<typename T>
VEC<double> dim(const VEC<T>& inp) {
  if(inp.im() == false) {
    std::cerr << "dim can only be called with matrix" << std::endl;
    #ifdef RLANG
      Rcpp::stop("Error");
    #else
      exit (EXIT_FAILURE);
    #endif
  }

  VEC<double> ret(2);

  ret[0] = inp.nr();
  ret[1] = inp.nc();

  return ret;
}


void dim(bool inp) {
    std::cerr << "dim can only be called with matrix" << std::endl;
    #ifdef RLANG
      Rcpp::stop("Error");
    #else
      exit (EXIT_FAILURE);
    #endif
}

void dim(int inp) {
    std::cerr << "dim can only be called with matrix" << std::endl;
    #ifdef RLANG
      Rcpp::stop("Error");
    #else
      exit (EXIT_FAILURE);
    #endif
}

void dim(double inp) {
    std::cerr << "dim can only be called with matrix" << std::endl;
    #ifdef RLANG
      Rcpp::stop("Error");
    #else
      exit (EXIT_FAILURE);
    #endif  
}

}

#endif
