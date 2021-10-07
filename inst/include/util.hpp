/*
R package ast2ast
Copyright (C) 2021 Konrad Krämer

This file is part of R package ast2ast


ast2ast is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with pso
If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4
*/

#ifndef UTIL
#define UTIL

#include <Rcpp.h>

#include "vec.hpp"
#include "header.hpp"
#include <iostream>
#include <vector>

// error function --> stop program & show message
void ass(bool inp, std::string message) {
  if(inp == false) {
    Rcpp::stop(message);
  }
}

// create range from start to end
std::vector<double> range(int start, int end) {
  std::vector<double> ret(end - start + 1);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}


// print empty line
void print() {
  std::cout << std::endl;
}


struct SEXP2MAT {
  std::vector<double> v;
  int ncols;
  int nrows;
};

template< typename T, typename R = std::vector<T> >
struct SUBSET {
  R sub;

  // for matrix
  int ncols;
  int nrows;

  R& data() {
    return sub.data();
  }

  int size() {
    return sub.size();
  }
};

#endif
