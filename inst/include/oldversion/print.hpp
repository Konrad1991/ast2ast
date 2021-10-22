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

You should have received a copy of the GNU General Public License along with ast2ast
If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4
*/


#ifndef PRINT
#define PRINT

#include "matrix.hpp"

// print empty line
void print() {
  std::cout << std::endl;
}

void print(std::string inp) {
  Rcpp::Rcout << inp << std::endl;
}


void print(double inp){
  Rcpp::Rcout << inp << std::endl;
}


// Vector
// print fct
// ================================================================
void print(VEC<double>& inp) { // const
    for(int i = 0; i < inp.size(); i++) {
      std::cout << inp[i] << std::endl;
    }
}

void print(VEC<double>& inp, std::string&& message) {
  std::cout << message << std::endl;

  for(int i = 0; i < inp.size(); i++) {
    std::cout << inp[i] << std::endl;
  }
}
// ================================================================


// Matrix
// print fct
// ================================================================
void print(const MAT<double>& inp) { // const
    for(int i = 0; i < inp.nrows; i++) {
      for(int j = 0; j < inp.ncols; j++) {
        std::cout << inp.d[i*inp.nrows + j] << "\t";
      }
        std::cout << std::endl;
    }
}


void print(const MAT<double>& inp, std::string&& message) {

  std::cout << message << std::endl;
  for(int i = 0; i < inp.nrows; i++) {
    for(int j = 0; j < inp.ncols; j++) {
      std::cout << inp.d[i*inp.nrows + j] << "\t";
    }
      std::cout << std::endl;
  }

}
// ================================================================




// subset
// print fct
// ================================================================
void print(const SUBSET<double>&inp) {
  if(inp.matrix == false) {
    for(int i = 0; i < inp.sub.size(); i++) {
      std::cout << inp.sub[i] << std::endl;
    }
  } else {
    for(int i = 0; i < inp.nrows; i++) {
      for(int j = 0; j < inp.ncols; j++) {
        std::cout << inp.sub[i*inp.nrows + j] << "\t";
      }
        std::cout << std::endl;
    }
  }
}


void print(const SUBSET<double>&inp, std::string message) {
  std::cout << message << std::endl;
  if(inp.matrix == false) {
    for(int i = 0; i < inp.sub.size(); i++) {
      std::cout << inp.sub[i] << std::endl;
    }
  } else {
    for(int i = 0; i < inp.nrows; i++) {
        for(int j = 0; j < inp.ncols; j++) {
          std::cout << inp.sub[i*inp.nrows + j] << "\t";
      }
        std::cout << std::endl;
    }
  }
}


#endif
