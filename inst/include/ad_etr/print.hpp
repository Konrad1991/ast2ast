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

#ifndef PRINT
#define PRINT

#include "subset_ad.hpp"

namespace etr {

// print empty line
inline void print() {
  Rcpp::Rcout << std::endl;
}

inline void print(std::string inp) {
  Rcpp::Rcout << inp << std::endl;
}

inline void print(const char *inp) {
  Rcpp::Rcout << inp << std::endl;
}

inline void print(double inp) {
  Rcpp::Rcout << inp << std::endl;
}

inline void print(int inp) {
  Rcpp::Rcout << inp << std::endl;
}

inline void print(bool inp) {
  Rcpp::Rcout << inp << std::endl;
}

// Vector & matrix
// print fct
// ================================================================
inline void print(VEC<double> &inp) { // const

  if (inp.im() == false) {
      for (int i = 0; i < inp.size(); i++) {
        Rcpp::Rcout << inp[i] << std::endl;
      }
  } else if (inp.im() == true) {
      for (int i = 0; i < inp.nr(); i++) {
        for (int j = 0; j < inp.nc(); j++) {
          Rcpp::Rcout << inp.d[j * inp.nr() + i] << "\t";
        }
        Rcpp::Rcout << std::endl;
      }
  }
}

inline void print(VEC<double> &&inp) { // const
  if (inp.im() == false) {
      for (int i = 0; i < inp.size(); i++) {
        Rcpp::Rcout << inp[i] << std::endl;
      }
  } else if (inp.im() == true) {
      for (int i = 0; i < inp.nr(); i++) {
        for (int j = 0; j < inp.nc(); j++) {
          Rcpp::Rcout << inp.d[j * inp.nr() + i] << "\t";
        }
        Rcpp::Rcout << std::endl;
      }
  }
}

inline void print(VEC<bool> &inp) { // const

  if (inp.im() == false) {
    for (int i = 0; i < inp.size(); i++) {
      Rcpp::Rcout << inp[i] << std::endl;
    }
  } else {
    std::cerr << "Error: found VEC<bool> is matrix" << std::endl;
    Rcpp::stop("Error");
  }
}

template <typename T2, typename R2> inline void print(const VEC<T2, R2> &inp) {

  if (inp.im() == false) {
      for (int i = 0; i < inp.size(); i++) {
        Rcpp::Rcout << inp[i] << std::endl;
      }
  } else if (inp.im() == true) {
      for (int i = 0; i < inp.nr(); i++) {
        for (int j = 0; j < inp.nc(); j++) {
          Rcpp::Rcout << inp.d[j * inp.nr() + i] << "\t";
        }
        Rcpp::Rcout << std::endl;
      }
  }
}

} // namespace etr

#endif
