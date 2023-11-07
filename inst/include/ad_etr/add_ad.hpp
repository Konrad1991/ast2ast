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

#ifndef ADD
#define ADD

#include "vec.hpp"

namespace etr {

template <typename T, typename L, typename R, typename Trait = PlusTrait> 
class PLUS : public BaseBinary<T, L, R> {
public:
  using TypeTrait = Trait;
  PLUS(const L &a, const R &b, bool ismatrix_, int rows, int cols)
      : BaseBinary<T, L, R>(a, b, ismatrix_, rows, cols) {}
  T operator[](const int i) const { 
    constexpr bool l_arithmetic = IsBoolIntOrDouble<L>();
    constexpr bool r_arithmetic = IsBoolIntOrDouble<R>();
    if constexpr(!l_arithmetic && !r_arithmetic) {
      return this -> l[i % this -> l.size()] + this -> r[i % this -> r.size()];   
    } else if constexpr(!l_arithmetic && r_arithmetic) {
      return this -> l[i % this -> l.size()] + this -> r;
    } else if constexpr(l_arithmetic && !r_arithmetic) {
      return this -> l + this -> r[i % this -> r.size()]; 
    } else {
      Rcpp::stop("Error");
    }
  } 
  T get_deriv_left(const int i) const { return 1.0; }
  T get_deriv_right(const int i) const { return 1.0; }
};

template <typename T, typename L, typename R>
inline VEC<T, PLUS<T, L, R, PlusTrait>> operator+(const VEC<T, L> &a,
                                         const VEC<T, R> &b) {

  bool ismatrix_ = false;
  size_t nrows_ = 0;
  size_t ncols_ = 0;
  constexpr bool l_arithmetic = IsBoolIntOrDouble<L>();
  constexpr bool r_arithmetic = IsBoolIntOrDouble<R>();
  if constexpr(!l_arithmetic && !r_arithmetic) {
    defineMatrix<1>(a.im(), b.im(),
                    a.nr(), b.nr(),
                    a.nc(), b.nc(),
                    ismatrix_, nrows_, ncols_);
    VEC<T, PLUS<T, L, R, PlusTrait>> ret(
      PLUS<T, L, R, PlusTrait>(a.data(), b.data(), ismatrix_, nrows_, ncols_));
    ret.set_matrix(ismatrix_);
    ret.set_ncol(ncols_);
    ret.set_nrow(nrows_);
    return ret;
  } else if constexpr(!l_arithmetic && r_arithmetic) {
    defineMatrix<2>(a.im(), b.im(),
                    a.nr(), b.nr(),
                    a.nc(), b.nc(),
                    ismatrix_, nrows_, ncols_);
    VEC<T, PLUS<T, L, R, PlusTrait>> ret(
      PLUS<T, L, R, PlusTrait>(a.data(), b.data(), ismatrix_, nrows_, ncols_));
    ret.set_matrix(ismatrix_);
    ret.set_ncol(ncols_);
    ret.set_nrow(nrows_);
    return ret;
  } else if constexpr(l_arithmetic && !r_arithmetic) {
    defineMatrix<3>(a.im(), b.im(),
                    a.nr(), b.nr(),
                    a.nc(), b.nc(),
                    ismatrix_, nrows_, ncols_);
    VEC<T, PLUS<T, L, R, PlusTrait>> ret(
      PLUS<T, L, R, PlusTrait>(a.data(), b.data(), ismatrix_, nrows_, ncols_));
    ret.set_matrix(ismatrix_);
    ret.set_ncol(ncols_);
    ret.set_nrow(nrows_);
    return ret;
  } else {
    Rcpp::stop("Error");
  }
}

} // namespace etr

#endif