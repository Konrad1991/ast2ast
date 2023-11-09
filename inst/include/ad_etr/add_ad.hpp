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
      : BaseBinary<T, L, R>(a, b, ismatrix_, rows, cols) {
        print_type(a);
      }
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


template<typename L, typename R, typename CTrait = BinaryTrait, typename Trait = PlusTrait>
class BinaryPlus : public BaseCalc {
public:
  using CaseTrait = CTrait;
  using TypeTrait = Trait;
  const L &l;
  const R &r;
  BinaryPlus(const L &a, const R &b, bool ismatrix_, int rows, int cols)
      : l(a), r(b) {
        std::cout << "type input into BinaryPlus" << std::endl;
        print_type(a);
        print_type(l);
        std::cout << std::endl;
        ismatrix = ismatrix_;
        rows_ = rows;
        columns_ = cols;
  }
  const L &getL() const { return l; }
  const R &getR() const { return r; }
  size_t size() const {
    constexpr bool l_arithmetic = IsBoolIntOrDouble<L>();
    constexpr bool r_arithmetic = IsBoolIntOrDouble<R>();
    if constexpr(!l_arithmetic && !r_arithmetic) {
      return (this -> l.size() > this -> r.size()) ? this -> l.size() : this -> r.size();  
    } else if constexpr(l_arithmetic && !r_arithmetic) {
      return this -> r.size();
    } else if constexpr(!l_arithmetic && r_arithmetic) {
      return this -> l.size();
    } else if constexpr(l_arithmetic && r_arithmetic) {
      return 1;
    }
  }
  double operator[](const int i) const { 
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
  
};


template <typename L, typename R>
inline VEC<double, BinaryPlus<L, R>> operator+(const L &a,
                                         const R &b) {
  constexpr bool l_arithmetic = IsBoolIntOrDouble<L>();
  constexpr bool r_arithmetic = IsBoolIntOrDouble<R>();
  bool ismatrix_ = false;
  size_t nrows_ = 0;
  size_t ncols_ = 0;
  if constexpr (!l_arithmetic && !r_arithmetic) {
    defineMatrix<1>(a.im(), b.im(),
                    a.nr(), b.nr(),
                    a.nc(), b.nc(),
                    ismatrix_, nrows_, ncols_);
    //std::cout << "construction of BinaryPlus<L, R> alone" << std::endl;
    //BinaryPlus<STORE<double, VariableTrait>, STORE<double, VariableTrait>> test(a.data(), b.data(), ismatrix_, nrows_, ncols_);
    //std::cout << "End construction of BinaryPlus<L, R> alone" << std::endl;
    //std::cout << std::endl;
    //std::cout << std::endl;
    //std::cout << std::endl;

    // I need constructors for BinaryClass and UnaryClass in VEC
    // Otherwise the wrong constructor is called. Super strange behaviour

    VEC<double, BinaryPlus<L, R>> ret(
      BinaryPlus<STORE<double, VariableTrait>, STORE<double, VariableTrait>>(a.data(), b.data(), ismatrix_, nrows_, ncols_));
    ret.set_matrix(ismatrix_);
    ret.set_ncol(ncols_);
    ret.set_nrow(nrows_);
    return ret;
  } else if constexpr(l_arithmetic && !r_arithmetic) {
    defineMatrix<3>(true, b.im(),
                      0, b.nr(),
                      0, b.nc(),
                      ismatrix_, nrows_, ncols_);
      VEC<double, BinaryPlus<L, R>> ret(
        BinaryPlus<L, R>(a, b.data(), ismatrix_, nrows_, ncols_));
      ret.set_matrix(ismatrix_);
      ret.set_ncol(ncols_);
      ret.set_nrow(nrows_);
      return ret;
  } else if constexpr(!l_arithmetic && r_arithmetic) {
    defineMatrix<2>(a.im(), true,
                  a.nr(), 0,
                  a.nc(), 0,
                  ismatrix_, nrows_, ncols_);
    VEC<double, BinaryPlus<L, R>> ret(
      BinaryPlus<L, R>(a.data(), b, ismatrix_, nrows_, ncols_));
    ret.set_matrix(ismatrix_);
    ret.set_ncol(ncols_);
    ret.set_nrow(nrows_);
    return ret;
  }
}

/*
template <typename T, typename L, typename R>
inline VEC<T, PLUS<T, L, R, PlusTrait>> operator+(const VEC<T, L> &a,
                                         const VEC<T, R> &b) {

  bool ismatrix_ = false;
  size_t nrows_ = 0;
  size_t ncols_ = 0;
  constexpr bool l_arithmetic = IsBoolIntOrDouble<L>();
  constexpr bool r_arithmetic = IsBoolIntOrDouble<R>();
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
}

template <typename T, typename L, typename R>
requires std::is_same_v<R, double>
inline VEC<T, PLUS<T, L, R, PlusTrait>> operator+(const VEC<T, L> &a,
                                         const R &b) {
  bool ismatrix_ = false;
  size_t nrows_ = 0;
  size_t ncols_ = 0;
  defineMatrix<2>(a.im(), true,
                  a.nr(), 0,
                  a.nc(), 0,
                  ismatrix_, nrows_, ncols_);
  VEC<T, PLUS<T, L, R, PlusTrait>> ret(
    PLUS<T, L, R, PlusTrait>(a.data(), b, ismatrix_, nrows_, ncols_));
  ret.set_matrix(ismatrix_);
  ret.set_ncol(ncols_);
  ret.set_nrow(nrows_);
  return ret;
}

template <typename T, typename L, typename R>
requires std::is_same_v<L, double>
inline VEC<T, PLUS<T, L, R, PlusTrait>> operator+(const L& a,
                                         const VEC<T, R> &b) {
  bool ismatrix_ = false;
  size_t nrows_ = 0;
  size_t ncols_ = 0;
  defineMatrix<3>(true, b.im(),
                      0, b.nr(),
                      0, b.nc(),
                      ismatrix_, nrows_, ncols_);
      VEC<T, PLUS<T, L, R, PlusTrait>> ret(
        PLUS<T, L, R, PlusTrait>(a, b.data(), ismatrix_, nrows_, ncols_));
      ret.set_matrix(ismatrix_);
      ret.set_ncol(ncols_);
      ret.set_nrow(nrows_);
      return ret;
}
*/
} // namespace etr

#endif