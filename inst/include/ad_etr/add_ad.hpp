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

/*
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
*/

template<typename L2, typename R2, binaryFct f, typename CTrait, typename Trait>
class BinaryOperation : public BaseCalc {
public:
  using CaseTrait = CTrait;
  using TypeTrait = Trait;
  const L2 &l;
  const R2 &r;
  BinaryOperation(const L2 &a, const R2 &b, bool ismatrix_, int rows, int cols)
      : l(a), r(b) {
        ismatrix = ismatrix_;
        rows_ = rows;
        columns_ = cols;
  }
  BinaryOperation(const BinaryOperation& other)
      : l(other.l), r(other.r) {
        this -> ismatrix = other.ismatrix;
        this -> rows_ = other.rows_;
        this -> columns_ = other.columns_;
  }

  const L2 &getL() const { return l; }
  const R2 &getR() const { return r; }
  size_t size() const {
    constexpr bool l_arithmetic = IsBoolIntOrDouble<L2>();
    constexpr bool r_arithmetic = IsBoolIntOrDouble<R2>();
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
    constexpr bool l_arithmetic = IsBoolIntOrDouble<L2>();
    constexpr bool r_arithmetic = IsBoolIntOrDouble<R2>();
    if constexpr(!l_arithmetic && !r_arithmetic) {
      return f(this -> l[i % this -> l.size()], this -> r[i % this -> r.size()]);   
    } else if constexpr(!l_arithmetic && r_arithmetic) {
      return f(this -> l[i % this -> l.size()], this -> r);
    } else if constexpr(l_arithmetic && !r_arithmetic) {
      return f(this -> l, this -> r[i % this -> r.size()]); 
    } else {
      Rcpp::stop("Error");
    }
  } 
  
};

template<typename T>
struct doubleWrapper {
  T d;
  doubleWrapper(double a) {
    d = a;
  }
  bool im() const {
    return false;
  }
  size_t nr() const {
    return 0;
  }
  size_t nc() const {
    return 0;
  }
  size_t size() const {
    return 1;
  }
  T data() const {
    return d;
  }
};

/*
template<typename T>
auto convert(T& obj) {
  if constexpr (std::is_same_v<T, double>) {
    return doubleWrapper<std::decay_t<T>>(std::forward<T>(obj));
  } else {
    return std::forward<T>(obj);
  }
}
*/
/*
template <typename T>
auto convert(T &&obj) -> std::conditional_t<std::is_same_v<T, double>, doubleWrapper<T>, T> {
    if constexpr (std::is_same_v<T, double>) {
        return doubleWrapper<T>(std::forward<T>(obj));
    } else {
        return std::forward<T>(obj);
    }
}
*/
template <typename T>
requires std::is_same_v<T, double>
constexpr doubleWrapper<T> convert(const T& obj) {
  return doubleWrapper<T>(obj);
}

template <typename T>
constexpr T convert(const T& obj) {
  return obj;
}

template <typename L, typename R>
inline auto  ownAdd(const L &a, const R &b) -> VEC<double,
                                                      BinaryOperation<decltype(convert(a).d),
                                                                      decltype(convert(b).d), Addition>>{
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
    VEC<double, BinaryOperation<decltype(convert(a.d)), decltype(convert(b.d)), Addition>> ret(
      BinaryOperation<decltype(convert(a.d)), decltype(convert(b.d)), Addition>(a.data(), b.data(), ismatrix_, nrows_, ncols_));
    ret.set_matrix(ismatrix_);
    ret.set_ncol(ncols_);
    ret.set_nrow(nrows_);
    return ret;
  } else if constexpr(l_arithmetic && !r_arithmetic) {
    doubleWrapper<L> aWrapped(a);
    defineMatrix<1>(aWrapped.im(), b.im(),
                   aWrapped.nr(), b.nr(),
                   aWrapped.nc(), b.nc(),
                    ismatrix_, nrows_, ncols_);
    VEC<double, BinaryOperation<decltype(convert(aWrapped.d)), decltype(convert(b.d)), Addition>> ret(
      BinaryOperation<decltype(convert(aWrapped.d)), decltype(convert(b.d)), Addition>(a, b.data(), ismatrix_, nrows_, ncols_));
    ret.set_matrix(ismatrix_);
    ret.set_ncol(ncols_);
    ret.set_nrow(nrows_);
    return ret;
  } else if constexpr(!l_arithmetic && r_arithmetic) {
    doubleWrapper<L> bWrapped(b);
    defineMatrix<1>(a.im(), bWrapped.im(),
                    a.nr(), bWrapped.nr(),
                    a.nc(), bWrapped.nc(),
                    ismatrix_, nrows_, ncols_);
    VEC<double, BinaryOperation<decltype(convert(a.d)), decltype(bWrapped.d), Addition>> ret(
      BinaryOperation<decltype(convert(a.d)), decltype(bWrapped.d), Addition>(
          a.data(), b, ismatrix_, nrows_, ncols_));
    ret.set_matrix(ismatrix_);
    ret.set_ncol(ncols_);
    ret.set_nrow(nrows_);
    return ret;
  }
}


/*
 note: candidate:
  ‘etr::BinaryOperation<L2, R2, f, CTrait, Trait>::BinaryOperation(
  etr::BinaryOperation<L2, R2, f, CTrait, Trait>&)
  [with L2 = etr::STORE<double, etr::VariableTrait>;
  R2 = etr::STORE<double, etr::VariableTrait>;
  double (* f)(double, double) = etr::Addition; CTrait = etr::BinaryTrait; Trait = etr::PlusTrait]’
   69 |   BinaryOperation(BinaryOperation& other)


note:   no known conversion for argument 1 from 
‘const etr::BinaryOperation<
              etr::VEC<double>,
              etr::VEC<double>,
              etr::Addition,
              etr::BinaryTrait, etr::PlusTrait>’ to 
‘etr::BinaryOperation<etr::STORE<double, etr::VariableTrait>,
                      etr::STORE<double, etr::VariableTrait>,
                      etr::Addition,
                      etr::BinaryTrait, etr::PlusTrait>&’
   69 |   BinaryOperation(BinaryOperation& other)
      |                   ~~~~~~~~~~~~~~~~~^~~~~

*/

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