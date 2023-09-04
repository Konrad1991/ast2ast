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

template <typename T, typename L, typename R, typename Trait = VVPlusTrait>
class VVPLUS {

public:
  const L &l;
  const R &r;
  bool ismatrix;
  int rows_;
  int columns_;

  using TypeTrait = Trait;
  VVPLUS(const L &a, const R &b, bool ismatrix_, int rows, int cols)
      : l(a), r(b), ismatrix(ismatrix_), rows_(rows), columns_(cols) {}

  T operator[](const int i) const { return l[i % l.size()] + r[i % r.size()]; } // issue: correct with % l.size !!!!!!!!!!!

  T get_deriv_left(const int i) const { return 1.0; }

  T get_deriv_right(const int i) const { return 1.0; }

  int size() const {
    int sz = (l.size() > r.size()) ? l.size() : r.size();
    return sz; // l.size(); // correct?
  }

  bool im() const { return this->ismatrix; }

  int nc() const { return columns_; }

  int nr() const { return rows_; }

  const L &getL() const { return l; }

  const R &getR() const { return r; }
};

template <typename T, typename L, typename R>
inline VEC<T, VVPLUS<T, L, R>> operator+(const VEC<T, L> &a,
                                         const VEC<T, R> &b) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;
  if ((a.im() == true) || (b.im() == true) ||
      (a.im() == true && b.im() == true)) {
    ismatrix_ = true;

    if ((a.im() == true) && (b.im() == true)) {
      nrows_ = (a.nr() > b.nr()) ? a.nr() : b.nr();
      ncols_ = (a.nc() > b.nc()) ? a.nc() : b.nc();
    } else if ((a.im() == false) && (b.im() == true)) {
      nrows_ = b.nr();
      ncols_ = b.nc();
    } else if ((a.im() == true) && (b.im() == false)) {
      nrows_ = a.nr();
      ncols_ = a.nc();
    } else {
#ifdef RLANG
      Rcpp::stop("Error");
#else
      exit(EXIT_FAILURE);
#endif
    }
  }

  VEC<T, VVPLUS<T, L, R>> ret(
      VVPLUS<T, L, R>(a.data(), b.data(), ismatrix_, nrows_, ncols_));

  ret.set_matrix(ismatrix_);
  ret.set_ncol(ncols_);
  ret.set_nrow(nrows_);

  return ret;
}

template <typename T, typename L, typename R, typename Trait = VSPlusTrait>
class VSPLUS {

public:
  const L &l;
  const R &r;
  bool ismatrix;
  int rows_;
  int columns_;

  using TypeTrait = Trait;
  VSPLUS(const L &a, const R &b, bool ismatrix_, int nrows_, int ncols_)
      : l(a), r(b), ismatrix(ismatrix_), rows_(nrows_), columns_(ncols_) {}

  T operator[](const int i) const { return l[i % l.size()] + r; }

  T get_deriv_left(const int i) const { return 1.0; }

  T get_deriv_right(const int i) const { return 1.0; }

  int size() const { return l.size(); }

  bool im() const { return ismatrix; }

  int nc() const { return columns_; }

  int nr() const { return rows_; }

  const L &getL() const { return l; }

  const R &getR() const { return r; }
};

template <typename T, typename L, typename R>
  requires std::is_same_v<R, double>
inline VEC<T, VSPLUS<T, L, R>> operator+(const VEC<T, L> &a, const R &b) {

  bool ismatrix_ = false;
  int rows_ = 0;
  int columns_ = 0;

  if (a.im() == true) {
    ismatrix_ = true;
    rows_ = a.nr();
    columns_ = a.nc();
  }

  VEC<T, VSPLUS<T, L, R>> ret(
      VSPLUS<T, L, R>(a.data(), b, a.im(), a.nr(), a.nc()));

  ret.set_matrix(ismatrix_);
  ret.set_ncol(columns_);
  ret.set_nrow(rows_);

  return ret;
}

template <typename T, typename L, typename R, typename Trait = SVPlusTrait>
class SVPLUS {

public:
  const L &l;
  const R &r;
  bool ismatrix;
  int rows_;
  int columns_;

  using TypeTrait = Trait;
  SVPLUS(const L &a, const R &b, bool ismatrix_, int nrows_, int ncols_)
      : l(a), r(b), ismatrix(ismatrix_), rows_(nrows_), columns_(ncols_) {}

  T operator[](const int i) const { return l + r[i % r.size()]; }

  T get_deriv_left(const int i) const { return 1.0; }

  T get_deriv_right(const int i) const { return 1.0; }

  int size() const { return r.size(); }

  bool im() const { return ismatrix; }

  int nc() const { return columns_; }

  int nr() const { return rows_; }

  const L &getL() const { return l; }

  const R &getR() const { return r; }
};

template <typename T, typename L, typename R>
  requires std::is_same_v<L, double>
inline VEC<T, SVPLUS<T, L, R>> operator+(const L &a, const VEC<T, R> &b) {

  bool ismatrix_ = false;
  int nrows_ = 0;
  int ncols_ = 0;

  if (b.im() == true) {
    ismatrix_ = true;
    nrows_ = b.nr();
    ncols_ = b.nc();
  }

  VEC<T, SVPLUS<T, L, R>> ret(
      SVPLUS<T, L, R>(a, b.data(), b.im(), b.nr(), b.nc()));

  ret.set_matrix(ismatrix_);
  ret.set_ncol(ncols_);
  ret.set_nrow(nrows_);

  return ret;
}

} // namespace etr

#endif