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
#ifndef VEC_H
#define VEC_H

#include "pointer_storage.hpp"

namespace etr {

/*
Vector & matrix module
*/
template <typename T, typename R = STORE<T>>
class VEC {
  using store = STORE<double, VariableTrait>;

public:
  // data
  R d;
  using trait_d = std::remove_reference<decltype(d)>::type::TypeTrait;
  using is_var = std::is_same<trait_d, VariableTrait>;
  using is_sub = std::is_same<trait_d, SubsetTrait>;

  template <typename T2> VEC(T2 n) = delete;

  VEC(const double value)
      : d(1, value) {
  } // d(1, value) or d(1) {d[0] = value} --> all tests run positive
  VEC(const long unsigned int n)
      : d(1, static_cast<double>(n)) {
  } // run all tests whether this is possible
  // Constructors for vector
  VEC(const int n) : d(n) {
    d.fill(static_cast<double>(n));
  } // fill is a hack that sexp s = 1 works;
  VEC(const int n, const double value) : d(n, value) {}

  VEC(const R &other_vec) : d(other_vec) {}

  VEC(): d() {} // maybe better initialize with 0
  VEC(const std::vector<T> inp) : d(inp) {}
  // Constructors for matrix
  VEC(const int rows, const int cols): d(rows * cols) {
    set_matrix(true);
    set_nrow(rows);
    set_ncol(cols);
  }
  VEC(const int rows, const int cols, const double value)
      : d(rows * cols, value) {
    set_matrix(true);
    set_nrow(rows);
    set_ncol(cols);
  }
  VEC(const int rows, const int cols, int value)
      : d(rows * cols, value) {
    set_matrix(true);
    set_nrow(rows);
    set_ncol(cols);
  }
  // constructor for calculations
  template <typename T2, typename R2>
  VEC(const VEC<T2, R2> &other_vec) : d(1) {
    if constexpr (is_var::value) {
      this->d.resize(other_vec.size());
      for (int i = 0; i < d.size(); i++) {
        this->d[i] = other_vec[i];
      }
      if (other_vec.d.im() == true) {
        set_matrix(true);
        set_nrow(other_vec.d.nr());
        set_ncol(other_vec.d.nc());
      }
    }
  }
  // constructor for COMPARISON
  VEC(const VEC<bool> &other_vec) : d(1) {
    d.resize(other_vec.size());
    set_matrix(false);
    for (int i = 0; i < d.size(); i++) {
      d[i] = other_vec[i];
    }
  }
  // constructor for pointer
  VEC(const int n, T *ptr, int cob)
      : d(n, ptr, cob) {} // cob = copy, owning, borrow
  VEC(const int r, const int c, T *ptr, int cob)
      : d(r * c, ptr, cob) {
      set_matrix(true);
      set_nrow(r);
      set_ncol(c);
  } // cob = copy, owning, borrow

  operator bool() const { return d[0]; } // issue: if d has length 1 (R version 4.2)

  operator SEXP() const {
    SEXP ret = R_NilValue;

    if (this->ismatrix) {
      ret = PROTECT(Rf_allocMatrix(REALSXP, this->nrows, this->ncols));
    } else {
      ret = PROTECT(Rf_allocVector(REALSXP, this->d.size()));
    }

    for (int i = 0; i < d.size(); i++) {
      REAL(ret)[i] = d[i];
    }

    UNPROTECT(1);

    return ret;
  }

  // constructor for Rcpp RcppArmadillo and SEXP
  VEC(const bool value) : d(1, value) {}
  VEC(Rboolean value) : d(1, value) {}
  VEC(SEXP inp) : d(1, 1.5) { // issue: 1.5????
    if (Rf_isReal(inp)) {
      d.init_sexp(inp);
      if (Rf_isMatrix(inp) == true) {
        set_matrix(true);
        set_ncol(Rf_ncols(inp));
        set_nrow(Rf_nrows(inp));
      }
    } else {
      Rf_error("no numeric input");
    }
  }
  VEC(Rcpp::NumericVector other_vec)
      : d(1, 1.5) {
    d.resize(other_vec.size());
    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
    }
  }
  VEC(Rcpp::NumericMatrix other_vec)
      : d(1, 1.5) {
    d.resize(other_vec.size());
    set_matrix(true);
    set_ncol(other_vec.ncol());
    set_nrow(other_vec.nrow());
    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
    }
  }
  VEC(arma::vec &other_vec) : d(1, 1.5) {
    d.resize(other_vec.size());
    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
    }
  }
  VEC(arma::mat &other_vec) : d(1, 1.5) {
    d.resize(other_vec.size());
    set_matrix(true);
    set_ncol(other_vec.n_cols);
    set_nrow(other_vec.n_rows);
    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
    }
  }

  VEC &operator=(SEXP inp) { 
    ass(Rf_isReal(inp), "no numeric input");
    if (Rf_isMatrix(inp) == true) {
      set_matrix(true);
      set_ncol(Rf_ncols(inp));
      set_nrow(Rf_nrows(inp));
    }
    d.init_sexp(inp);
    return *this;
  }

  operator Rcpp::NumericVector() const {
    Rcpp::NumericVector ret(this->size());
    for (int i = 0; i < ret.size(); i++) {
      ret[i] = d[i];
    }
    return ret;
  }

  VEC &operator=(Rcpp::NumericVector &other_vec) {
    if constexpr(!is_var::value) {
      ass(other_vec.size() <= d.size(), "number of items to replace is not a multiple of replacement length");
      for (int i = 0; i < other_vec.size(); i++) {
        d[i] = other_vec[i];
      }
    } else {
      d.resize(other_vec.size());
      for (int i = 0; i < other_vec.size(); i++) {
        d[i] = other_vec[i];
      }
    }
    return *this;
  }

  operator Rcpp::NumericMatrix() const {
    ass(this->im() == true, "Object cannot be converted to NumericMatrix");
    Rcpp::NumericMatrix ret(this->nr(), this->nc());
    for (int i = 0; i < ret.size(); i++) {
      ret[i] = d[i];
    }
    return ret;
  }

  VEC &operator=(Rcpp::NumericMatrix &other_vec) {
    if constexpr(!is_var::value) {
      ass(other_vec.size() <= d.size(), "number of items to replace is not a multiple of replacement length");
      set_matrix(true);
      set_nrow(other_vec.nrow());
      set_ncol(other_vec.ncol());
      for (int i = 0; i < other_vec.size(); i++) {
        d[i] = other_vec[i];
      }
    } else {
      d.resize(other_vec.size());
      set_matrix(true);
      set_nrow(other_vec.nrow());
      set_ncol(other_vec.ncol());
      for (int i = 0; i < other_vec.size(); i++) {
        d[i] = other_vec[i];
      }  
    }
    return *this;
  }

  operator arma::vec() const {
    arma::vec ret(this->size());
    for (int i = 0; i < this->size(); i++) {
      ret[i] = d[i];
    }
    return ret;
  }

  VEC &operator=(arma::vec &other_vec) { // issue: correct for armadillo () vs []
    if constexpr(!is_var::value) {
      ass(other_vec.size() <= d.size(), "number of items to replace is not a multiple of replacement length");
      for (int i = 0; i < other_vec.size(); i++) {
        d[i] = other_vec[i];
      }
    } else {
      d.resize(other_vec.size());
      for (int i = 0; i < other_vec.size(); i++) {
        d[i] = other_vec[i];
      }
    }
    return *this;
  }

  operator arma::mat() const {
    ass(this->im() == true, "Object cannot be converted to arma::mat");
    arma::mat ret(this->nr(), this->nc());
    for (int i = 0; i < ret.size(); i++) {
      ret[i] = d[i];
    }
    return ret;
  }

  VEC &operator=(arma::mat &other_vec) { // issue: see above
    if constexpr(!is_var::value) {
      ass(other_vec.size() <= d.size(), "number of items to replace is not a multiple of replacement length");
      set_matrix(true);
      set_nrow(other_vec.n_rows);
      set_ncol(other_vec.n_cols);
      for (int i = 0; i < other_vec.size(); i++) {
        d[i] = other_vec[i];
      }
    } else {
      d.resize(other_vec.size());
      set_matrix(true);
      set_nrow(other_vec.n_rows);
      set_ncol(other_vec.n_cols);
      for (int i = 0; i < other_vec.size(); i++) {
        d[i] = other_vec[i];
      }  
    }
    return *this;
  }

  // vector & matrix operator=
  // ================================================================
  template <typename TD>
    requires std::is_same_v<TD, double>
  VEC &operator=(const TD &dob) {
    if constexpr(!is_var::value) {
      for(std::size_t i = 0; i < d.size(); i++) {
        d[i] = dob;
      }
    } else {
        d.resize(1);
        d[0] = dob;
    }
    return *this;
  }

  VEC &operator=(const VEC &other_vec) {
    if constexpr(!is_var::value) {
      ass(static_cast<int>(d.size()) <= other_vec.size(),
              "number of items to replace is not a multiple of replacement length");
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
    } else {
        if (d.size() != other_vec.size()) {
          d.resize(other_vec.size());
        }
        for (int i = 0; i < d.size(); i++) {
          d[i] = other_vec[i];
        }
    }

    if (other_vec.im() == true) {
      set_matrix(true);
      set_nrow(other_vec.nr());
      set_ncol(other_vec.nc());
    }
    
    return *this;
  }

  template <typename T2, typename R2>
  VEC &operator=(const VEC<T2, R2> &other_vec) {

    if constexpr(!is_var::value) {
        // issue: count references if counter > 0 --> than directly store in d --> has to be done everywhere
        if(d.ptr -> ref_counter > 1) { // issue: did I missed a method for this check?
            ass(d.size() <= other_vec.size(), "number of items to replace is not a multiple of replacement length");
            store temp;
            temp.resize(d.size());
            for (int i = 0; i < temp.size(); i++) {
              temp[i] = other_vec[i];
            }
            for (std::size_t i = 0; i < d.size(); i++) {
              this->d[i] = temp[i];
            }
            d.ptr -> ref_counter = 0;
        } else {
          for (int i = 0; i < other_vec.size(); i++) {
              this -> d[i] = other_vec[i];
          }
        }
    } else {
        store temp;
        temp.resize(other_vec.size());
        for (int i = 0; i < temp.size(); i++) {
          temp[i] = other_vec[i];
        }
        if (d.size() != other_vec.size()) { // .d?
          d.resize(other_vec.size());       // .d ?
        }
        if (d.todelete == true) {
          d.moveit(temp);
        } else {
          this->d = temp; // copy necessary in case only ownership is borrowed!
        }
    }

    if (other_vec.im() == true) {
      set_matrix(true);
      set_nrow(other_vec.nr());
      set_ncol(other_vec.nc());
    }
    
    return *this;
  }

  // getter methods for vector
  // ================================================================
  int size() const { return d.size(); }

  T operator[](int i) const { return d[i]; }

  T &operator[](int i) { return d[i]; }

  const R &data() const {
    return d; // d.p
  }

  R &data() { return d; }

  T *pointer() { return d.data(); }

  // ================================================================

  void realloc(int new_size) { // when is it called?
    d.realloc(new_size);
  }

  // getter methods for matrix
  // ================================================================
  int ncol() const { return d.columns_; }

  int nrow() const { return d.rows_; }

  bool im() const { return d.ismatrix; }

  int nc() const { return d.columns_; }

  int nr() const { return d.rows_; }

  auto begin() const { return It<T>{d.p}; }

  auto end() const { return It<T>{d.p + this->size()}; }

  T &back() const { return d.p[this->size() - 1]; }

  // resize indices
  void rsi(int sizenew) { this->indices.resize(sizenew); }

  void set_matrix(bool i) {
    this -> d.ismatrix = i;
  }

  void set_ncol(int ncol) {
    this -> d.columns_ = ncol;
  }

  void set_nrow(int nrow) {
    this -> d.rows_ = nrow;
  }

}; // end class VEC

// type traits for VEC<T>
template <typename T> struct is_vec : std::false_type {};

template <> struct is_vec<VEC<double>> : std::true_type {};

template <> struct is_vec<std::initializer_list<double>> : std::true_type {};

template <> struct is_vec<double> : std::false_type {};

inline int d2i(double inp) { return static_cast<int>(inp); }

inline double i2d(int inp) { return static_cast<double>(inp); }

inline double &at(const VEC<double> &inp, int i) {
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &&inp, int i) {
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &inp, double i_) {
  int i = d2i(i_);
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &&inp, double i_) {
  int i = d2i(i_);
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &inp, int r, int c) {

  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline double &at(const VEC<double> &inp, double r_, double c_) {

  ass(inp.im() == true, "Input is not a matrix!");
  int r = d2i(r_);
  int c = d2i(c_);
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline double &at(const VEC<double> &&inp, int r, int c) {

  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline double &at(const VEC<double> &&inp, double r_, double c_) {

  ass(inp.im() == true, "Input is not a matrix!");
  int r = d2i(r_);
  int c = d2i(c_);
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

} // namespace etr

#endif
