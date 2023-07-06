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
template <typename T, typename R = STORE<T>
// ,typename Trait = NullTrait
  > class VEC {

private:
public:
  bool subsetted;
  std::vector<int> indices;

  // for Matrix
  bool ismatrix;
  int ncols;
  int nrows;

  int ncols_sub;
  int nrows_sub;

  // data
  R d;
  STORE<T> temp;
  //using TypeTrait = Trait;

  template <typename T2> VEC(T2 n) = delete;

  VEC(const double value)
      : subsetted(0), ismatrix(0), d(1, value), temp(1) {} // d(1, value) or d(1) {d[0] = value} --> all tests run positive
  VEC(const long unsigned int n)
      : subsetted(0), ismatrix(0), d(1, static_cast<double>(n)), temp(1) {
  } // run all tests whether this is possible
  // Constructors for vector
  VEC(const int n) : subsetted(0), ismatrix(0), d(n), temp(1) {
    d.fill(static_cast<double>(n));
  } // fill is a hack that sexp s = 1 works;
  VEC(const int n, const double value)
      : subsetted(0), ismatrix(0), d(n, value), temp(1) {}

  VEC(const R &other_vec) : subsetted(0), ismatrix(0), d(other_vec), temp(1) {}

  VEC()
      : subsetted(0), ismatrix(0), ncols(0), nrows(0), d(), temp() {
  } // maybe better initialize with 0
  VEC(const std::vector<T> inp)
      : subsetted(0), ismatrix(0), nrows(0), ncols(0), d(inp), temp(1) {}
  // Constructors for matrix
  VEC(const int rows, const int cols)
      : subsetted(0), ismatrix(1), ncols(cols), nrows(rows), d(rows * cols),
        temp(1) {}
  VEC(const int rows, const int cols, const double value)
      : subsetted(0), ismatrix(1), ncols(cols), nrows(rows),
        d(rows * cols, value), temp(1) {}
  VEC(const int rows, const int cols, int value)
      : subsetted(0), ismatrix(1), nrows(rows), ncols(cols),
        d(rows * cols, value), temp(1) {}
  // constructor for calculations
  template <typename T2, typename R2>
  VEC(const VEC<T2, R2> &other_vec) : d(1), temp(1) {
    this->d.resize(other_vec.size());
    this->ismatrix = false;
    for (int i = 0; i < d.size(); i++) {
      this->d[i] = other_vec[i];
    }

    if (other_vec.d.im() == true) {
      this->ismatrix = true;
      this->ncols = other_vec.d.nc();
      this->nrows = other_vec.d.nr();
    }

    subsetted = false;
  }
  // constructor for COMPARISON
  VEC(const VEC<bool> &other_vec) : d(1), temp(1) {
    d.resize(other_vec.size());
    ismatrix = false;
    subsetted = false;
    for (int i = 0; i < d.size(); i++) {
      d[i] = other_vec[i];
    }
  }
  // constructor for pointer
  VEC(const int n, T *ptr, int cob)
      : subsetted(0), ismatrix(0), d(n, ptr, cob), temp(1) {
  } // cob = copy, owning, borrow
  VEC(const int r, const int c, T *ptr, int cob)
      : subsetted(0), ismatrix(1), ncols(c), nrows(r), d(r * c, ptr, cob),
        temp(1) {} // cob = copy, owning, borrow

  operator bool() const { return d[0]; }

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
  VEC(const bool value) : subsetted(0), ismatrix(0), d(1, value), temp(1) {}
  VEC(Rboolean value) : subsetted(0), ismatrix(0), d(1, value), temp(1) {}
  VEC(SEXP inp) : subsetted(0), ismatrix(0), d(1, 1.5), temp(1) {

    if (Rf_isReal(inp)) {
      d.init_sexp(inp);
      subsetted = false;
      ismatrix = false;
      if (Rf_isMatrix(inp) == true) {
        ismatrix = true;
        ncols = Rf_ncols(inp);
        nrows = Rf_nrows(inp);
      }
    } else {
      Rf_error("no numeric input");
    }
  }
  VEC(Rcpp::NumericVector other_vec)
      : subsetted(0), ismatrix(0), d(1, 1.5), temp(1) {
    d.resize(other_vec.size());
    this->ismatrix = false;
    this->ncols = 0;
    this->nrows = 0;
    subsetted = false;

    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
    }
  }
  VEC(Rcpp::NumericMatrix other_vec)
      : subsetted(0), ismatrix(0), d(1, 1.5), temp(1) {
    d.resize(other_vec.size());
    this->ismatrix = true;
    this->ncols = other_vec.ncol();
    this->nrows = other_vec.nrow();
    subsetted = false;

    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
    }
  }
  VEC(arma::vec &other_vec) : subsetted(0), ismatrix(0), d(1, 1.5), temp(1) {
    d.resize(other_vec.size());
    this->ismatrix = false;
    this->ncols = 0;
    this->nrows = 0;
    subsetted = false;

    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
    }
  }
  VEC(arma::mat &other_vec) : subsetted(0), ismatrix(0), d(1, 1.5), temp(1) {
    d.resize(other_vec.size());
    this->ismatrix = true;
    this->ncols = other_vec.n_cols;
    this->nrows = other_vec.n_rows;
    subsetted = false;

    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
    }
  }

  VEC &operator=(SEXP inp) {
    subsetted = false;
    ismatrix = false;

    ass(Rf_isReal(inp), "no numeric input");
    if (Rf_isMatrix(inp) == true) {
      ismatrix = true;
      ncols = Rf_ncols(inp);
      nrows = Rf_nrows(inp);
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

    d.resize(other_vec.size());
    this->ismatrix = false;
    this->ncols = 0;
    this->nrows = 0;
    subsetted = false;

    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
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

    d.resize(other_vec.size());
    this->ismatrix = true;
    this->ncols = other_vec.ncol();
    this->nrows = other_vec.nrow();
    subsetted = false;

    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
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

  VEC &operator=(arma::vec &other_vec) {

    d.resize(other_vec.size());
    this->ismatrix = false;
    this->ncols = 0;
    this->nrows = 0;
    subsetted = false;

    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
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

  VEC &operator=(arma::mat &other_vec) {

    d.resize(other_vec.size());
    this->ismatrix = true;
    this->ncols = other_vec.n_cols;
    this->nrows = other_vec.n_rows;
    subsetted = false;

    for (int i = 0; i < other_vec.size(); i++) {
      d[i] = other_vec[i];
    }

    return *this;
  }

  // vector & matrix operator=
  // ================================================================
  template<typename TD>
  requires std::is_same_v<TD, double>
  VEC &operator=(const TD &dob) {
    if (subsetted == false) {
      d.resize(1);
      ismatrix = false;
      d[0] = dob;
    } else {
      for (std::size_t i = 0; i < indices.size(); i++) {
        d[indices[i]] = dob;
      }
    }

    subsetted = false;
    return *this;
  }

  VEC &operator=(const VEC &other_vec) {
    if (this->subsetted == false) {
      if (d.size() != other_vec.size()) {
        d.resize(other_vec.size());
      }

      this->ismatrix = false;

      for (int i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }

      if (other_vec.im() == true) {
        this->ismatrix = true;
        this->ncols = other_vec.nc();
        this->nrows = other_vec.nr();
      }
    } else {

      ass(static_cast<int>(indices.size()) <= other_vec.size(),
          "number of items to replace is not a multiple of replacement length");

      for (std::size_t i = 0; i < indices.size(); i++) {
        d[indices[i]] = other_vec[i];
      }
    }

    subsetted = false;

    return *this;
  }

  template<typename Tree_L, typename Tree_R>
  void print_tree(const Tree_L& l,const Tree_R& r) const {
      std::string demangled_r = demangle(typeid(r).name());
      Rcpp::Rcout << "type of R: " << demangled_r << " address of R " << &r << std::endl;
      std::string demangled_l = demangle(typeid(l).name());
      Rcpp::Rcout << "type of L: " << demangled_l << " address of L " << &l << std::endl;
      Rcpp::Rcout << std::endl;
  }

  template <typename T2>  
  requires HasTypeTrait<T2>
  void walk(const T2 &other_vec) {
    if constexpr (std::is_same_v<typename T2::TypeTrait, NullTrait>) {
        return;
    } else {
        auto r = other_vec.getR();
        auto l = other_vec.getL();
        Rcpp::Rcout << "bla1 " << &r << " " << &l << std::endl;
        //using trait_l = typename std::remove_reference<decltype(l)>::type::TypeTrait;
        //using trait_r = typename std::remove_reference<decltype(r)>::type::TypeTrait;

        using trait_l = typename decltype(l)::TypeTrait;
        using trait_r = typename decltype(r)::TypeTrait;

        bool isNull_l = std::is_same<trait_l, NullTrait>::value;
        bool isNull_r = std::is_same<trait_r, NullTrait>::value;
    
        // base case
        if (isNull_r && isNull_l) {
          print_tree(l, r);
          return; 
        } else if (!isNull_l && isNull_r) {
          print_tree(l, r);
          walk(l);
        } else if (isNull_l && !isNull_r) {
          print_tree(l, r);
          walk(r);
        } else {
          print_tree(l, r);
          walk(l);
        }
    }
    
  }

  template <typename T2>  
  requires (!HasTypeTrait<T2>)
  void walk(const T2 &other_vec) {
    std::string demangled = demangle(typeid(other_vec).name());
    Rcpp::Rcout << demangled << std::endl;
    auto d_other_vec = other_vec.data(); // extract e.g VVPLUS from VEC
    std::string demangled_d = demangle(typeid(d_other_vec).name());
    Rcpp::Rcout << demangled_d << std::endl;
    auto r = d_other_vec.getR();
    auto& rRef = d_other_vec.getR(); // this works. auto& instead of auto! auto leads to a copy!
    auto l = d_other_vec.getL();
    Rcpp::Rcout << "bla2 " << &d_other_vec.l << " " << &d_other_vec.r << " " << &rRef << std::endl; // works --> ok than public l and r
    // or maybe test a reference_wrapper in the VVPLUS class. around r and l
    Rcpp::Rcout << "bla2 " << &r << " " << &l << std::endl;
    //using trait_l = typename std::remove_reference<decltype(l)>::type::TypeTrait;
    //using trait_r = typename std::remove_reference<decltype(r)>::type::TypeTrait;

    using trait_l = typename decltype(l)::TypeTrait;
    using trait_r = typename decltype(r)::TypeTrait;
    
    bool isNull_l = std::is_same<trait_l, NullTrait>::value;
    bool isNull_r = std::is_same<trait_r, NullTrait>::value;

    // base case
    if (isNull_r && isNull_l) {
      print_tree(l, r);
      return; 
    } else if (!isNull_l && isNull_r) {
      print_tree(l, r);
      walk(l);
    } else if (isNull_l && !isNull_r) {
      print_tree(l, r);
      walk(r);
    } else {
      print_tree(l, r);
      walk(l);
    }
    
  }

  template <typename T2, typename R2>
  VEC &operator=(const VEC<T2, R2> &other_vec) {

    walk(other_vec);
    
    if (subsetted == false) {
      this->ismatrix = false;

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

      if (other_vec.d.im() == true) {
        ismatrix = true;
        ncols = other_vec.d.nc();
        nrows = other_vec.d.nr();
      }

    } else {
      temp.resize(indices.size());
      for (int i = 0; i < temp.size(); i++) {
        temp[i] = other_vec[i];
      }
      for (std::size_t i = 0; i < indices.size(); i++) {
        this->d[indices[i]] = temp[i];
      }
    }

    subsetted = false;

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

  bool is_subsetted() const { return subsetted; }

  void realloc(int new_size) { // when is it called?
    d.realloc(new_size);
  }

  // getter methods for matrix
  // ================================================================
  int ncol() const { return ncols; }

  int nrow() const { return nrows; }

  bool im() const { return ismatrix; }

  int nc() const { return ncols; }

  int nr() const { return nrows; }

  auto begin() const { return It<T>{d.p}; }

  auto end() const { return It<T>{d.p + this->size()}; }

  T &back() const { return d.p[this->size() -1]; }

  // resize indices
  void rsi(int sizenew) { this->indices.resize(sizenew); }

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
