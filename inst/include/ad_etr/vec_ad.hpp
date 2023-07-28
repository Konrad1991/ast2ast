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
#include "operator.hpp"

namespace etr {

/*
Vector & matrix module
*/
template <typename T, typename R = STORE<T>
          // ,typename Trait = VariableTrait
          >
class VEC {
  using store = STORE<double, VariableTrait>;
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

  template <typename T2> VEC(T2 n) = delete;

  VEC(const double value)
      : subsetted(0), ismatrix(0), d(1, value), temp(1) {
  } // d(1, value) or d(1) {d[0] = value} --> all tests run positive
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
  template <typename TD>
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

  template <typename Tree_L, typename Tree_R>
  void fill_tape(const Tree_L &l, const Tree_R &r, std::vector<Operation<store> >& tape, const int optor) const {
    using trait_l = std::remove_reference<decltype(l)>::type::TypeTrait;
    using trait_r = std::remove_reference<decltype(r)>::type::TypeTrait;
    constexpr bool isVar_l = std::is_same<trait_l, VariableTrait>::value;
    constexpr bool isVar_r = std::is_same<trait_r, VariableTrait>::value;
    if constexpr (!isVar_l && !isVar_r) {
      std::vector<double> l_(l.size());
      for(int i = 0; i < l.size(); i++) l_[i] = l[i];
      std::vector<double> r_(r.size());
      for(int i = 0; i < r.size(); i++) r_[i] = r[i];
      Operation<store> op(l_, r_, optor);
      tape.push_back(op);
    } else if constexpr (!isVar_l && isVar_r) {
      std::vector<double> l_(l.size());
      for(int i = 0; i < l.size(); i++) l_[i] = l[i];
      std::vector<double> r_(r.size());
      for(int i = 0; i < r.size(); i++) r_[i] = r[i];
      Operation<store> op(l_, r_, optor);
      tape.push_back(op);
      tape.back().radd = &r;
    } else if constexpr (!isVar_r && isVar_l) {
      std::vector<double> l_(l.size());
      for(int i = 0; i < l.size(); i++) l_[i] = l[i];
      std::vector<double> r_(r.size());
      for(int i = 0; i < r.size(); i++) r_[i] = r[i];
      Operation<store> op(l_, r_, optor);
      tape.push_back(op);
      tape.back().ladd = &l;
    } else if constexpr (isVar_l && isVar_r) {
      std::vector<double> l_(l.size());
      for(int i = 0; i < l.size(); i++) l_[i] = l[i];
      std::vector<double> r_(r.size());
      for(int i = 0; i < r.size(); i++) r_[i] = r[i];
      Operation<store> op(l_, r_, optor);
      tape.push_back(op);
      tape.back().ladd = &l;
      tape.back().radd = &r;
    }
  }

  template <typename T2>
    requires HasTypeTrait<T2>
  void walk(const T2 &other_vec, std::vector<Operation<store> >& tape) {
    if constexpr (std::is_same_v<typename T2::TypeTrait, VariableTrait>) {
      return;
    } else {
      auto& r = other_vec.getR();
      auto& l = other_vec.getL();
      using trait_l = std::remove_reference<decltype(l)>::type::TypeTrait;
      using trait_r = std::remove_reference<decltype(r)>::type::TypeTrait;
      constexpr bool isVar_l = std::is_same<trait_l, VariableTrait>::value;
      constexpr bool isVar_r = std::is_same<trait_r, VariableTrait>::value;
      using trait_vec = std::remove_reference<decltype(other_vec)>::type::TypeTrait; 

      Rcpp::Rcout << "trait node " << demangle(typeid(trait_vec).name()) <<  " value of node: " << other_vec[0] << 
                   " trait left child " << demangle(typeid(trait_l).name()) << " value of left child: " << l[0] << 
                   " trait right child " << demangle(typeid(trait_r).name()) << " value of right child: " << r[0] << std::endl;

      constexpr int vec_trait = checkTraits<trait_vec>();
      if constexpr (isVar_r && isVar_l) {
        fill_tape(l, r, tape, vec_trait);
        return;
      } else if constexpr (!isVar_l && isVar_r) {
        fill_tape(l, r, tape, vec_trait);
        walk(l, tape);
      } else if constexpr (isVar_l && !isVar_r) {
        fill_tape(l, r, tape, vec_trait);
        walk(r, tape);
      } else if constexpr (!isVar_l && !isVar_r) {
        fill_tape(l, r, tape, vec_trait);
        walk(l, tape);
        walk(r, tape);
      } 
    }
  }

  template <typename T2>
    requires(!HasTypeTrait<T2>)
  void walk(const T2 &other_vec, std::vector<Operation<store> >& tape) {
    auto d_other_vec = other_vec.data(); // extract e.g VVPLUS from VEC
    using trait_vec = std::remove_reference<decltype(d_other_vec)>::type::TypeTrait; // type if current operation
    constexpr int vec_trait = checkTraits<trait_vec>();
    auto& r = d_other_vec.getR();
    auto& l = d_other_vec.getL();
    using trait_l = std::remove_reference<decltype(l)>::type::TypeTrait;
    using trait_r = std::remove_reference<decltype(r)>::type::TypeTrait;

    Rcpp::Rcout << "trait node " << demangle(typeid(trait_vec).name()) <<  " value of node: " << d_other_vec[0] << 
                   " trait left child " << demangle(typeid(trait_l).name()) << " value of left child: " << l[0] << 
                   " trait right child " << demangle(typeid(trait_r).name()) << " value of right child: " << r[0] << std::endl;

    constexpr bool isVar_l = std::is_same<trait_l, VariableTrait>::value;
    constexpr bool isVar_r = std::is_same<trait_r, VariableTrait>::value;
    if constexpr (isVar_r && isVar_l) {
        fill_tape(l, r, tape, vec_trait);
        return;
      } else if constexpr (!isVar_l && isVar_r) {
        fill_tape(l, r, tape, vec_trait);
        walk(l, tape);
      } else if constexpr (isVar_l && !isVar_r) {
        fill_tape(l, r, tape, vec_trait);
        walk(r, tape);
      } else if constexpr (!isVar_l && !isVar_r) {
        fill_tape(l, r, tape, vec_trait);
        walk(l, tape);
        walk(r, tape);
    } 
  }

  template <typename T2>
    requires HasTypeTrait<T2>
  void extract_variables(const T2& other_vec, std::vector<const store*>& vars) {
      if constexpr (std::is_same_v<typename T2::TypeTrait, VariableTrait>) {
        vars.push_back(&other_vec);
        return;
      } else {
          auto& r = other_vec.getR();
          auto& l = other_vec.getL();
          using trait_l = std::remove_reference<decltype(l)>::type::TypeTrait;
          using trait_r = std::remove_reference<decltype(r)>::type::TypeTrait;
          constexpr bool isVar_l = std::is_same<trait_l, VariableTrait>::value;
          constexpr bool isVar_r = std::is_same<trait_r, VariableTrait>::value;
          if constexpr (isVar_l && isVar_r) {
            vars.push_back(&l);
            vars.push_back(&l);
            vars.push_back(&r);
            vars.push_back(&r);
          } else if constexpr(!isVar_l && isVar_r) {
            vars.push_back(&r); // r.laddress
            vars.push_back(&r); // r.raddress
            extract_variables(l, vars);
          } else if constexpr(isVar_l && !isVar_r) {
            vars.push_back(&l); // l.laddress
            vars.push_back(&l); // l.raddress
            extract_variables(r, vars);
          } else {
            extract_variables(l, vars);
            extract_variables(r, vars);
          }        
      }
  }

  void which(const store* add, std::vector<const store*>& vars, std::vector<int>& indices) {
    std::fill(indices.begin(), indices.end(), -1);
    for(int i = 0; i < vars.size(); i++) {
      if(vars[i] == add) {
        indices[i] = i;
        break;
      } 
    }
  }

  void fill_trace(std::vector<int>& trace, std::vector<int>& indices, int num_var, int i, int val) {
    for(int j = 0; j < indices.size(); j++) {
          if(indices[j] >= 0) {
            trace[i * num_var + indices[j]] = val;
          }
    }
  }

  std::vector<int> calc_trace(std::vector<Operation<store> >& tape, int num_var, std::vector<const store*>& vars) {
    std::vector<int> trace(tape.size() * num_var);
    std::fill(trace.begin(), trace.end(), -1);
    std::vector<int> indices(num_var);
    for(int i = 0; i < tape.size(); i++) {
      if(tape[i].ladd != nullptr && tape[i].radd == nullptr) {
        which(tape[i].ladd, vars, indices);
        fill_trace(trace, indices, num_var, i, 0);
      } else if(tape[i].radd != nullptr && tape[i].ladd == nullptr) {
        which(tape[i].radd, vars, indices);
        fill_trace(trace, indices, num_var, i, 1);
      } else if(tape[i].ladd != nullptr && tape[i].radd != nullptr) {
        which(tape[i].ladd, vars, indices);
        fill_trace(trace, indices, num_var, i, 0);
        which(tape[i].radd, vars, indices);
        fill_trace(trace, indices, num_var, i, 1);
      } 

      if(tape[i].ladd == tape[i].radd) {
        which(tape[i].ladd, vars, indices);
        fill_trace(trace, indices, num_var, i, 2);
        which(tape[i].radd, vars, indices);
        fill_trace(trace, indices, num_var, i, 2);
      }
    }
    return trace;
  }

  std::vector<double> backward(std::vector<Operation<store> >& tape, std::vector<int>& trace, int num_var) {
    std::vector<double> derivs(num_var);
    std::fill(derivs.begin(), derivs.end(), 0);
    for(int i = tape.size() - 1; i >= 0; i--) {
      Operation op = tape[i];
      for(int j = 0; j < num_var; j++) {
        int idx = trace[i * num_var + j];
        if(idx == 0) {
          derivs[j] += op.evaluate_deriv(true, 0);
        } else if(idx == 1) {
          derivs[j] += op.evaluate_deriv(false, 0);
        } else if(idx == 2) {
          derivs[j] += 2.0*op.evaluate_deriv(true, 0);
        } 
      }
    }
    return derivs;
  }

  template <typename T2, typename R2>
  VEC &operator=(const VEC<T2, R2> &other_vec) {

    std::string demangled = demangle(typeid(other_vec).name());
    Rcpp::Rcout << demangled << std::endl;

    std::vector<Operation<store> > tape;
    
    walk(other_vec, tape);

    Rcpp::Rcout << "start of tape " << std::endl;
    for(int i = 0; i < tape.size(); i++) {
      tape[i].print();
    }
    Rcpp::Rcout << "end of tape " << std::endl;

    std::vector<const store*> vars;
    extract_variables(other_vec.data().getL(), vars);
    extract_variables(other_vec.data().getR(), vars);
    remove_duplicates(vars);
    Rcpp::Rcout << "start vars " << std::endl;
    for(auto i: vars) Rcpp::Rcout << i << std::endl;
    Rcpp::Rcout << "end vars " << std::endl;
    int num_var = vars.size();

    std::vector<int> trace = calc_trace(tape, num_var, vars);

    std::vector<double> derivs = backward(tape, trace, num_var);
    Rcpp::Rcout << "start derivs " << std::endl;
    for(auto i: derivs) Rcpp::Rcout << i << std::endl;
    Rcpp::Rcout << "end derivs " << std::endl;

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

  T &back() const { return d.p[this->size() - 1]; }

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
