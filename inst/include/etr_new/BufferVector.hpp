#ifndef BUFFER_VECTOR_H
#define BUFFER_VECTOR_H

#include "UtilsTraits.hpp"
#include "unaryCalculations.hpp"
#include "binaryCalculations.hpp"

namespace etr {


template <typename T, typename Trait, typename CTrait> struct Buffer : public BaseStore<T> {
  	using TypeTrait = Trait;
  	using CaseTrait = CTrait;
};

template <typename T, typename R, typename Trait> struct Vec {
  using Type = T;
  using TypeTrait = Trait;
  using CaseTrait = Trait;
  R d;
  using typeTraitD = std::remove_reference<decltype(d)>::type::TypeTrait;
  using isBuffer = std::is_same<typeTraitD, BufferTrait>;
  using isBorrow = std::is_same<typeTraitD, BorrowTrait>;
  using isBorrowSEXP = std::is_same<typeTraitD, BorrowSEXPTrait>;
  using isSubset = std::is_same<typeTraitD, SubsetTrait>;
  using isSubsetCalc = std::is_same<typeTraitD, SubsetCalcTrait>;
  using caseTraitD = std::remove_reference<decltype(d)>::type::CaseTrait;
  using isUnaryOP = std::is_same<caseTraitD, UnaryTrait>;
  using isBinaryOP = std::is_same<caseTraitD, BinaryTrait>; 

  template <typename T2> Vec(T2 n) = delete;
  
  explicit Vec(size_t sz) : d(sz) {}
  explicit Vec(int sz) : d(static_cast<size_t>(sz)) {}

  explicit Vec() : d() {}
      
  explicit Vec(size_t rows, size_t cols) : d(rows * cols) {d.setMatrix(true, rows, cols); }

  explicit Vec(size_t rows, size_t cols, const double value) : d(rows * cols) {
  	d.setMatrix(true, rows, cols); 
  	d.fill(value);
  }

  template <typename LType, typename RType, binaryFct f,
            typename OperationTrait>
  explicit Vec(const BinaryOperation<LType, RType, f, OperationTrait> &&bOp) : d(bOp) {
    using TypeTrait = OperationTrait;
  }

  template <typename IType, UnaryFct f,
            typename OperationTrait>
  explicit Vec(const UnaryOperation<IType, f, OperationTrait> &&bOp) : d(bOp) {
    using TypeTrait = OperationTrait;
  }

  template <typename T2, typename R2, typename Trait2>
  explicit Vec(const Vec<T2, R2, Trait2> &other_vec) : d() {
  	using TypeTrait = Trait2;
  	using CaseTrait = Trait2;
    this->d.resize(other_vec.size());
    for (size_t i = 0; i < d.size(); i++) {
      d[i] = other_vec[i];
    }
    if (other_vec.d.im()) {
    	d.setMatrix(true, other_vec.nr(), other_vec.nc());
    }
  }

  template <typename T2, typename R2, typename Trait2>
  explicit Vec(const Vec<T2, R2, Trait2> &&other_vec) : d(std::move(other_vec.d)) {
    using TypeTrait = Trait2;
    using CaseTrait = Trait2;
  }

  size_t size() const { return d.size(); }
  bool im() const { return d.im(); }
  size_t nc() const { return d.nc(); }
  size_t nr() const { return d.nr(); }
  auto begin() const { return It<T>{d.p}; }
  auto end() const { return It<T>{d.p + this -> size()}; }
  T &back() { return d.p[this -> size()]; }
  void fill(T value) { d.fill(value); }
  void resize(size_t newSize) { d.resize(newSize); }

  T& operator[](size_t idx) {
    if constexpr (isSubset::value || isSubsetCalc::value) { // issue: find out why this is necessary. Maybe due to the fact that d.p is not const and set as nullptr
      ass(d.p != nullptr, "Subset is pointing to nothing!");
      return d.p -> operator[](d.ind[d.ind[idx] % d.ind.size()]); 
    } else {
      return d[idx];
    }
  }
  T operator[](size_t idx) const {
    if constexpr (isSubset::value || isSubsetCalc::value) { // issue: find out why this is necessary
      ass(d.p != nullptr, "Subset is pointing to nothing!");
      return d.p -> operator[](d.ind[d.ind[idx] % d.ind.size()]); 
    } else {
  	 return d[idx];
    }
  }

  template<typename T2>
  requires std::is_same_v<T2, bool>
  explicit Vec(const Vec<T2> &other_vec) : d() {
    d.resize(other_vec.size());
    for (size_t i = 0; i < d.size(); i++) d[i] = other_vec[i];
  }
  
  template<typename U = R, typename T2>
  requires std::is_same_v<U, Borrow<BaseType>>
  explicit Vec(const Borrow<T2>& borrowed) : d(borrowed) {}

  template<typename U = R>
  requires std::is_same_v<U, BorrowSEXP<BaseType>>
  explicit Vec(SEXP inp) : d(inp) {}

  template<typename L2>
  explicit Vec(Subset<L2>& inp) : d(inp) {}

  template<typename L2>
  explicit Vec(const Subset<L2>&& inp) : d(inp) {}

  template<typename L2>
  explicit Vec(SubsetCalc<L2>& inp) : d(inp) {}

  template<typename L2>
  explicit Vec(const SubsetCalc<L2>&& inp) : d(inp) {}

  template<typename L2, typename R2, binaryFct f>
  explicit Vec(BinaryOperation<L2, R2, f>& inp) : d(inp) {}

  template<typename L2, UnaryFct f>
  explicit Vec(UnaryOperation<L2, f>& inp) : d(inp) {}

  friend std::ostream& operator<<(std::ostream& os, const Vec& vec) {
      os << "Vec [ ";
      for (size_t i = 0; i < vec.size(); i++) {
          os << vec.d.p[i] << " ";
      }
      os << "]";
      return os;
  }

  template<typename TD>
  requires std::is_same_v<TD, double>
  Vec& operator=(const TD inp) {
    if constexpr(isSubset::value || isSubsetCalc::value) {
      for(size_t i = 0; i < this -> size(); i++) d.p -> operator[](d.ind[d.ind[i] % d.ind.size()]) = inp; // issue: find out why this is necessary
    } else {
      d.resize(1); d[0] = inp;
    }
    return *this;
  }

  Vec& operator=(const Vec<double>& other) {
    static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
    static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
    if (this == &other) return *this;
    if(size() != other.size()) {
      resize(other.size());
      for(size_t i = 0; i < other.size(); i++) d[i] = other[i];
    }
    return *this;
  }

  template <typename T2, typename R2, typename Trait2>
  Vec& operator=(const Vec<T2, R2, Trait2> &other_vec) {

  using caseTraitDInput = std::remove_reference<decltype(other_vec.d)>::type::CaseTrait;
  using isUnaryOPInput = std::is_same<caseTraitDInput, UnaryTrait>;
  using isBinaryOPInput = std::is_same<caseTraitDInput, BinaryTrait>; 

  static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
  static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
  static_assert(!isSubsetCalc::value, "Cannot assign to subset of a calculation");

    if constexpr (isUnaryOPInput::value || isBinaryOPInput::value) {
      if constexpr (isSubset::value) {
        Buffer<T> temp;
        ass(other_vec.size() == this -> size(), "number of items to replace is not a multiple of replacement length");
        if(d.p -> ref_counter > 1) {
          temp.resize(other_vec.size());
          for(size_t i = 0; i < other_vec.size(); i++) temp[i] = other_vec[i];
          d.resize(other_vec.size()); // issue: needs fix for Subset class
          if constexpr (isBorrowSEXP::value) {
            if (d.todelete) {
              d.moveit(temp); 
            } else {
              this->d = temp; 
            }
          } else {
            this->d = temp; 
          }
      } else {
          for(size_t i = 0; i < other_vec.size(); i++) d[i] = other_vec[i];  
        }
      } else if constexpr (isBorrowSEXP::value){
        Buffer<T> temp;
        temp.resize(other_vec.size());
        for(size_t i = 0; i < other_vec.size(); i++) temp[i] = other_vec[i];
        d.resize(other_vec.size());
        if (d.todelete) {
          d.moveit(temp); 
        } else {
          this->d = temp; 
        }
        d.setMatrix(other_vec.d.mp);
      } else {
        Buffer<T> temp;
        temp.resize(other_vec.size());
        for(size_t i = 0; i < other_vec.size(); i++) temp[i] = other_vec[i];
        d.resize(other_vec.size());
        d.moveit(temp); 
        d.setMatrix(other_vec.d.mp);
      }
    } else {
      static_assert(!isUnaryOPInput::value && !isBinaryOPInput::value, "Cannot assign this type");
    }
  	/*
    if constexpr(!isBuffer::value) {
        if(d.ptr -> ref_counter > 1) { 
            ass(d.size() <= other_vec.size(), "number of items to replace is not a multiple of replacement length");
            R temp;
            temp.resize(d.size());
            for (int i = 0; i < temp.size(); i++) {
              temp[i] = other_vec[i];
            }
            for (std::size_t i = 0; i < d.size(); i++) {
              this->d[i] = temp[i];
            }
            d.ptr -> ref_counter = 0;
        } else {
          for (int i = 0; i < d.size(); i++) {
              this -> d[i] = other_vec[i];
          }
        }
    } else if constexpr({
        R temp;
        temp.resize(other_vec.size());
        for (int i = 0; i < temp.size(); i++) {
          temp[i] = other_vec[i];
        }
        if (d.size() != other_vec.size()) { 
          d.resize(other_vec.size());       
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
    */
  	return *this;
  }

};

}

#endif
  /*
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
  VEC &operator=(const T &dob) {
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

  template <typename T2, typename R2>
  VEC &operator=(const VEC<T2, R2> &other_vec) {

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
        d.moveit(temp); // currently not working but why?????
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
*/
