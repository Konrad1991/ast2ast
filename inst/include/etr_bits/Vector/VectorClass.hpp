#ifndef BUFFER_VECTOR_H
#define BUFFER_VECTOR_H

#include "../Core.hpp"

#include "../Allocation.hpp"
#include "../BinaryCalculations.hpp"

#include "../SEXPConversions.hpp"
#include "../UnaryCalculations.hpp"

namespace etr {

template <typename T, typename R, typename Trait> struct Vec {
  using Type = T;
  using TypeTrait = Trait;
  using CaseTrait = Trait;
  R d;

#ifdef DERIV_ETR
  Buffer<double> deriv;
#endif
  Buffer<T> temp;
  using DType = R;
  using RetType = typename std::remove_reference<decltype(d)>::type::RetType;
  using typeTraitD =
      typename std::remove_reference<decltype(d)>::type::TypeTrait;
  using isBuffer = typename std::is_same<typeTraitD, BufferTrait>;
  using isBorrow = typename std::is_same<typeTraitD, BorrowTrait>;
  using isBorrowSEXP = typename std::is_same<typeTraitD, BorrowSEXPTrait>;
  using isSubset = typename std::is_same<typeTraitD, SubsetTrait>;
  using isVarPointer = typename std::is_same<typeTraitD, VarPointerTrait>;
  using caseTraitD =
      typename std::remove_reference<decltype(d)>::type::CaseTrait;
  using isUnaryOP = typename std::is_same<caseTraitD, UnaryTrait>;
  using isBinaryOP = typename std::is_same<caseTraitD, BinaryTrait>;
  using isRVec = typename std::is_same<CaseTrait, RVecTrait>;
  using isVariableType = typename std::is_same<CaseTrait, VariableTypeTrait>;

  RetType getRetType() const { return RetType{}; }

#include "AssignmentOperator.hpp"
#include "Constructors.hpp"

  RetType &operator[](std::size_t idx) { return d[idx]; }
  RetType operator[](std::size_t idx) const { return d[idx]; }

  operator RetType() const {
    if constexpr (std::is_same_v<RetType, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      // NOTE: otherwise
      // subsetting does not
      // work. Thus, warn
      // instead of assert
      return d[0];
    } else {
      return d[0];
    }
  }

  std::size_t size() const { return d.size(); }
  bool im() const { return d.im(); }
  std::size_t nc() const { return d.nc(); }
  std::size_t nr() const { return d.nr(); }

  auto begin() const {
    if constexpr (isSubset::value) {
      return It<T>{d.p->p};
    } else {
      return It<T>{d.p};
    }
  }
  auto end() const {
    if constexpr (isSubset::value) {
      return It<T>{d.p->p + this->size()};
    } else {
      return It<T>{d.p + this->size()};
    }
  }

  T &back() const { return d.p[this->size()]; }
  void fill(T value) { d.fill(value); }
  void resize(std::size_t newSize) { d.resize(newSize); }
  friend std::ostream &operator<<(std::ostream &os, const Vec &vec) {
    os << "Vec [ ";
    for (std::size_t i = 0; i < vec.size(); i++) {
      os << vec.d.p[i] << " ";
    }
    os << "]";
    return os;
  }

#ifdef STANDALONE_ETR
#else
  Vec &operator=(Rcpp::NumericVector &otherVec) {
    d.resize(static_cast<std::size_t>(otherVec.size()));
    d.mp.setMatrix(false, 0, 0);
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      d[i] = otherVec[i];
    }
    return *this;
  }
  Vec &operator=(Rcpp::NumericMatrix &otherVec) {
    d.resize(static_cast<std::size_t>(otherVec.size()));
    d.mp.setMatrix(true, otherVec.nrow(), otherVec.ncol());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      d[i] = otherVec[i];
    }
    return *this;
  }
  Vec &operator=(arma::vec &otherVec) {
    d.resize(static_cast<std::size_t>(otherVec.size()));
    d.mp.setMatrix(false, 0, 0);
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      d[i] = otherVec[i];
    }
    return *this;
  }
  Vec &operator=(arma::mat &otherVec) {
    d.resize(static_cast<std::size_t>(otherVec.size()));
    d.mp.setMatrix(true, otherVec.n_rows, otherVec.n_cols);
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      d[i] = otherVec[i];
    }
    return *this;
  }
  Vec(Rcpp::NumericVector otherVec) : d() {
    d.resize(static_cast<std::size_t>(otherVec.size()));
    d.mp.setMatrix(false, 0, 0);
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      d[i] = otherVec[i];
    }
  }
  Vec(arma::vec otherVec) : d() {
    d.resize(static_cast<std::size_t>(otherVec.size()));
    d.mp.setMatrix(false, 0, 0);
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      d[i] = otherVec[i];
    }
  }
  Vec(Rcpp::NumericMatrix otherVec) : d() {
    d.resize(static_cast<std::size_t>(otherVec.size()));
    d.mp.setMatrix(true, otherVec.nrow(), otherVec.ncol());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      d[i] = otherVec[i];
    }
  }
  Vec(arma::mat otherVec) : d() {
    d.resize(static_cast<std::size_t>(otherVec.size()));
    d.mp.setMatrix(true, otherVec.n_rows, otherVec.n_cols);
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      d[i] = otherVec[i];
    }
  }

  operator Rcpp::NumericVector() const {
    Rcpp::NumericVector ret(this->size());
    for (std::size_t i = 0; i < ret.size(); i++) {
      ret[i] = d[i];
    }
    return ret;
  }
  operator Rcpp::NumericMatrix() const {
    ass<"Object cannot be converted to NumericMatrix">(this->im() == true);
    Rcpp::NumericMatrix ret(this->nr(), this->nc());
    for (int i = 0; i < ret.size(); i++) {
      ret[i] = d[i];
    }
    return ret;
  }
  operator arma::vec() const {
    arma::vec ret(this->size());
    for (std::size_t i = 0; i < this->size(); i++) {
      ret[i] = d[i];
    }
    return ret;
  }
  operator arma::mat() const {
    ass<"Object cannot be converted to arma::mat">(this->im() == true);
    arma::mat ret(this->nr(), this->nc());
    for (std::size_t i = 0; i < ret.size(); i++) {
      ret[i] = d[i];
    }
    return ret;
  }
#endif

  template <typename I> auto operator()(const I &idx) {
    return subset(*this, idx);
  }

  template <typename IL, typename IR>
  auto operator()(const IL &idxL, const IR &idxR) {
    return subset(*this, idxL, idxR);
  }
};

} // namespace etr

#endif
