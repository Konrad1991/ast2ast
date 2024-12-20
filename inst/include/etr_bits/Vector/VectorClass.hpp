#ifndef BUFFER_VECTOR_H
#define BUFFER_VECTOR_H

#include "../Core.hpp"

namespace etr {

template <typename T, typename R> struct Vec {
  using Type = T;
  R d;
#ifdef DERIV_ETR
  Buffer<double> deriv;
#endif
  Buffer<T> temp;
  using DType = R;
  using RetType = typename ReRef<decltype(d)>::type::RetType;


  template <typename T2> Vec(T2 n) = delete;

#ifdef DERIV_ETR

  // TODO: maybe for some constructors the deriv.allocated can be set to false
  // instead of allocating

  // NOTE: define Vector with specific size
  explicit Vec(SI &sz) : d(sz.sz), deriv(sz.sz) {}
  explicit Vec(SI &&sz) : d(sz.sz), deriv(sz.sz) {}

  // NOTE: Buffer
  template <typename L2>
  explicit Vec(const Buffer<L2> &&inp) : d(inp), deriv(inp) {
    d.setMatrix(inp.mp);
  }
  template <typename L2>
  explicit Vec(const Buffer<L2> &inp) : d(inp), deriv(inp) {
    d.setMatrix(inp.mp);
  }
  template <typename L2, typename TraitOther>
  explicit Vec(const Buffer<L2, TraitOther> &inp) : d(inp), deriv(inp) {
    d.setMatrix(inp.mp);
  }

  // NOTE: Borrow
  template <typename U = R, typename T2>
  requires IS<U, Borrow<BaseType>>
  explicit Vec(const Borrow<T2> &&borrowed) : d(borrowed), deriv(borrowed.sz) {
    d.setMatrix(borrowed.mp);
  }
  template <typename U = R, typename T2>
  requires IS<U, Borrow<T>>
  explicit Vec(const Borrow<T2> &borrowed) : d(borrowed), deriv(borrowed.sz) {
    d.setMatrix(borrowed.mp);
  }
  template <typename U = R>
  requires IS<U, Borrow<T>>
  explicit Vec(T *ptr, std::size_t s) : d(ptr, s), deriv(s) {}

  // NOTE: BorrowSEXP
#ifdef STANDALONE_ETR
#else
  template <typename U = R>
  requires IS<U, BorrowSEXP<T>>
  explicit Vec(SEXP &&inp) = delete;

  template <typename U = R>
  requires IS<U, BorrowSEXP<T>>
  explicit Vec(SEXP inp) : d(inp) {
    deriv.resize(d.sz);
    deriv.fill(0.0);
  }
#endif

  // NOTE: Subset
  template <typename L2>
  explicit Vec(const Subset<L2> &&inp) : d(inp), deriv(inp.size()) {
    d.setMatrix(inp.mp);
  }
  template <typename L2>
  explicit Vec(Subset<L2> &inp) : d(inp), deriv(inp.size()) {
    d.setMatrix(inp.mp);
  }
  // NOTE: Subset lazy
  template <typename L2, typename R2, typename TraitInp>
  requires IS<TraitInp, SubsetClassTrait>
  explicit Vec(const SubsetClass<L2, R2, TraitInp> &&inp)
  : d(inp), deriv(inp.size()) {
    d.setMatrix(inp.mp);
  }
  template <typename L2, typename R2, typename TraitInp>
  requires IS<TraitInp, SubsetClassTrait>
  explicit Vec(SubsetClass<L2, R2, TraitInp> &inp) : d(inp), deriv(inp.size()) {
    d.setMatrix(inp.mp);
  }

  // NOTE: Binary operation
  template <typename L2, typename R2, typename OperationTrait>
  requires IsBinaryOperation<BinaryOperation<L2, R2, OperationTrait>>
  explicit Vec(const BinaryOperation<L2, R2, OperationTrait> &&inp)
  : d(inp), deriv(inp.size()) {
    using TypeTrait = OperationTrait;
    d.setMatrix(inp.mp);
  }
  template <typename L2, typename R2, typename OperationTrait,
  typename DetailTrait> // NOTE: only for comparison!
  explicit Vec(const BinaryOperation<L2, R2, OperationTrait, DetailTrait> &&inp)
  : d(inp), deriv(inp.size()) {
    using TypeTrait = OperationTrait;
    d.setMatrix(inp.mp);
  }
  template <typename L2, typename R2, typename OperationTrait>
  explicit Vec(BinaryOperation<L2, R2, OperationTrait> &inp)
  : d(inp), deriv(inp.size()) {
    using TypeTrait = OperationTrait;
    d.setMatrix(inp.mp);
  }

  // NOTE: Unary operation
  template <typename L2, typename OperationTrait>
  explicit Vec(const UnaryOperation<L2, OperationTrait> &&inp)
  : d(inp), deriv(inp.size()) {
    using TypeTrait = OperationTrait;
    d.setMatrix(inp.mp);
  }
  template <typename L2, typename OperationTrait>
  explicit Vec(UnaryOperation<L2, OperationTrait> &inp)
  : d(inp), deriv(inp.size()) {
    using TypeTrait = OperationTrait;
    d.setMatrix(inp.mp);
  }

  // NOTE: arithmetic constructors
  explicit Vec(int sz) : d(1), deriv(1) { d[0] = static_cast<T>(sz); }
  explicit Vec(std::size_t sz) : d(1), deriv(1) { d[0] = sz; }
  Vec(double sz) : d(1), deriv(1) { d[0] = sz; }
#ifdef STANDALONE_ETR
  Vec(bool b) : d(1), deriv(1) { d[0] = static_cast<T>(b); }
#else
  // Vec(Rboolean b) : d(1), deriv(1) { d[0] = static_cast<T>(b); }
#endif

  // NOTE: empty vector
  explicit Vec() : d(), deriv() {}

  // NOTE: matrix constructors
  explicit Vec(std::size_t rows, std::size_t cols)
  : d(rows * cols), deriv(rows * cols) {
    d.setMatrix(true, rows, cols);
  }
  explicit Vec(std::size_t rows, std::size_t cols, const double value)
  : d(rows * cols), deriv(rows * cols) {
    d.setMatrix(true, rows, cols);
    d.fill(value);
  }
  // NOTE: other vector which is not of type RVec and has to be copied
  template <typename T2, typename R2, typename Trait2>
  requires IsVec<const Vec<T2, R2, Trait2>>
  Vec(const Vec<T2, R2, Trait2> &&other_vec) {
    using TypeTrait = Trait2;
    using CaseTrait = Trait2;
    if constexpr (isBorrow::value) {
      ass<"Sizes do not match">(d.sz <=
                                other_vec.size()); // FIX: is this correct?
      d.sz = other_vec.size();
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    } else {
      if (d.sz < other_vec.size()) {
        this->d.resize(other_vec.size());
      } else {
        d.sz = other_vec.size();
      }
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    }
    if (d.sz >= 1) {
      deriv.resize(d.sz);
      deriv.fill(1.0);
    }
  }

  // NOTE: other vector which is of type RVec and has different base type
  template <typename T2, typename R2, typename Trait2>
  requires(IsRVec<const Vec<T2, R2, Trait2>> && !IS<T, T2>)
  Vec(const Vec<T2, R2, Trait2> &&other_vec) {
    using TypeTrait = Trait2;
    using CaseTrait = Trait2;
    if constexpr (isBorrow::value) {
      ass<"Sizes do not match">(d.sz <= other_vec.size());
      d.sz = other_vec.size();
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    } else {
      if (d.sz < other_vec.size()) {
        this->d.resize(other_vec.size());
      } else {
        d.sz = other_vec.size();
      }
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    }
    if (d.sz >= 1) {
      deriv.resize(d.sz);
      this->indep_var = other_vec.indep_var;
      deriv.fill(0.0);
    }
  }

  // NOTE: other vector which is of type RVec and with same base type
  template <typename T2, typename R2, typename Trait2>
  requires(IsRVec<const Vec<T2, R2, Trait2>> && IS<T, T2>)
  Vec(Vec<T2, R2, Trait2> &&other_vec) {
    using TypeTrait = Trait2;
    using CaseTrait = Trait2;
    if constexpr (isBorrow::value) {
      ass<"Sizes do not match">(d.sz <= other_vec.size());
      d.sz = other_vec.size();
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    } else {
      std::size_t tempSize = other_vec.size();
      d.allocated = other_vec.d.allocated;
      other_vec.d.sz = this->size();
      d.sz = tempSize;
      T *tempP = other_vec.d.p;
      other_vec.d.p = d.p;
      d.p = tempP;
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    }
    if (d.sz >= 1) {
      deriv.resize(d.sz);
      deriv.fill(0.0);
    }
  }

  // NOTE: pointer constructor
  Vec(T *ptr, std::size_t size) : d(size), deriv(size) {
    if constexpr (isBorrow::value) {
      d.init(ptr, size);
    } else if constexpr (isBuffer::value) {
      for (std::size_t i = 0; i < size; i++)
        d[i] = ptr[i];
      d.setMatrix(false, 0, 0);
    }
    deriv.fill(0.0);
  }
  Vec(BaseType *ptr, std::size_t rows, std::size_t cols)
  : d(rows * cols), deriv(rows * cols) {
    for (std::size_t i = 0; i < d.size(); i++)
      d[i] = ptr[i];
    d.setMatrix(true, rows, cols);
    deriv.fill(0.0);
  }

#else
  // NOTE: define Vector with specific size
  explicit Vec(SI &sz) : d(sz.sz) {}
  explicit Vec(SI &&sz) : d(sz.sz) {}

  // NOTE: Buffer
  template <typename L2> explicit Vec(const Buffer<L2> &&inp) : d(inp) {
    d.setMatrix(inp.mp);
  }
  template <typename L2> explicit Vec(const Buffer<L2> &inp) : d(inp) {
    d.setMatrix(inp.mp);
  }
  template <typename L2, typename TraitOther>
  explicit Vec(const Buffer<L2, TraitOther> &inp) : d(inp) {
    d.setMatrix(inp.mp);
  }

  // NOTE: Borrow
  template <typename U = R, typename T2>
  requires IS<U, Borrow<BaseType>>
  explicit Vec(const Borrow<T2> &&borrowed) : d(borrowed) {
    d.setMatrix(borrowed.mp);
  }
  template <typename U = R, typename T2>
  requires IS<U, Borrow<T>>
  explicit Vec(const Borrow<T2> &borrowed) : d(borrowed) {
    d.setMatrix(borrowed.mp);
  }
  template <typename U = R>
  requires IS<U, Borrow<T>>
  explicit Vec(T *ptr, std::size_t s) : d(ptr, s) {}

  // NOTE: BorrowSEXP
#ifdef STANDALONE_ETR
#else
  template <typename U = R>
  requires IS<U, BorrowSEXP<T>>
  explicit Vec(SEXP &&inp) = delete;

  template <typename U = R>
  requires IS<U, BorrowSEXP<T>>
  explicit Vec(SEXP inp) : d(inp) {}
#endif

  // NOTE: Subset
  template <typename L2> explicit Vec(const Subset<L2> &&inp) : d(inp) {
    d.setMatrix(inp.mp);
  }
  template <typename L2> explicit Vec(Subset<L2> &inp) : d(inp) {
    d.setMatrix(inp.mp);
  }
  // NOTE: Subset lazy
  template <typename L2, typename R2, typename TraitL>
  requires IS<TraitL, SubsetClassTrait>
  explicit Vec(const SubsetClass<L2, R2, TraitL> &&inp) : d(std::move(inp)) {
    d.setMatrix(inp.mp);
  }
  template <typename L2, typename R2, typename TraitL>
  requires IS<TraitL, SubsetClassTrait>
  explicit Vec(SubsetClass<L2, R2, TraitL> &inp) : d(std::move(inp)) {
    d.setMatrix(inp.mp);
  }

  // NOTE: Binary operation
  template <typename L2, typename R2, typename OperationTrait>
  explicit Vec(const BinaryOperation<L2, R2, OperationTrait> &&inp) : d(inp) {
    d.setMatrix(inp.mp);
  }
  template <typename L2, typename R2, typename OperationTrait>
  explicit Vec(BinaryOperation<L2, R2, OperationTrait> &inp) : d(inp) {
    d.setMatrix(inp.mp);
  }

  // NOTE: Unary operation
  template <typename L2, typename OperationTrait>
  explicit Vec(const UnaryOperation<L2, OperationTrait> &&inp) : d(inp) {
    d.setMatrix(inp.mp);
  }
  template <typename L2, typename OperationTrait>
  explicit Vec(UnaryOperation<L2, OperationTrait> &inp) : d(inp) {
    d.setMatrix(inp.mp);
  }

  // NOTE: arithmetic constructors
  explicit Vec(int sz) : d(1) { d[0] = static_cast<T>(sz); }
  explicit Vec(std::size_t sz) : d(1) { d[0] = sz; }
  Vec(double sz) : d(1) { d[0] = sz; }
#ifdef STANDALONE_ETR
  Vec(bool b) : d(1) { d[0] = static_cast<T>(b); }
#else
  // Vec(Rboolean b) : d(1) { d[0] = static_cast<T>(b); }
#endif

  // NOTE: empty vector
  explicit Vec() : d() {}

  // NOTE: matrix constructors
  explicit Vec(std::size_t rows, std::size_t cols) : d(rows * cols) {
    d.setMatrix(true, rows, cols);
  }
  explicit Vec(std::size_t rows, std::size_t cols, const double value)
  : d(rows * cols) {
    d.setMatrix(true, rows, cols);
    d.fill(value);
  }
  // NOTE: other vector which is not of type RVec and has to be copied
  template <typename T2, typename R2, typename Trait2>
  requires IsVec<const Vec<T2, R2>>
  Vec(const Vec<T2, R2> &&other_vec) {
    if constexpr (IsBorrow<R>) {
      ass<"Sizes do not match">(d.sz <=
                                other_vec.size()); // FIX: is this correct?
      d.sz = other_vec.size();
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    } else {
      if (d.sz < other_vec.size()) {
        this->d.resize(other_vec.size());
      } else {
        d.sz = other_vec.size();
      }
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    }
  }

  // NOTE: other vector which is of type RVec and has different base type
  template <typename T2, typename R2>
  requires(IsRVec<const Vec<T2, R2>> && !IS<T, T2>)
  Vec(const Vec<T2, R2> &&other_vec) {
    if constexpr (IsBorrow<R>) {
      ass<"Sizes do not match">(d.sz <= other_vec.size());
      d.sz = other_vec.size();
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    } else {
      if (d.sz < other_vec.size()) {
        this->d.resize(other_vec.size());
      } else {
        d.sz = other_vec.size();
      }
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    }
  }

  // NOTE: other vector which is of type RVec and with same base type
  template <typename T2, typename R2>
  requires(IsRVec<const Vec<T2, R2>> && IS<T, T2>)
  Vec(Vec<T2, R2> &&other_vec) {
    if constexpr (IsBorrow<R>) {
      ass<"Sizes do not match">(d.sz <= other_vec.size());
      d.sz = other_vec.size();
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    } else {
      std::size_t tempSize = other_vec.size();
      d.allocated = other_vec.d.allocated;
      other_vec.d.sz = this->size();
      d.sz = tempSize;
      T *tempP = other_vec.d.p;
      other_vec.d.p = d.p;
      d.p = tempP;
      if (other_vec.d.im()) {
        d.setMatrix(true, other_vec.nr(), other_vec.nc());
      }
    }
  }

  // NOTE: pointer constructor
  Vec(T *ptr, std::size_t size) : d(size) {
    if constexpr (IsBorrow<R>) {
      d.init(ptr, size);
    } else if constexpr (IsLBuffer<R>) {
      for (std::size_t i = 0; i < size; i++)
        d[i] = ptr[i];
      d.setMatrix(false, 0, 0);
    }
  }
  Vec(BaseType *ptr, std::size_t rows, std::size_t cols) : d(rows * cols) {
    for (std::size_t i = 0; i < d.size(); i++)
      d[i] = ptr[i];
    d.setMatrix(true, rows, cols);
  }
#endif


// Assignments
template <typename TD>
  requires IsArithV<TD>
Vec &operator=(const TD inp) {
  static_assert(!IsUnary<R>, "Cannot assign to unary calculation");
  static_assert(!IsBinary<R>, "Cannot assign to binary calculation");
  static_assert(!IsRBuffer<R>,
                "Cannot assign to an r value. E.g. c(1, 2, 3) <- 1");
  if constexpr (is<TD, T>) {
    if constexpr (IsSubset<R>) {
      for (std::size_t i = 0; i < d.ind.size(); i++) {
        d[i] = inp;
      }
#ifdef DERIV_ETR
      for (std::size_t i = 0; i < d.ind.size(); i++) {
        deriv[i] = 0;
      }
#endif
    } else if constexpr (IsBorrow<R>) {
      d.sz = 1;
      d[0] = inp;
#ifdef DERIV_ETR
      deriv.resize(1);
      deriv[0] = 0;
#endif

    } else {
      d.resize(1);
      d[0] = inp;
#ifdef DERIV_ETR
      deriv.resize(1);
      deriv[0] = 0;
#endif
    }
    return *this;
  } else {
    if constexpr (IsSubset<R>) {
      for (std::size_t i = 0; i < d.ind.size(); i++) {
        d[i] = static_cast<T>(inp);
      }
#ifdef DERIV_ETR
      for (std::size_t i = 0; i < d.ind.size(); i++) {
        deriv[i] = 0;
      }
#endif

    } else if constexpr (IsBorrow<R>) {
      d.sz = 1;
      d[0] = static_cast<T>(inp);
#ifdef DERIV_ETR
      deriv.resize(1);
      deriv[0] = 0;
#endif

    } else {
      d.resize(1);
      d[0] = static_cast<T>(inp);
#ifdef DERIV_ETR
      deriv.resize(1);
      deriv[0] = 0;
#endif
    }
    return *this;
  }
}

Vec &operator=(const Vec<T, R> &otherVec) {
  static_assert(!IsUnary<R>, "Cannot assign to unary calculation");
  static_assert(!IsBinary<R>, "Cannot assign to binary calculation");
  static_assert(!IsRBuffer<R>,
                "Cannot assign to an r value. E.g. c(1, 2, 3) <- 1");

  using DataTypeOtherVec = typename etr::ExtractDataType<
      ReRef<decltype(otherVec)>>::RetType;
  if constexpr (IsLBuffer<R>) {
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    d.moveit(temp);
  } else if constexpr (IsBorrow<R>) {
    ass<"number of items to replace is not a multiple of replacement length">(
        otherVec.size() <= d.capacity);
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    ass<"size cannot be increased above the size of the borrowed object">(
        d.sz <= otherVec.size());
    d.sz = otherVec.size();
    for (std::size_t i = 0; i < otherVec.size(); i++)
      d[i] = temp[i];
  } else if constexpr (IsBorrowSEXP<R>) {
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    if (otherVec.size() > this->size())
      d.resize(otherVec.size());

    for (std::size_t i = 0; i < otherVec.size(); i++)
      d[i] = temp[i];

  } else if constexpr (IsSubset<R>) {
    ass<"number of items to replace is not a multiple of replacement length">(
        otherVec.size() == d.ind.size());
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    if (d.p->size() < temp.size())
      d.resize(temp.size());
    for (std::size_t i = 0; i < d.ind.size(); i++) {
      d[i % d.ind.size()] = temp[i];
    }
  }
  if (otherVec.d.im()) {
    d.setMatrix(true, otherVec.d.nr(), otherVec.d.nc());
  }

#ifdef DERIV_ETR
  // TODO: what if other Vec is subsetted
  // deriv.resize(otherVec.size());
  // for (std::size_t i = 0; i < deriv.size(); i++) {
  //   deriv[i] = otherVec.deriv[i];
  // }
  // TODO: is there something to do here?
#endif

  return *this;
}

// TODO: finish this
template <typename T2, typename R2>
  requires(IsRVec<const Vec<T2, R2>> && IS<T, T2>)
Vec &operator=(Vec<T2, R2> &&otherVec) {
  static_assert(!IsUnary<R>, "Cannot assign to unary calculation");
  static_assert(!IsBinary<R>, "Cannot assign to binary calculation");
  static_assert(!IsRBuffer<R>,
                "Cannot assign to an r value. E.g. c(1, 2, 3) <- 1");
  std::size_t tempSize = otherVec.size();
  std::size_t tempCapacity = otherVec.d.capacity;
  T *tempP = otherVec.d.p;
  bool allocated = otherVec.d.allocated;

  otherVec.d.allocated = d.allocated;
  otherVec.d.sz = this->size();
  otherVec.d.capacity = d.capacity;
  otherVec.d.p = d.p;

  d.allocated = allocated;
  d.sz = tempSize;
  d.capacity = tempCapacity;
  d.p = tempP;

  if (otherVec.d.im()) {
    d.setMatrix(true, otherVec.nr(), otherVec.nc());
  }
#ifdef DERIV_ETR
  deriv.resize(this->size());
  for (std::size_t i = 0; i < deriv.size(); i++) {
    deriv[i] = 0;
  }
#endif

  return *this;
}

template <typename T2, typename R2>
Vec &operator=(const Vec<T2, R2> &otherVec) {
  static_assert(!IsUnary<R>, "Cannot assign to unary calculation");
  static_assert(!IsBinary<R>, "Cannot assign to binary calculation");
  static_assert(!IsRBuffer<R>,
                "Cannot assign to an r value. E.g. c(1, 2, 3) <- 1");
    // TODO: replace is with IS
  using DataTypeOtherVec = typename ReRef<decltype(otherVec.d)>::type::RetType;
  if constexpr (IsLBuffer<R>) {

    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    d.moveit(temp);
  } else if constexpr (IsBorrow<R>) {
    ass<"number of items to replace is not a multiple of replacement length">(
        otherVec.size() <= d.capacity);
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    ass<"size cannot be increased above the size of the borrowed object">(
        d.sz <= otherVec.size());
    d.sz = otherVec.size();
    for (std::size_t i = 0; i < otherVec.size(); i++)
      d[i] = temp[i];
  } else if constexpr (IsBorrowSEXP<R>) {
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    if (otherVec.size() > this->size())
      d.resize(otherVec.size());

    for (std::size_t i = 0; i < otherVec.size(); i++)
      d[i] = temp[i];

  } else if constexpr (IsSubset<R>) {
    ass<"number of items to replace is not a multiple of replacement length">(
        otherVec.size() == d.ind.size());
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    for (std::size_t i = 0; i < d.ind.size(); i++) {
      d[i % d.ind.size()] = temp[i];
    }
  }
  if (otherVec.d.im()) {
    d.setMatrix(true, otherVec.d.nr(), otherVec.d.nc());
  }

#ifdef DERIV_ETR
  if constexpr (IsRVec<const Vec<T2, R2, Trait2>> ||
                Operation<decltype(otherVec.d)>) {
    deriv.resize(this->size());
    // for (std::size_t i = 0; i < deriv.size(); i++) {
    //   deriv[i] = 0;
    // }
  } else {
    // deriv.resize(otherVec.size());
    // for (std::size_t i = 0; i < deriv.sz; i++) {
    //   deriv[i] = otherVec.deriv[i];
    // }
  }
#endif

  return *this;
}

#ifdef STANDALONE_ETR
#else
Vec &operator=(SEXP s) {
  d.initSEXP(s);
#ifdef DERIV_ETR
  deriv.resize(d.sz);
  deriv.fill(0.0);
#endif
  return *this;
}
#endif



  RetType &operator[](std::size_t idx) { return d[idx]; }
  RetType operator[](std::size_t idx) const { return d[idx]; }

  operator RetType() const {
    if constexpr (IS<RetType, bool>) {
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
    if constexpr (IsSubset<R>) {
      return It<T>{d.p->p};
    } else if constexpr (IsSubsetClass<R>) {
      return d.begin();
    } else {
      return It<T>{d.p};
    }
  }
  auto end() const {
    if constexpr (IsSubset<R>) {
      return It<T>{d.p->p + this->size()};
    } else if constexpr (IsSubsetClass<R>) {
      return d.end();
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

};

} // namespace etr

#endif
