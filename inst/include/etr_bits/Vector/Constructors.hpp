#ifndef CONSTRUCTORS_H
#define CONSTRUCTORS_H

#include "VectorClass.hpp"

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
  requires std::is_same_v<U, Borrow<BaseType>>
explicit Vec(const Borrow<T2> &&borrowed) : d(borrowed), deriv(borrowed.sz) {
  d.setMatrix(borrowed.mp);
}
template <typename U = R, typename T2>
  requires std::is_same_v<U, Borrow<T>>
explicit Vec(const Borrow<T2> &borrowed) : d(borrowed), deriv(borrowed.sz) {
  d.setMatrix(borrowed.mp);
}
template <typename U = R>
  requires std::is_same_v<U, Borrow<T>>
explicit Vec(T *ptr, std::size_t s) : d(ptr, s), deriv(s) {}

// NOTE: BorrowSEXP
#ifdef STANDALONE_ETR
#else
template <typename U = R>
  requires std::is_same_v<U, BorrowSEXP<T>>
explicit Vec(SEXP &&inp) = delete;

template <typename U = R>
  requires std::is_same_v<U, BorrowSEXP<T>>
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
template <typename L2, typename R2, typename TraitL>
  requires std::is_same_v<TraitL, SubsetClassTrait>
explicit Vec(const SubsetClass<L2, R2, TraitL> &&inp)
    : d(inp), deriv(inp.size()) {
  d.setMatrix(inp.mp);
}
template <typename L2, typename R2, typename TraitL>
  requires std::is_same_v<TraitL, SubsetClassTrait>
explicit Vec(SubsetClass<L2, R2, TraitL> &inp) : d(inp), deriv(inp.size()) {
  d.setMatrix(inp.mp);
}

// NOTE: Binary operation
template <typename L2, typename R2, typename OperationTrait>
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
  requires(IsRVec<const Vec<T2, R2, Trait2>> && !std::is_same_v<T, T2>)
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
  requires(IsRVec<const Vec<T2, R2, Trait2>> && std::is_same_v<T, T2>)
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
  requires std::is_same_v<U, Borrow<BaseType>>
explicit Vec(const Borrow<T2> &&borrowed) : d(borrowed) {
  d.setMatrix(borrowed.mp);
}
template <typename U = R, typename T2>
  requires std::is_same_v<U, Borrow<T>>
explicit Vec(const Borrow<T2> &borrowed) : d(borrowed) {
  d.setMatrix(borrowed.mp);
}
template <typename U = R>
  requires std::is_same_v<U, Borrow<T>>
explicit Vec(T *ptr, std::size_t s) : d(ptr, s) {}

// NOTE: BorrowSEXP
#ifdef STANDALONE_ETR
#else
template <typename U = R>
  requires std::is_same_v<U, BorrowSEXP<T>>
explicit Vec(SEXP &&inp) = delete;

template <typename U = R>
  requires std::is_same_v<U, BorrowSEXP<T>>
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
  requires std::is_same_v<TraitL, SubsetClassTrait>
explicit Vec(const SubsetClass<L2, R2, TraitL> &&inp) : d(std::move(inp)) {
  d.setMatrix(inp.mp);
}
template <typename L2, typename R2, typename TraitL>
  requires std::is_same_v<TraitL, SubsetClassTrait>
explicit Vec(SubsetClass<L2, R2, TraitL> &inp) : d(std::move(inp)) {
  d.setMatrix(inp.mp);
}

// NOTE: Binary operation
template <typename L2, typename R2, typename OperationTrait>
explicit Vec(const BinaryOperation<L2, R2, OperationTrait> &&inp) : d(inp) {
  using TypeTrait = OperationTrait;
  d.setMatrix(inp.mp);
}
template <typename L2, typename R2, typename OperationTrait,
          typename DetailTrait> // NOTE: only for comparison!
explicit Vec(const BinaryOperation<L2, R2, OperationTrait, DetailTrait> &&inp)
    : d(inp) {
  using TypeTrait = OperationTrait;
  d.setMatrix(inp.mp);
}
template <typename L2, typename R2, typename OperationTrait>
explicit Vec(BinaryOperation<L2, R2, OperationTrait> &inp) : d(inp) {
  using TypeTrait = OperationTrait;
  d.setMatrix(inp.mp);
}

// NOTE: Unary operation
template <typename L2, typename OperationTrait>
explicit Vec(const UnaryOperation<L2, OperationTrait> &&inp) : d(inp) {
  using TypeTrait = OperationTrait;
  d.setMatrix(inp.mp);
}
template <typename L2, typename OperationTrait>
explicit Vec(UnaryOperation<L2, OperationTrait> &inp) : d(inp) {
  using TypeTrait = OperationTrait;
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
}

// NOTE: other vector which is of type RVec and has different base type
template <typename T2, typename R2, typename Trait2>
  requires(IsRVec<const Vec<T2, R2, Trait2>> && !std::is_same_v<T, T2>)
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
}

// NOTE: other vector which is of type RVec and with same base type
template <typename T2, typename R2, typename Trait2>
  requires(IsRVec<const Vec<T2, R2, Trait2>> && std::is_same_v<T, T2>)
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
}

// NOTE: pointer constructor
Vec(T *ptr, std::size_t size) : d(size) {
  if constexpr (isBorrow::value) {
    d.init(ptr, size);
  } else if constexpr (isBuffer::value) {
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

#endif
