/*
TODO:
1. write internal functions for
  - nc
  - nr
  - im
2. Make the classes as small as possible
3. Use the interal functions also in BinaryOperatipons
  - Define also a size method for binary operations. SUbsetting needs a own
*/
#include "../Core.hpp"
#include "../Core/Traits.hpp"
#include <optional>
#include <type_traits>

#ifndef LAZYSUBSETTING
#define LAZYSUBSETTING

namespace etr {

template <typename V, typename I>
void precalcVecBool(V &vec, Indices &ind, I *idx) {
  std::size_t sizeTrue = 0;
  for (std::size_t i = 0; i < vec.size(); i++)
    if ((*idx)[i % idx->size()])
      sizeTrue++;
  ind.resize(sizeTrue);
  std::size_t counter = 0;
  for (std::size_t i = 0; i < vec.size(); i++) {
    if ((*idx)[i % idx->size()]) {
      ind[counter] = i + 1;
      counter++;
    }
  }
}

template <typename L, typename R> struct SubsetClassIterator {
  L &l;
  const R &r;
  size_t index;

  SubsetClassIterator(L &l, const R &r, size_t index = 0)
      : l(l), r(r), index(index) {}

  auto &operator*() const {
    if constexpr (IsArithV<R>) {
      return l[r];
    } else if constexpr (!IsArithV<R>) {
      return l[r[index % r.size()] - 1];
    }
  }

  SubsetClassIterator &operator++() {
    ++index;
    return *this;
  }

  bool operator!=(const SubsetClassIterator &other) const {
    return index != other.index;
  }
};

template <typename L, typename R> struct SubsetClassConstantIterator {
  L &l;
  const R &r;
  size_t index;

  SubsetClassConstantIterator(L &l, const R &r, size_t index = 0)
      : l(l), r(r), index(index) {}

  auto operator*() const {
    if constexpr (IsArithV<R>) {
      return l[r];
    } else if constexpr (!IsArithV<R>) {
      return l[r[index % r.size()] - 1];
    }
  }

  SubsetClassConstantIterator &operator++() {
    ++index;
    return *this;
  }

  bool operator!=(const SubsetClassConstantIterator &other) const {
    return index != other.index;
  }
};

// NOTE: L and R are L values
template <typename L, typename R, typename Trait> struct SubsetClass {
  using RetType = typename ReRef<L>::type::RetType;
  using TypeTrait =
      Trait; // TODO: change this to Trait. This is everywhere used!
  L &l;
  const R &r;

  using typeTraitL = L;
  using typeTraitR = R;
  MatrixParameter mp;

  bool im() const { return CalcIMInternal(l, r); }
  std::size_t nc() const { return CalcNCInternal(l, r); }
  std::size_t nr() const { return CalcNRInternal(l, r); }

  SubsetClass(L &l_, const R &r_) : l(l_), r(r_) {}

  auto &operator[](std::size_t i) {
    if constexpr (IsArithV<R>) {
      if (!r)
        ass<"Access out of bound (probably empty vector)">(false);
      return l[i];
    } else {
      return l[r[i % l.size()] - 1];
    }
  }
  std::size_t size() const {
    if constexpr (IsArithV<R>) {
      return l.size();
    } else {
      return r.size();
      // TODO: Is this correct or rather l.size() > r.size() ?
      // l.size() : r.size();
    }
  }

  void setMatrix(bool i, std::size_t nrow, std::size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }
  void setMatrix(MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  auto begin() const { return SubsetClassIterator<L, R>{l, r, 0}; }
  auto end() const { return SubsetClassIterator<L, R>{l, r, this->size()}; }
};

// NOTE: L is L value and R is R value
template <typename L, typename R, typename Trait>
struct SubsetClass<L, const R, Trait> {
  using RetType = typename ReRef<L>::type::RetType;
  using TypeTrait = Trait;
  L &l;
  R r;

  using typeTraitL = L;
  using typeTraitR = R;
  MatrixParameter mp;

  bool im() const { return CalcIMInternal(l, r); }
  std::size_t nc() const { return CalcNCInternal(l, r); }
  std::size_t nr() const { return CalcNRInternal(l, r); }

  SubsetClass(L &l_, const R &r_) : l(l_), r(std::move(r_)) {}

  auto &operator[](std::size_t i) {
    if constexpr (IsArithV<R>) {
      if (!r)
        ass<"Access out of bound (probably empty vector)">(false);
      return l[i];
    } else {
      return l[r[i % l.size()] - 1];
    }
  }
  std::size_t size() const {
    if constexpr (IsArithV<R>) {
      return l.size();
    } else {
      return r.size();
    }
  }

  void setMatrix(bool i, std::size_t nrow, std::size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }
  void setMatrix(MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  auto begin() const { return SubsetClassIterator<L, R>{l, r, 0}; }
  auto end() const { return SubsetClassIterator<L, R>{l, r, this->size()}; }
};

// NOTE: L is R value and R is L or R value
template <typename L, typename R, typename Trait>
struct SubsetClass<const L, const R, Trait> {
  using RetType = typename ReRef<L>::type::RetType;
  using TypeTrait = Trait;
  const L l;
  const R r;

  using typeTraitL = L;
  using typeTraitR = R;
  MatrixParameter mp;

  bool im() const { return CalcIMInternal(l, r); }
  std::size_t nc() const { return CalcNCInternal(l, r); }
  std::size_t nr() const { return CalcNRInternal(l, r); }

  SubsetClass(const L &l_, const R &r_) : l(std::move(l_)), r(std::move(r_)) {}

  auto operator[](std::size_t i) const {
    if constexpr (IsArithV<R>) {
      if (!r)
        ass<"Access out of bound (probably empty vector)">(false);
      return l[i];
    } else {
      return l[r[i % l.size()] - 1];
    }
  }
  std::size_t size() const {
    if constexpr (IsArithV<R>) {
      return l.size();
    } else {
      return r.size();
    }
  }

  void setMatrix(bool i, std::size_t nrow, std::size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }
  void setMatrix(MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  auto begin() const { return SubsetClassIterator<L, R>{l, r, 0}; }
  auto end() const { return SubsetClassIterator<L, R>{l, r, this->size()}; }
};

/*
Var1            Var2
1  IVec            bool
2  IVec             int
3  IVec          double
4  IVec        LVecBool
5  IVec LVecIntOrDouble
6  IVec        RVecBool
7  IVec RVecIntOrDouble
1  RVec            bool
2  RVec             int
3  RVec          double
4  RVec        LVecBool
5  RVec LVecIntOrDouble
6  RVec        RVecBool
7  RVec RVecIntOrDouble
*/

// NOTE: R = int | double
template <typename L, typename R> inline auto &subsetArithmeticL(L &&l, R &&r) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  if constexpr (IsFloatingPointV<R>) {
    ass<"Negative indices are not supported">(r >= 0);
    return static_cast<RetType &>(l[static_cast<size_t>(std::floor(r)) - 1]);
  } else if constexpr (IsIntegral<R>) {
    ass<"Negative indices are not supported">(r >= 0);
    return static_cast<RetType &>(l.d[r - 1]);
  } else {
    ass<"Found unsupported arithmetic value">(false);
  }
}

template <typename L, typename R>
inline const auto &subsetArithmeticR(L &&l, R &&r) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  if constexpr (IsFloatingPointV<R>) {
    ass<"Negative indices are not supported">(r >= 0);
    return static_cast<RetType>(l[static_cast<size_t>(std::floor(r)) - 1]);
  } else if constexpr (IsIntegral<R>) {
    ass<"Negative indices are not supported">(r >= 0);
    return static_cast<RetType>(l.d[r - 1]);
  } else {
    ass<"Found unsupported arithmetic value">(false);
  }
}

template <typename L, typename R> inline auto &subsetArithmetic(L &&l, R &&r) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  constexpr bool isLArgRvalue = IsRvalue<L &&>;
  if constexpr (isLArgRvalue) {
    return subsetArithmeticR(std::forward<decltype(l)>(l),
                             std::forward<decltype(r)>(r));
  } else {
    return subsetArithmeticL(std::forward<decltype(l)>(l),
                             std::forward<decltype(r)>(r));
  }
}

template <typename L, typename R>
  requires((IsFloatingPointV<R> || IsIntegral<R>) && !IsBool<R>)
inline auto &subset_test(L &&l, R &&r) {
  static_assert(!IsArithV<Decayed<L>>,
                "\n\nYou cannot subset a scalar value\n\n");
  constexpr bool isArithmeticL = IsArithV<Decayed<L>>;
  constexpr bool isArithmeticR = IsArithV<Decayed<R>>;
  using LType = ReRef<L>;
  using RType = ReRef<R>;
  constexpr bool isLArgRvalue = IsRvalue<L &&>;
  constexpr bool isRArgRvalue = IsRvalue<R &&>;
  return subsetArithmetic(std::forward<decltype(l)>(l),
                          std::forward<Decayed<R>>(r));
}

// NOTE: R = bool
template <typename L, typename R>
inline auto subsetArithmeticBool(L &&l, R &&r) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  constexpr bool isLArgRvalue = IsRvalue<L &&>;
  if constexpr (isLArgRvalue) {
    SubsetClass<const decltype(l.d), R, SubsetClassTrait> sub(std::move(l.d),
                                                              r);
    return Vec<RetType, decltype(sub)>(std::move(sub));
  } else {
    SubsetClass<decltype(l.d), R, SubsetClassTrait> sub(l.d, r);
    return Vec<RetType, decltype(sub)>(std::move(sub));
  }
}
template <typename L> inline auto subset_test(L &&l, bool &&r) {
  static_assert(!IsArithV<Decayed<L>>,
                "\n\nYou cannot subset a scalar value\n\n");
  return subsetArithmeticBool(std::forward<decltype(l)>(l),
                              std::forward<Decayed<bool>>(r));
}

// NOTE: R = Vec<int|double>
template <typename L, typename R> inline auto subset_test(L &&l, R &&r) {
  static_assert(!IsArithV<Decayed<L>>,
                "\n\nYou cannot subset a scalar value\n\n");
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  using SubsetType =
      SubsetClass<decltype(l.d), decltype(r.d), SubsetClassTrait>;
  if constexpr (!IsRvalue<L &&> && !IsRvalue<R &&>) {
    return Vec<RetType,
               SubsetClass<decltype(l.d), decltype(r.d), SubsetClassTrait>>(
        SubsetClass<decltype(l.d), decltype(r.d), SubsetClassTrait>(l.d, r.d));
  } else if constexpr (!IsRvalue<L &&> && IsRvalue<R &&>) {
    return Vec<RetType, SubsetClass<decltype(l.d), const decltype(r.d),
                                    SubsetClassTrait>>(
        SubsetClass<decltype(l.d), const decltype(r.d), SubsetClassTrait>(l.d,
                                                                          r.d));
  } else {
    return Vec<RetType, SubsetClass<const decltype(l.d), const decltype(r.d),
                                    SubsetClassTrait>>(
        SubsetClass<const decltype(l.d), const decltype(r.d), SubsetClassTrait>(
            l.d, r.d));
  }
}

} // namespace etr

#endif
