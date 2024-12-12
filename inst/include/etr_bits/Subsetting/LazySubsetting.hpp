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
  const L &l;
  const R &r;
  size_t index;

  SubsetClassIterator(const L &l, const R &r, size_t index = 0)
      : l(l), r(r), index(index) {}

  auto operator*() const {
    constexpr bool isArithmeticR = IsArithV<R>;
    if constexpr (isArithmeticR) {
      return l[r];
    } else if constexpr (!isArithmeticR) {
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

template <typename L, typename R, typename Trait> struct SubsetClass {
  using RetType = typename ExtractDataType<L>::RetType;
  using CaseTrait = BinaryTrait;
  using TypeTrait = Trait;
  std::optional<L> storedL;
  std::optional<R> storedR;
  const L &l;
  const R &r;

  using typeTraitL = L;
  using typeTraitR = R;
  MatrixParameter mp;

  bool im() const { return l.im() || r.im(); }
  std::size_t calculateDim() const {
    if (l.im() && r.im())
      return (l.nr() > r.nr()) ? l.nr() : r.nr();
    if (!l.im() && r.im())
      return r.nr();
    if (l.im() && !r.im())
      return l.nr();
    ass<"Matrix calculation failed!">(false);
    return 0;
  }

  std::size_t nc() const { return calculateDim(); }
  std::size_t nr() const { return calculateDim(); }

  // Constructor for lvalues (both `l_` and `r_`)
  SubsetClass(const L &l_, const R &r_) : l(l_), r(r_) {}

  // Constructor for rvalues (both `l_` and `r_`)
  SubsetClass(L &&l_, R &&r_)
      : storedL(std::move(l_)), storedR(std::move(r_)), l(*storedL),
        r(*storedR) {}

  // Constructor for lvalue `l_` and rvalue `r_`
  SubsetClass(const L &l_, R &&r_)
      : storedR(std::move(r_)), l(l_), r(*storedR) {}

  // Constructor for rvalue `l_` and lvalue `r_`
  SubsetClass(L &&l_, const R &r_)
      : storedL(std::move(l_)), l(*storedL), r(r_) {}

  SubsetClass(const SubsetClass &other) : l(other.l), r(other.r) {}
  // Move constructor
  SubsetClass(const SubsetClass &&other)
      : storedL(other.storedL ? std::move(other.storedL) : std::optional<L>{}),
        storedR(other.storedR ? std::move(other.storedR) : std::optional<R>{}),
        l(other.storedL.has_value() ? *storedL : other.l),
        r(other.storedR.has_value() ? *storedR : other.r) {}
  template <typename LType, typename RType, typename TraitL>
  SubsetClass(const SubsetClass<LType, RType, TraitL> &other)
      : l(other.l), r(other.r) {}

  auto operator[](std::size_t i) const { return l[r[i % l.size()] - 1]; }
  // TODO: it is possible to use indices larger than length of l. It is safe
  // but not the behaviour in R
  std::size_t size() const { return r.size(); }

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

// NOTE: the following subsetArithmetic, subsetBoolean, subsetVector and
// subsetSubset functions mean that a vector is subsetted by something which
// can be either a scalar (int or double), a bool, a vector or a SubsetClass
// (wrapped in a Vec).

template <typename T> using Decayed = std::decay_t<T>;

template <typename T>
constexpr bool IsFloatingPoint = std::is_floating_point_v<T>;

template <typename T> constexpr bool IsIntegral = std::is_integral_v<T>;

template <typename T> constexpr bool IsBool = std::is_same_v<T, bool>;

template <typename T> constexpr bool IsRvalue = std::is_rvalue_reference_v<T>;

template <typename L, typename R> auto subsetArithmeticR(L &&l, R &&r) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  if constexpr (IsFloatingPoint<R>) {
    return static_cast<RetType>(l[static_cast<size_t>(std::floor(r)) - 1]);
  } else if constexpr (IsBool<R>) {
    if (r) {
      return l;
    } else {
      return Vec<RetType>(); // TODO: check is empty vector always safe?
    }
  } else if constexpr (IsIntegral<R>) {
    return static_cast<RetType>(l.d[r - 1]);
  } else {
    ass<"Found unsupported arithmetic value">(false);
  }
}

template <typename L, typename R> auto subsetArithmeticL(L &&l, R &&r) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  if constexpr (IsFloatingPoint<R>) {
    return static_cast<RetType &>(l[static_cast<size_t>(std::floor(r)) - 1]);
  } else if constexpr (IsBool<R>) {
    if (r) {
      return l;
    } else {
      return Vec<RetType>(); // TODO: check is empty vector always safe?
    }
  } else if constexpr (IsIntegral<R>) {
    return static_cast<RetType &>(l.d[r - 1]);
  } else {
    ass<"Found unsupported arithmetic value">(false);
  }
}

template <typename L, typename R> auto subsetArithmetic(L &&l, R &&r) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  constexpr bool isLArgRvalue = std::is_rvalue_reference_v<L &&>;
  if constexpr (isLArgRvalue) {
    return subsetArithmeticR(std::forward<decltype(l)>(l),
                             std::forward<decltype(r)>(r));
  } else {
    return subsetArithmeticL(std::forward<decltype(l)>(l),
                             std::forward<decltype(r)>(r));
  }
}

template <typename L, typename R> auto subsetBoolean(L &&l, R &&r) {
  constexpr bool isLArgRvalue = std::is_rvalue_reference_v<L &&>;
  constexpr bool isRArgRvalue = std::is_rvalue_reference_v<R &&>;
  using dataType = typename ExtractDataType<Decayed<L>>::RetType;
  Indices ind;
  precalcVecBool(l, ind, &r);
  if constexpr (isLArgRvalue) {
    return Vec<dataType,
               SubsetClass<decltype(l.d), decltype(ind), SubsetClassTrait>>(
        SubsetClass<decltype(l.d), decltype(ind), SubsetClassTrait>(
            std::move(l.d), std::move(ind)));
  } else {
    return Vec<dataType,
               SubsetClass<decltype(l.d), decltype(ind), SubsetClassTrait>>(
        SubsetClass<decltype(l.d), decltype(ind), SubsetClassTrait>(
            l.d, std::move(ind)));
  }
}

template <typename L, typename R> auto subsetSubset(L &&l, R &&r) {
  constexpr bool isLArgRvalue = std::is_rvalue_reference_v<L &&>;
  constexpr bool isRArgRvalue = std::is_rvalue_reference_v<R &&>;
  using dataType = typename ExtractDataType<std::decay_t<L>>::RetType;
  Indices ind(r.size());
  for (std::size_t i = 0; i < r.size(); i++) {
    ind[i] = r.d[i];
  }
  if constexpr (isLArgRvalue) {
    return Vec<dataType,
               SubsetClass<decltype(l.d), decltype(ind), SubsetClassTrait>>(
        SubsetClass<decltype(l.d), decltype(ind), SubsetClassTrait>(
            std::move(l.d), std::move(ind)));
  } else {
    return Vec<dataType,
               SubsetClass<decltype(l.d), decltype(ind), SubsetClassTrait>>(
        SubsetClass<decltype(l.d), decltype(ind), SubsetClassTrait>(
            l.d, std::move(ind)));
  }
}

template <typename L, typename R> auto subsetVector(L &&l, R &&r) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  using SubsetType =
      SubsetClass<decltype(l.d), decltype(r.d), SubsetClassTrait>;
  if constexpr (IsRvalue<L &&> && IsRvalue<R &&>) {
    return subsetSubset(std::forward<L>(l), std::forward<R>(r));
  } else if constexpr (IsRvalue<L &&>) {
    return Vec<RetType, SubsetType>(SubsetType(std::move(l.d), r.d));
  } else if constexpr (IsRvalue<R &&>) {
    return Vec<RetType, SubsetType>(SubsetType(l.d, std::move(r.d)));
  } else {
    return Vec<RetType, SubsetType>(SubsetType(l.d, r.d));
  }
}

template <typename L, typename R> auto subset_test(L &&l, R &&r) {
  static_assert(!std::is_arithmetic_v<Decayed<L>>,
                "\n\nYou cannot subset a scalar value\n\n");
  constexpr bool isArithmeticL = std::is_arithmetic_v<Decayed<L>>;
  constexpr bool isArithmeticR = std::is_arithmetic_v<Decayed<R>>;
  using LType = std::remove_reference_t<L>;
  using RType = std::remove_reference_t<R>;
  constexpr bool isLArgRvalue = std::is_rvalue_reference_v<L &&>;
  constexpr bool isRArgRvalue = std::is_rvalue_reference_v<R &&>;
  if constexpr (isArithmeticR) {
    return subsetArithmetic(std::forward<decltype(l)>(l),
                            std::forward<Decayed<R>>(r));
  } else {
    using dataType = typename ExtractDataType<Decayed<L>>::RetType;
    using dataTypeR = typename ExtractDataType<Decayed<R>>::RetType;
    if constexpr (std::is_same_v<dataTypeR, bool>) {
      return subsetBoolean(std::forward<decltype(l)>(l),
                           std::forward<decltype(r)>(r));
    } else {
      return subsetVector(std::forward<decltype(l)>(l),
                          std::forward<decltype(r)>(r));
    }
  }
}

} // namespace etr

#endif
