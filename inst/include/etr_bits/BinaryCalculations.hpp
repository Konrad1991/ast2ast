#ifndef BINARYOPERATION
#define BINARYOPERATION

#include "Core.hpp"

namespace etr {

template<typename L, typename R>
inline auto determine_type_binary_op() {
  if constexpr (IsVec<L> && IsVec<R>) {
    using RetType = std::common_type_t<typename L::RetType, typename R::RetType>;
    return RetType{};
  } else if constexpr(!IsVec<L> && IsVec<R>) {
    using RetType = std::common_type_t<L, typename R::RetType>;
    return RetType{};
  } else if constexpr(IsVec<L> && !IsVec<R>) {
    using RetType = std::common_type_t<typename L::RetType, R>;
    return RetType{};
  }
}


template <typename L, typename R, typename BTrait>
struct BinaryOperation {
  using Trait = BTrait;
  // TODO: the identification of RetType misses the fact that it is bool if 
  // BTrait is part of the Comparison Traits
  using RetType = typename ReRef<decltype(determine_type_binary_op<L, R>())>;
  const L &l;
  const R &r;
  using typeTraitL = L;
  using typeTraitR = R;
  MatrixParameter mp;
  bool mpCalculated = false;

  bool im() const {
    if constexpr (IsArithV<L>) {
      return r.im();
    } else if constexpr (IsArithV<R>) {
      return l.im();
    } else if constexpr (IsArithV<L> && IsArithV<R>) {
      return false;
    } else {
      return l.im() || r.im();
    }
    return false;
  }
  std::size_t nc() const {
    if constexpr (IsArithV<L>) {
      return r.nc();
    } else if constexpr (IsArithV<R>) {
      return l.nc();
    } else if constexpr (IsArithV<L> && IsArithV<R>) {
      return 0;
    } else {
      if (l.im() && r.im()) {
        return (l.nc() > r.nc()) ? l.nc() : r.nc();
      } else if (!l.im() && r.im()) {
        return r.nc();
      } else if (l.im() && !r.im()) {
        return l.nc();
      } else {
        ass<"Matrix calculation failed!">(false);
      }
    }
    ass<"Matrix calculation failed!">(false);
    return (0);
  }
  std::size_t nr() const {
    if constexpr (IsArithV<L>) {
      return r.nr();
    } else if constexpr (IsArithV<R>) {
      return l.nr();
    } else if constexpr (IsArithV<L> && IsArithV<R>) {
      return 0;
    } else {
      if (l.im() && r.im()) {
        return (l.nr() > r.nr()) ? l.nr() : r.nr();
      } else if (!l.im() && r.im()) {
        return r.nr();
      } else if (l.im() && !r.im()) {
        return l.nr();
      } else {
        ass<"Matrix calculation failed!">(false);
      }
    }
    ass<"Matrix calculation failed!">(false);
    return (0);
  }
  // TODO: are all of those constructors required?
  // Actually only   BinaryOperation(const L &l_, const R &r_) : l(l_), r(r_) {}
  // is used.
  // BinaryOperation(const BinaryOperation &other) : l(other.l), r(other.r) {}
  // BinaryOperation(const BinaryOperation &&other)
  //     : l(std::move(other.l)), r(std::move(other.r)) {}
  BinaryOperation(const L &l_, const R &r_) : l(l_), r(r_) {}
  // template <typename LType, typename RType, typename TraitOther>
  // BinaryOperation(const BinaryOperation<LType, RType, TraitOther>
  //                     &other) // TODO: needs move constructor
  //     : l(other.l), r(other.r) {}

  auto operator[](std::size_t i) const {
    constexpr bool isDoubleL = IsArithV<L>;
    constexpr bool isDoubleR = IsArithV<R>;
    if constexpr (!isDoubleL && isDoubleR) {
      return Trait::f(l[i % l.size()], r);
    } else if constexpr (isDoubleL && !isDoubleR) {
      return Trait::f(l, r[i % r.size()]);
    } else if constexpr (!isDoubleL && !isDoubleR) {
      return Trait::f(l[i % l.size()], r[i % r.size()]);
    }
  }
  std::size_t size() const {
    constexpr bool isDoubleL = IsArithV<L>;
    constexpr bool isDoubleR = IsArithV<R>;
    if constexpr (isDoubleL && isDoubleR) {
      return 1;
    } else if constexpr (!isDoubleL && isDoubleR) {
      return l.size();
    } else if constexpr (isDoubleL && !isDoubleR) {
      return r.size();
    } else if constexpr (!isDoubleL && !isDoubleR) {
      return l.size() > r.size() ? l.size() : r.size();
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

};

template <typename L, typename R> auto operator+(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<typename std::common_type<typename ExtractDataType<L>::RetType,
                                         R>::type,
               BinaryOperation<decltype(l.d), R, PlusTrait>>(
        BinaryOperation<decltype(l.d), R, PlusTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<typename std::common_type<
                   L, typename ExtractDataType<R>::RetType>::type,
               BinaryOperation<L, decltype(r.d), PlusTrait>>(
        BinaryOperation<L, decltype(r.d), PlusTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<
        typename std::common_type<typename ExtractDataType<L>::RetType,
                                  typename ExtractDataType<R>::RetType>::type,
        BinaryOperation<decltype(l.d), decltype(r.d), PlusTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), PlusTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R> auto operator-(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<typename std::common_type<typename ExtractDataType<L>::RetType,
                                         R>::type,
               BinaryOperation<decltype(l.d), R, MinusTrait>>(
        BinaryOperation<decltype(l.d), R, MinusTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<typename std::common_type<
                   L, typename ExtractDataType<R>::RetType>::type,
               BinaryOperation<L, decltype(r.d), MinusTrait>>(
        BinaryOperation<L, decltype(r.d), MinusTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<
        typename std::common_type<typename ExtractDataType<L>::RetType,
                                  typename ExtractDataType<R>::RetType>::type,
        BinaryOperation<decltype(l.d), decltype(r.d), MinusTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), MinusTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R> auto operator*(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<typename std::common_type<typename ExtractDataType<L>::RetType,
                                         R>::type,
               BinaryOperation<decltype(l.d), R, TimesTrait>>(
        BinaryOperation<decltype(l.d), R, TimesTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<typename std::common_type<
                   L, typename ExtractDataType<R>::RetType>::type,
               BinaryOperation<L, decltype(r.d), TimesTrait>>(
        BinaryOperation<L, decltype(r.d), TimesTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<
        typename std::common_type<typename ExtractDataType<L>::RetType,
                                  typename ExtractDataType<R>::RetType>::type,
        BinaryOperation<decltype(l.d), decltype(r.d), TimesTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), TimesTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R> auto operator/(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<typename std::common_type<typename ExtractDataType<L>::RetType,
                                         R>::type,
               BinaryOperation<decltype(l.d), R, DivideTrait>>(
        BinaryOperation<decltype(l.d), R, DivideTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<typename std::common_type<
                   L, typename ExtractDataType<R>::RetType>::type,
               BinaryOperation<L, decltype(r.d), DivideTrait>>(
        BinaryOperation<L, decltype(r.d), DivideTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<
        typename std::common_type<typename ExtractDataType<L>::RetType,
                                  typename ExtractDataType<R>::RetType>::type,
        BinaryOperation<decltype(l.d), decltype(r.d), DivideTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), DivideTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R> auto power(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<typename std::common_type<typename ExtractDataType<L>::RetType,
                                         R>::type,
               BinaryOperation<decltype(l.d), R, PowTrait>>(
        BinaryOperation<decltype(l.d), R, PowTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<typename std::common_type<
                   L, typename ExtractDataType<R>::RetType>::type,
               BinaryOperation<L, decltype(r.d), PowTrait>>(
        BinaryOperation<L, decltype(r.d), PowTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<
        typename std::common_type<typename ExtractDataType<L>::RetType,
                                  typename ExtractDataType<R>::RetType>::type,
        BinaryOperation<decltype(l.d), decltype(r.d), PowTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), PowTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R> auto operator==(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<bool, BinaryOperation<decltype(l.d), R, EqualTrait>>(
        BinaryOperation<decltype(l.d), R, EqualTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<bool, BinaryOperation<L, decltype(r.d), EqualTrait>>(
        BinaryOperation<L, decltype(r.d), EqualTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<bool, BinaryOperation<decltype(l.d), decltype(r.d), EqualTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), EqualTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R> auto operator!=(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<bool, BinaryOperation<decltype(l.d), R, UnEqualTrait>>(
        BinaryOperation<decltype(l.d), R, UnEqualTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<bool, BinaryOperation<L, decltype(r.d), UnEqualTrait>>(
        BinaryOperation<L, decltype(r.d), UnEqualTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<bool,
               BinaryOperation<decltype(l.d), decltype(r.d), UnEqualTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d),

                        UnEqualTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R> auto operator>(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<bool, BinaryOperation<decltype(l.d), R, LargerTrait>>(
        BinaryOperation<decltype(l.d), R, LargerTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<bool, BinaryOperation<L, decltype(r.d), LargerTrait>>(
        BinaryOperation<L, decltype(r.d), LargerTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<bool,
               BinaryOperation<decltype(l.d), decltype(r.d), LargerTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d),

                        LargerTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R> auto operator>=(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<bool, BinaryOperation<decltype(l.d), R, LargerEqualTrait>>(
        BinaryOperation<decltype(l.d), R, LargerEqualTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<bool, BinaryOperation<L, decltype(r.d), LargerEqualTrait>>(
        BinaryOperation<L, decltype(r.d), LargerEqualTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<bool,
               BinaryOperation<decltype(l.d), decltype(r.d), LargerEqualTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d),

                        LargerEqualTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R> auto operator<(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<bool, BinaryOperation<decltype(l.d), R, SmallerTrait>>(
        BinaryOperation<decltype(l.d), R, SmallerTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<bool, BinaryOperation<L, decltype(r.d), SmallerTrait>>(
        BinaryOperation<L, decltype(r.d), SmallerTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<bool,
               BinaryOperation<decltype(l.d), decltype(r.d), SmallerTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d),

                        SmallerTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R> auto operator<=(const L &l, const R &r) {
  constexpr bool isDoubleL = IsArithV<L>;
  constexpr bool isDoubleR = IsArithV<R>;
  if constexpr (!isDoubleL && isDoubleR) {
    return Vec<bool, BinaryOperation<decltype(l.d), R, SmallerEqualTrait>>(
        BinaryOperation<decltype(l.d), R, SmallerEqualTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<bool, BinaryOperation<L, decltype(r.d), SmallerEqualTrait>>(
        BinaryOperation<L, decltype(r.d), SmallerEqualTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<
        bool, BinaryOperation<decltype(l.d), decltype(r.d), SmallerEqualTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d),

                        SmallerEqualTrait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

} // namespace etr

#endif
