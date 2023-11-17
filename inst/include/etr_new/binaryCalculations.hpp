#ifndef BINARYOPERATION
#define BINARYOPERATION

#include "UtilsTraits.hpp"

namespace etr {

template <typename L, typename R, binaryFct f, typename Trait = BinaryTrait,
          typename CTrait = BinaryTrait>
struct BinaryOperation {
  using TypeTrait = Trait;
  using CaseTrait = BinaryTrait;
  const L &l;
  const R &r;
  MatrixParameter mp;
  bool im() const { return mp.im(); }
  size_t nc() const { return mp.nc(); }
  size_t nr() const { return mp.nr(); }
  BinaryOperation(const BinaryOperation &other)
      : l(other.l), r(other.r), mp(other.mp) {}
  BinaryOperation(const BinaryOperation &&other)
      : l(std::move(other.l)), r(std::move(other.r)), mp(std::move(other.mp)) {}
  BinaryOperation(const L &l_, const R &r_) : l(l_), r(r_) {}
  template <typename LType, typename RType, binaryFct fOther,
            typename TraitOther>
  BinaryOperation(const BinaryOperation<LType, RType, fOther, TraitOther>
                      &other) // issue: needs move constructor
      : l(other.l), r(other.r) {}
  double operator[](size_t i) const {
    constexpr bool isDoubleL = std::is_same_v<L, double>;
    constexpr bool isDoubleR = std::is_same_v<R, double>;
    if constexpr (isDoubleL && isDoubleR) {
      return f(l, r);
    } else if constexpr (!isDoubleL && isDoubleR) {
      return f(l[i % l.size()], r);
    } else if constexpr (isDoubleL && !isDoubleR) {
      return f(l, r[i % r.size()]);
    } else if constexpr (!isDoubleL && !isDoubleR) {
      return f(l[i % l.size()], r[i % r.size()]);
    }
  }
  size_t size() const {
    constexpr bool isDoubleL = std::is_same_v<L, double>;
    constexpr bool isDoubleR = std::is_same_v<R, double>;
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
};

template <typename L, typename R>
auto operator+(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           Addition, PlusTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    return Vec<double, BinaryOperation<double, double, Addition, PlusTrait>>(
        BinaryOperation<double, double, Addition, PlusTrait>(l, r));
  } else if constexpr (!isDoubleL && isDoubleR) {
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Addition, PlusTrait>>(
        BinaryOperation<decltype(l.d), double, Addition, PlusTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Addition, PlusTrait>>(
        BinaryOperation<double, decltype(r.d), Addition, PlusTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Addition,
                                       PlusTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Addition, PlusTrait>(
            l.d, r.d));
  }
}

template <typename L, typename R>
auto operator-(const L &l, const R &r)
    -> Vec<double, BinaryOperation<decltype(convert(l).d),
                                   decltype(convert(r).d), Minus, MinusTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    return Vec<double, BinaryOperation<double, double, Minus, MinusTrait>>(
        BinaryOperation<double, double, Minus, MinusTrait>(l, r));
  } else if constexpr (!isDoubleL && isDoubleR) {
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Minus, MinusTrait>>(
        BinaryOperation<decltype(l.d), double, Minus, MinusTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Minus, MinusTrait>>(
        BinaryOperation<double, decltype(r.d), Minus, MinusTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Minus,
                                       MinusTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Minus, MinusTrait>(l.d,
                                                                         r.d));
  }
}

template <typename L, typename R>
auto operator*(const L &l, const R &r)
    -> Vec<double, BinaryOperation<decltype(convert(l).d),
                                   decltype(convert(r).d), Times, TimesTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    return Vec<double, BinaryOperation<double, double, Times, TimesTrait>>(
        BinaryOperation<double, double, Times, TimesTrait>(l, r));
  } else if constexpr (!isDoubleL && isDoubleR) {
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Times, TimesTrait>>(
        BinaryOperation<decltype(l.d), double, Times, TimesTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Times, TimesTrait>>(
        BinaryOperation<double, decltype(r.d), Times, TimesTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Times,
                                       TimesTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Times, TimesTrait>(l.d,
                                                                         r.d));
  }
}

template <typename L, typename R>
auto operator/(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           Divide, DivideTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    return Vec<double, BinaryOperation<double, double, Divide, DivideTrait>>(
        BinaryOperation<double, double, Divide, DivideTrait>(l, r));
  } else if constexpr (!isDoubleL && isDoubleR) {
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Divide, DivideTrait>>(
        BinaryOperation<decltype(l.d), double, Divide, DivideTrait>(l.d, r));
  } else if constexpr (isDoubleL && !isDoubleR) {
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Divide, DivideTrait>>(
        BinaryOperation<double, decltype(r.d), Divide, DivideTrait>(l, r.d));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Divide,
                                       DivideTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Divide, DivideTrait>(
            l.d, r.d));
  }
}

} // namespace etr

#endif