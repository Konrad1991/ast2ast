#ifndef BINARYOPERATION
#define BINARYOPERATION

#include "UtilsTraits.hpp"

namespace etr {

template <typename L, typename R, binaryFct f, typename Trait = BinaryTrait,
          typename CTrait = BinaryTrait>
struct BinaryOperation {
  using Type = DoubleTrait;
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
  BinaryOperation(const L &l_, const R &r_, const MatrixParameter& mp_) : l(l_), r(r_), mp(mp_) {}
  template <typename LType, typename RType, binaryFct fOther,
            typename TraitOther>
  BinaryOperation(const BinaryOperation<LType, RType, fOther, TraitOther>
                      &other) // issue: needs move constructor
      : l(other.l), r(other.r), mp(other.mp) {}
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
   void setMatrix(bool i, size_t nrow, size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }
  void setMatrix(MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
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
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, Addition, PlusTrait>>(
        BinaryOperation<double, double, Addition, PlusTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Addition, PlusTrait>>(
        BinaryOperation<decltype(l.d), double, Addition, PlusTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Addition, PlusTrait>>(
        BinaryOperation<double, decltype(r.d), Addition, PlusTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Addition,
                                       PlusTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Addition, PlusTrait>(
            l.d, r.d, mp));
  }
}

template <typename L, typename R>
auto operator-(const L &l, const R &r)
    -> Vec<double, BinaryOperation<decltype(convert(l).d),
                                   decltype(convert(r).d), Minus, MinusTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, Minus, MinusTrait>>(
        BinaryOperation<double, double, Minus, MinusTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Minus, MinusTrait>>(
        BinaryOperation<decltype(l.d), double, Minus, MinusTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Minus, MinusTrait>>(
        BinaryOperation<double, decltype(r.d), Minus, MinusTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Minus,
                                       MinusTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Minus, MinusTrait>(l.d,
                                                                         r.d, mp));
  }
}

template <typename L, typename R>
auto operator*(const L &l, const R &r)
    -> Vec<double, BinaryOperation<decltype(convert(l).d),
                                   decltype(convert(r).d), Times, TimesTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, Times, TimesTrait>>(
        BinaryOperation<double, double, Times, TimesTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Times, TimesTrait>>(
        BinaryOperation<decltype(l.d), double, Times, TimesTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Times, TimesTrait>>(
        BinaryOperation<double, decltype(r.d), Times, TimesTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Times,
                                       TimesTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Times, TimesTrait>(l.d,
                                                                         r.d, mp));
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
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, Divide, DivideTrait>>(
        BinaryOperation<double, double, Divide, DivideTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Divide, DivideTrait>>(
        BinaryOperation<decltype(l.d), double, Divide, DivideTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Divide, DivideTrait>>(
        BinaryOperation<double, decltype(r.d), Divide, DivideTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Divide,
                                       DivideTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Divide, DivideTrait>(
            l.d, r.d, mp));
  }
}

template <typename L, typename R>
auto operator^(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           Pow, PowTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, Pow, PowTrait>>(
        BinaryOperation<double, double, Pow, PowTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Pow, PowTrait>>(
        BinaryOperation<decltype(l.d), double, Pow, PowTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Pow, PowTrait>>(
        BinaryOperation<double, decltype(r.d), Pow, PowTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Pow,
                                       PowTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Pow, PowTrait>(
            l.d, r.d, mp));
  }
}

template <typename L, typename R>
auto operator==(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           Equal, EqualTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, Equal, EqualTrait>>(
        BinaryOperation<double, double, Equal, EqualTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Equal, EqualTrait>>(
        BinaryOperation<decltype(l.d), double, Equal, EqualTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Equal, EqualTrait>>(
        BinaryOperation<double, decltype(r.d), Equal, EqualTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Equal,
                                       EqualTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Equal, EqualTrait>(
            l.d, r.d, mp));
  }
}

template <typename L, typename R>
auto operator!=(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           UnEqual, UnEqualTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, UnEqual, UnEqualTrait>>(
        BinaryOperation<double, double, UnEqual, UnEqualTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, UnEqual, UnEqualTrait>>(
        BinaryOperation<decltype(l.d), double, UnEqual, UnEqualTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), UnEqual, UnEqualTrait>>(
        BinaryOperation<double, decltype(r.d), UnEqual, UnEqualTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), UnEqual,
                                       UnEqualTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), UnEqual, UnEqualTrait>(
            l.d, r.d, mp));
  }
}

template <typename L, typename R>
auto operator>(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           Larger, LargerTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, Larger, LargerTrait>>(
        BinaryOperation<double, double, Larger, LargerTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Larger, LargerTrait>>(
        BinaryOperation<decltype(l.d), double, Larger, LargerTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Larger, LargerTrait>>(
        BinaryOperation<double, decltype(r.d), Larger, LargerTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Larger,
                                       LargerTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Larger, LargerTrait>(
            l.d, r.d, mp));
  }
}

template <typename L, typename R>
auto operator>=(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           LargerEqual, LargerEqualTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, LargerEqual, LargerEqualTrait>>(
        BinaryOperation<double, double, LargerEqual, LargerEqualTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, LargerEqual, LargerEqualTrait>>(
        BinaryOperation<decltype(l.d), double, LargerEqual, LargerEqualTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), LargerEqual, LargerEqualTrait>>(
        BinaryOperation<double, decltype(r.d), LargerEqual, LargerEqualTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), LargerEqual,
                                       LargerEqualTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), LargerEqual, LargerEqualTrait>(
            l.d, r.d, mp));
  }
}

template <typename L, typename R>
auto operator<(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           Smaller, SmallerTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, Smaller, SmallerTrait>>(
        BinaryOperation<double, double, Smaller, SmallerTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, Smaller, SmallerTrait>>(
        BinaryOperation<decltype(l.d), double, Smaller, SmallerTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), Smaller, SmallerTrait>>(
        BinaryOperation<double, decltype(r.d), Smaller, SmallerTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), Smaller,
                                       SmallerTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), Smaller, SmallerTrait>(
            l.d, r.d, mp));
  }
}

template <typename L, typename R>
auto operator<=(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           SmallerEqual, SmallerEqualTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<double, double, SmallerEqual, SmallerEqualTrait>>(
        BinaryOperation<double, double, SmallerEqual, SmallerEqualTrait>(l, r, mp));
  } else if constexpr (!isDoubleL && isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<decltype(l.d), double, SmallerEqual, SmallerEqualTrait>>(
        BinaryOperation<decltype(l.d), double, SmallerEqual, SmallerEqualTrait>(l.d, r, mp));
  } else if constexpr (isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double,
               BinaryOperation<double, decltype(r.d), SmallerEqual, SmallerEqualTrait>>(
        BinaryOperation<double, decltype(r.d), SmallerEqual, SmallerEqualTrait>(l, r.d, mp));
  } else if constexpr (!isDoubleL && !isDoubleR) {
    MatrixParameter mp; defineMatrix(l, r, mp);
    return Vec<double, BinaryOperation<decltype(l.d), decltype(r.d), SmallerEqual,
                                       SmallerEqualTrait>>(
        BinaryOperation<decltype(l.d), decltype(r.d), SmallerEqual, SmallerEqualTrait>(
            l.d, r.d, mp));
  }
}

} // namespace etr

#endif