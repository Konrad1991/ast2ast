#ifndef TRAITS_ETR_H
#define TRAITS_ETR_H

#include "Header.hpp"

namespace etr {
struct DoubleTrait {};
struct IntTrait {};
struct BoolTrait {};
struct ComparisonTrait {
  using RetType = bool;
};

struct BaseStoreTrait {};
struct BufferTrait {};
struct VectorTrait {};
struct VariableTrait {};
struct SubsetTrait {};
struct BorrowTrait {};
struct BorrowSEXPTrait {};
struct RVecTrait {};
struct SubVecTrait {};
struct RBufTrait {};

struct UnaryTrait {
  using RetType = BaseType;
};
struct BinaryTrait {
  using RetType = BaseType;
};
struct QuarternaryTrait {
  using RetType = BaseType;
};

typedef double (*quaternaryFct)(double, double, double, double);
inline double TimesDeriv(double l, double r, double lDeriv, double rDeriv) {
  return l * rDeriv + r * lDeriv;
}
inline double PlusDeriv(double lDeriv, double rDeriv) {
  return lDeriv + rDeriv;
}

struct PlusDerivTrait {
  template <typename L, typename R>
  static inline std::common_type<L, R>::type f(L l, R r) {
    return l + r;
  }

  template <typename L, typename R>
  static inline std::common_type<L, R>::type fDeriv(L l, R r) {
    return l + r;
  }
};

struct MinusDerivTrait {
  template <typename L, typename R>
  static inline std::common_type<L, R>::type f(L l, R r) {
    return l - r;
  }

  template <typename L, typename R>
  static inline std::common_type<L, R>::type fDeriv(L l, R r) {
    return l - r;
  }
};

struct TimesDerivTrait {
  template <typename L, typename R>
  static inline std::common_type<L, R>::type f(L l, R r) {
    return l * r;
  }

  template <typename L, typename R, typename LDeriv, typename RDeriv>
  static inline std::common_type<L, R>::type fDeriv(L l, R r, LDeriv ld,
                                                    RDeriv rd) {
    return ld * r + rd * l;
  }
};

struct DivideDerivTrait {
  template <typename L = BaseType, typename R = BaseType>
  static inline auto f(L l, R r) {
    if constexpr (std::is_integral_v<L> && std::is_integral_v<R>) {
      return static_cast<BaseType>(static_cast<BaseType>(l) /
                                   static_cast<BaseType>(r));
    } else if constexpr (std::is_integral_v<L> && !std::is_integral_v<R>) {
      return static_cast<BaseType>(l) / r;
    } else if constexpr (!std::is_integral_v<L> && std::is_integral_v<R>) {
      return l / static_cast<BaseType>(r);
    } else {
      static_assert(std::is_arithmetic_v<L> && std::is_arithmetic_v<R>,
                    "Type not supported by operation /");
      return l / r;
    }
  }

  template <typename L, typename R, typename LDeriv, typename RDeriv>
  static inline std::common_type<L, R>::type fDeriv(L l, R r, LDeriv ld,
                                                    RDeriv rd) {
    return (ld * r - l * rd) /
           std::pow(rd, 2); // TODO: add check for integral ...
  }
};

struct DivideByConstantDerivTrait {
  template <typename L = BaseType, typename R = BaseType>
  static inline auto f(L l, R r) {
    if constexpr (std::is_integral_v<L> && std::is_integral_v<R>) {
      return static_cast<BaseType>(static_cast<BaseType>(l) /
                                   static_cast<BaseType>(r));
    } else if constexpr (std::is_integral_v<L> && !std::is_integral_v<R>) {
      return static_cast<BaseType>(l) / r;
    } else if constexpr (!std::is_integral_v<L> && std::is_integral_v<R>) {
      return l / static_cast<BaseType>(r);
    } else {
      static_assert(std::is_arithmetic_v<L> && std::is_arithmetic_v<R>,
                    "Type not supported by operation /");
      return l / r;
    }
  }

  template <typename L, typename R, typename LDeriv, typename RDeriv>
  static inline std::common_type<L, R>::type fDeriv(L l, R r, LDeriv ld,
                                                    RDeriv rd) {
    return ld / r;
  }
};

struct PowDerivTrait {
  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type f(L l, R r) {
    return std::pow(l, r);
  }

  template <typename L, typename R, typename LDeriv, typename RDeriv>
  static inline std::common_type<L, R>::type fDeriv(L l, R r, LDeriv ld,
                                                    RDeriv rd) {
    return r * std::pow(l, r - 1) * ld;
  }
};

struct EqualDerivTrait {
  template <typename L = BaseType, typename R = BaseType>
  static inline bool
  f(L a,
    R b) { // TODO: add this to documentationion for package authors
    if (fabs(a - b) < 1E-3) {
      return true;
    } else {
      return false;
    }
  }

  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type fDeriv(L l, R r) {
    return false;
  }
};
struct SmallerDerivTrait {
  template <typename L = BaseType, typename R = BaseType>
  static bool f(L a, R b) {
    if (a < b) {
      return true;
    } else {
      return false;
    }
  }
  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type fDeriv(L l, R r) {
    return false;
  }
};
struct SmallerEqualDerivTrait {
  template <typename L = BaseType, typename R = BaseType>
  static bool f(L a, R b) {
    if (a <= b) {
      return true;
    } else {
      return false;
    }
  }
  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type fDeriv(L l, R r) {
    return false;
  }
};
struct LargerDerivTrait {
  template <typename L = BaseType, typename R = BaseType>
  static bool f(L a, R b) {
    if (a > b) {
      return true;
    } else {
      return false;
    }
  }
  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type fDeriv(L l, R r) {
    return false;
  }
};
struct LargerEqualDerivTrait {
  template <typename L = BaseType, typename R = BaseType>
  static bool f(L a, R b) {
    if (a >= b) {
      return true;
    } else {
      return false;
    }
  }
  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type fDeriv(L l, R r) {
    return false;
  }
};
struct UnEqualDerivTrait {
  template <typename L = BaseType, typename R = BaseType>
  static bool f(L a, R b) {
    if (fabs(a - b) > 1E-3) {
      return true;
    } else {
      return false;
    }
  }
  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type fDeriv(L l, R r) {
    return false;
  }
};

struct SinusDerivTrait {
  template <typename L> static inline L f(L l) { return sin(l); }
  template <typename L> static inline L fDeriv(L l) { return cos(l); }
};

struct PlusTrait {
  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type f(L l, R r) {
    return l + r;
  }
};

struct MinusTrait {
  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type f(L l, R r) {
    return l - r;
  }
};
struct TimesTrait {
  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type f(L l, R r) {
    return l * r;
  }
};
struct DivideTrait {
  template <typename L = BaseType, typename R = BaseType>
  static inline auto f(L l, R r) {
    if constexpr (std::is_integral_v<L> && std::is_integral_v<R>) {
      return static_cast<BaseType>(static_cast<BaseType>(l) /
                                   static_cast<BaseType>(r));
    } else if constexpr (std::is_integral_v<L> && !std::is_integral_v<R>) {
      return static_cast<BaseType>(l) / r;
    } else if constexpr (!std::is_integral_v<L> && std::is_integral_v<R>) {
      return l / static_cast<BaseType>(r);
    } else {
      static_assert(std::is_arithmetic_v<L> && std::is_arithmetic_v<R>,
                    "Type not supported by operation /");
      return l / r;
    }
  }
};
struct PowTrait {
  template <typename L = BaseType, typename R = BaseType>
  static inline std::common_type<L, R>::type f(L l, R r) {
    return std::pow(l, r);
  }
};
struct EqualTrait {
  template <typename L = BaseType, typename R = BaseType>
  static inline bool
  f(L a,
    R b) { // issue: add this to documentationion for package authors
    if (fabs(a - b) < 1E-3) {
      return true;
    } else {
      return false;
    }
  }
};
struct SmallerTrait {
  template <typename L = BaseType, typename R = BaseType>
  static bool f(L a, R b) {
    if (a < b) {
      return true;
    } else {
      return false;
    }
  }
};
struct SmallerEqualTrait {
  template <typename L = BaseType, typename R = BaseType>
  static bool f(L a, R b) {
    if (a <= b) {
      return true;
    } else {
      return false;
    }
  }
};
struct LargerTrait {
  template <typename L = BaseType, typename R = BaseType>
  static bool f(L a, R b) {
    if (a > b) {
      return true;
    } else {
      return false;
    }
  }
};
struct LargerEqualTrait {
  template <typename L = BaseType, typename R = BaseType>
  static bool f(L a, R b) {
    if (a >= b) {
      return true;
    } else {
      return false;
    }
  }
};
struct UnEqualTrait {
  template <typename L = BaseType, typename R = BaseType>
  static bool f(L a, R b) {
    if (fabs(a - b) > 1E-3) {
      return true;
    } else {
      return false;
    }
  }
};

struct SinusTrait {
  template <typename L = BaseType> static inline auto f(L a) { return sin(a); }
};
struct ASinusTrait {
  template <typename L = BaseType> static inline auto f(L a) { return asin(a); }
};
struct SinusHTrait {
  template <typename L = BaseType> static inline auto f(L a) { return sinh(a); }
};
struct CosinusTrait {
  template <typename L = BaseType> static inline auto f(L a) { return cos(a); }
};
struct ACosinusTrait {
  template <typename L = BaseType> static inline auto f(L a) { return acos(a); }
};
struct CosinusHTrait {
  template <typename L = BaseType> static inline auto f(L a) { return cosh(a); }
};
struct TangensTrait {
  template <typename L = BaseType> static inline auto f(L a) { return tan(a); }
};
struct ATangensTrait {
  template <typename L = BaseType> static inline auto f(L a) { return atan(a); }
};
struct TangensHTrait {
  template <typename L = BaseType> static inline auto f(L a) { return tanh(a); }
};
struct ExpTrait {
  template <typename L = BaseType> static inline auto f(L a) { return exp(a); }
};
struct LogTrait {
  template <typename L = BaseType> static inline auto f(L a) { return log(a); }
};
struct SquareRootTrait {
  template <typename L = BaseType> static inline auto f(L a) { return sqrt(a); }
};
struct MinusUnaryTrait {
  template <typename L = BaseType> static inline auto f(L a) { return -a; }
};

struct VarPointerTrait {};
struct VariableTypeTrait {};
struct ConstantTypeTrait {};

} // namespace etr

#endif
