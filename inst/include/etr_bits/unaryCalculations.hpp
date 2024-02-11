#ifndef UNARYOPERATION
#define UNARYOPERATION

#include "UtilsTraits.hpp"

namespace etr {
template <typename I, UnaryFct f, typename Trait, typename CTrait>
struct UnaryOperation {
  using RetType = typename CTrait::RetType;
  using TypeTrait = Trait;
  using CaseTrait = CTrait;
  const I &obj;
  using typeTraitObj = I;
  MatrixParameter mp;
  bool im() const { return mp.im(); }
  size_t nc() const { return mp.nc(); }
  size_t nr() const { return mp.nr(); }
  UnaryOperation(const UnaryOperation &other) : obj(other.obj), mp(other.mp) {}
  UnaryOperation(const UnaryOperation &&other) : obj(other.obj), mp(other.mp) {}
  UnaryOperation(const I &obj_, const MatrixParameter &mp)
      : obj(obj_), mp(mp) {}
  template <typename IType, UnaryFct fOther, typename TraitOther>
  UnaryOperation(const UnaryOperation<IType, fOther, TraitOther> &other)
      : obj(other.obj), mp(other.mp) {}
  RetType operator[](size_t i) const {
    constexpr bool isDouble = std::is_same_v<I, double>;
    if constexpr (isDouble) {
      return f(obj);
    } else if constexpr (!isDouble) {
      return f(obj[i % obj.size()]);
    }
  }
  size_t size() const {
    constexpr bool isDouble = std::is_same_v<I, double>;
    if constexpr (isDouble) {
      return 1;
    } else if constexpr (!isDouble) {
      return obj.size();
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

  template <typename AV> static RetType getSize(AV &av) {
    using Ty = typename std::remove_reference<typeTraitObj>::type;
    return Ty::template getSize<AV>(av);
  }

  template <typename AV> static RetType getVal(AV &av, size_t VecIdx) {
      using Ty = typename std::remove_reference<typeTraitObj>::type;
      return f(Ty::template getVal<AV>(av, VecIdx % Ty::template getSize(av)) ); 
  }

};


template <typename T>
auto operator-(const T &obj)
    -> Vec<
        BaseType,
        UnaryOperation<decltype(convert(obj).d), MinusUnary, MinusUnaryTrait>,
        UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, BaseType>;
  if constexpr (!isDouble) {
    return Vec<BaseType,
               UnaryOperation<decltype(obj.d), MinusUnary, MinusUnaryTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), MinusUnary, MinusUnaryTrait>(obj.d,
                                                                     obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<BaseType, UnaryOperation<BaseType, MinusUnary, MinusUnaryTrait>>(
        UnaryOperation<BaseType, MinusUnary, MinusUnaryTrait>{obj});
  }
}

template <typename T>
auto sinus(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), Sinus, SinusTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), Sinus, SinusTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), Sinus, SinusTrait>(obj.d, obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, Sinus, SinusTrait>>(
        UnaryOperation<double, Sinus, SinusTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto sinus(const T &obj) -> BaseType {
  return Sinus(obj);
}

template <typename T>
auto sinush(const T &obj)
    -> Vec<double,
           UnaryOperation<decltype(convert(obj).d), SinusH, SinusHTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), SinusH, SinusHTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), SinusH, SinusHTrait>(obj.d, obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, SinusH, SinusHTrait>>(
        UnaryOperation<double, SinusH, SinusHTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto sinush(const T &obj) -> BaseType {
  return SinusH(obj);
}

template <typename T>
auto asinus(const T &obj)
    -> Vec<double,
           UnaryOperation<decltype(convert(obj).d), ASinus, ASinusTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), ASinus, ASinusTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), ASinus, ASinusTrait>(obj.d, obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, ASinus, ASinusTrait>>(
        UnaryOperation<double, ASinus, ASinusTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto asinus(const T &obj) -> BaseType {
  return ASinus(obj);
}

template <typename T>
auto cosinus(const T &obj)
    -> Vec<double,
           UnaryOperation<decltype(convert(obj).d), Cosinus, CosinusTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), Cosinus, CosinusTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), Cosinus, CosinusTrait>(obj.d,
                                                               obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, Cosinus, CosinusTrait>>(
        UnaryOperation<double, Cosinus, CosinusTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto cosinus(const T &obj) -> BaseType {
  return Cosinus(obj);
}

template <typename T>
auto cosinush(const T &obj)
    -> Vec<double,
           UnaryOperation<decltype(convert(obj).d), CosinusH, CosinusHTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), CosinusH, CosinusHTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), CosinusH, CosinusHTrait>(obj.d,
                                                                 obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, CosinusH, CosinusHTrait>>(
        UnaryOperation<double, CosinusH, CosinusHTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto cosinush(const T &obj) -> BaseType {
  return CosinusH(obj);
}

template <typename T>
auto acosinus(const T &obj)
    -> Vec<double,
           UnaryOperation<decltype(convert(obj).d), ACosinus, ACosinusTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), ACosinus, ACosinusTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), ACosinus, ACosinusTrait>(obj.d,
                                                                 obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, ACosinus, ACosinusTrait>>(
        UnaryOperation<double, ACosinus, ACosinusTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto acosinus(const T &obj) -> BaseType {
  return ACosinus(obj);
}

template <typename T>
auto tangens(const T &obj)
    -> Vec<double,
           UnaryOperation<decltype(convert(obj).d), Tangens, TangensTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), Tangens, TangensTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), Tangens, TangensTrait>(obj.d,
                                                               obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, Tangens, TangensTrait>>(
        UnaryOperation<double, Tangens, TangensTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto tangens(const T &obj) -> BaseType {
  return Tangens(obj);
}

template <typename T>
auto tangensh(const T &obj)
    -> Vec<double,
           UnaryOperation<decltype(convert(obj).d), TangensH, TangensHTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), TangensH, TangensHTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), TangensH, TangensHTrait>(obj.d,
                                                                 obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, TangensH, TangensHTrait>>(
        UnaryOperation<double, TangensH, TangensHTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto tangensh(const T &obj) -> BaseType {
  return TangensH(obj);
}

template <typename T>
auto atangens(const T &obj)
    -> Vec<double,
           UnaryOperation<decltype(convert(obj).d), ATangens, ATangensTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), ATangens, ATangensTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), ATangens, ATangensTrait>(obj.d,
                                                                 obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, ATangens, ATangensTrait>>(
        UnaryOperation<double, ATangens, ATangensTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto atangens(const T &obj) -> BaseType {
  return ATangens(obj);
}

template <typename T>
auto ln(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), Log, LogTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), Log, LogTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), Log, LogTrait>(obj.d, obj.d.mp));
  } else if constexpr (isDouble) { // issue: this should be never be used. Also
                                   // for binary operations true.
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, Log, LogTrait>>(
        UnaryOperation<double, Log, LogTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto ln(const T &obj) -> BaseType {
  return Log(obj);
}

template <typename T>
auto sqroot(const T &obj)
    -> Vec<
        double,
        UnaryOperation<decltype(convert(obj).d), SquareRoot, SquareRootTrait>,
        UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double,
               UnaryOperation<decltype(obj.d), SquareRoot, SquareRootTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), SquareRoot, SquareRootTrait>(obj.d,
                                                                     obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, SquareRoot, SquareRootTrait>>(
        UnaryOperation<double, SquareRoot, SquareRootTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto sqroot(const T &obj) -> BaseType {
  return SquareRoot(obj);
}

template <typename T>
auto exp(const T &obj) // issue: document new function e^x --> exp
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), Exp, ExpTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), Exp, ExpTrait>,
               UnaryTrait>(
        UnaryOperation<decltype(obj.d), Exp, ExpTrait>(obj.d, obj.d.mp));
  } else if constexpr (isDouble) {
    static_assert(std::is_same_v<T, BaseType>, "This should never be called!");
    return Vec<double, UnaryOperation<double, Exp, ExpTrait>>(
        UnaryOperation<double, Exp, ExpTrait>{obj});
  }
}
template <typename T>
  requires std::is_same_v<T, BaseType>
auto exp(const T &obj) -> BaseType {
  return Exp(obj);
}

} // namespace etr

#endif
