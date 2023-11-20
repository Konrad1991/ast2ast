#ifndef UNARYOPERATION
#define UNARYOPERATION

#include "UtilsTraits.hpp"

namespace etr {

template <typename I, UnaryFct f, typename Trait = UnaryTrait,
          typename CTrait = UnaryTrait>
struct UnaryOperation {
  using TypeTrait = Trait;
  using CaseTrait = Trait;
  const I &obj;
  MatrixParameter mp;
  UnaryOperation(const UnaryOperation &other) : obj(other.obj), mp(other.mp) {}
  UnaryOperation(const UnaryOperation &&other) : obj(other.obj), mp(other.mp) {}
  UnaryOperation(const I &obj_) : obj(obj_) {}
  template <typename IType, UnaryFct fOther, typename TraitOther>
  UnaryOperation(const UnaryOperation<IType, fOther, TraitOther> &other)
      : obj(other.obj) {}
  double operator[](size_t i) const {
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
};

template <typename T>
auto sinus(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), Sinus, SinusTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), Sinus, SinusTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), Sinus, SinusTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, Sinus, SinusTrait>>(
        UnaryOperation<double, Sinus, SinusTrait>{obj});
  }
}

template <typename T>
auto sinush(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), SinusH, SinusHTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), SinusH, SinusHTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), SinusH, SinusHTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, SinusH, SinusHTrait>>(
        UnaryOperation<double, SinusH, SinusHTrait>{obj});
  }
}

template <typename T>
auto asinus(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), ASinus, ASinusTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), ASinus, ASinusTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), ASinus, ASinusTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, ASinus, ASinusTrait>>(
        UnaryOperation<double, ASinus, ASinusTrait>{obj});
  }
}


template <typename T>
auto cosinus(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), Cosinus, CosinusTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), Cosinus, CosinusTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), Cosinus, CosinusTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, Cosinus, CosinusTrait>>(
        UnaryOperation<double, Cosinus, CosinusTrait>{obj});
  }
}

template <typename T>
auto cosinush(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), CosinusH, CosinusHTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), CosinusH, CosinusHTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), CosinusH, CosinusHTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, CosinusH, CosinusHTrait>>(
        UnaryOperation<double, CosinusH, CosinusHTrait>{obj});
  }
}

template <typename T>
auto acosinus(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), ACosinus, ACosinusTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), ACosinus, ACosinusTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), ACosinus, ACosinusTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, ACosinus, ACosinusTrait>>(
        UnaryOperation<double, ACosinus, ACosinusTrait>{obj});
  }
}

template <typename T>
auto tangens(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), Tangens, TangensTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), Tangens, TangensTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), Tangens, TangensTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, Tangens, TangensTrait>>(
        UnaryOperation<double, Tangens, TangensTrait>{obj});
  }
}

template <typename T>
auto tangensh(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), TangensH, TangensHTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), TangensH, TangensHTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), TangensH, TangensHTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, TangensH, TangensHTrait>>(
        UnaryOperation<double, TangensH, TangensHTrait>{obj});
  }
}

template <typename T>
auto atangens(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), ATangens, ATangensTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), ATangens, ATangensTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), ATangens, ATangensTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, ATangens, ATangensTrait>>(
        UnaryOperation<double, ATangens, ATangensTrait>{obj});
  }
}

template <typename T>
auto logarithm(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), Log, LogTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), Log, LogTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), Log, LogTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, Log, LogTrait>>(
        UnaryOperation<double, Log, LogTrait>{obj});
  }
}

template <typename T>
auto squareRoot(const T &obj)
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), SquareRoot, SquareRootTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), SquareRoot, SquareRootTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), SquareRoot, SquareRootTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, SquareRoot, SquareRootTrait>>(
        UnaryOperation<double, SquareRoot, SquareRootTrait>{obj});
  }
}

template <typename T>
auto Exp(const T &obj) // issue: docuemtn that the behaviour has changed from exponent to e^x
    -> Vec<double, UnaryOperation<decltype(convert(obj).d), Exp, ExpTrait>,
           UnaryTrait> {
  constexpr bool isDouble = std::is_same_v<T, double>;
  if constexpr (!isDouble) {
    return Vec<double, UnaryOperation<decltype(obj.d), Exp, ExpTrait>, UnaryTrait>
        (UnaryOperation<decltype(obj.d), Exp, ExpTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<double, UnaryOperation<double, Exp, ExpTrait>>(
        UnaryOperation<double, Exp, ExpTrait>{obj});
  }
}

} // namespace etr

#endif