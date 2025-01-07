#ifndef UNARYOPERATION
#define UNARYOPERATION

#include "Core.hpp"

namespace etr {

template <typename I, typename Trait> struct UnaryOpClassIterator {
  const I &obj;
  size_t index;
  
  UnaryOpClassIterator(const I &obj, size_t index = 0)
  : obj(obj), index(index) {}

  auto operator*() const {
    if constexpr (IsArithV<I>) {
      return Trait::f(obj);
    } else if constexpr (!IsArithV<I>) {
      return Trait::f(obj[index % obj.size()]);
    }
  }

  UnaryOpClassIterator &operator++() {
    ++index;
    return *this;
  }

  bool operator!=(const UnaryOpClassIterator &other) const {
    return index != other.index;
  }
};

template <typename I, typename UTrait> struct UnaryOperation {
  using Trait = UTrait;
  using RetType = typename I::RetType;
  const I &obj;
  using typeTraitObj = I;
  MatrixParameter mp;
  bool im() const {
    if constexpr (!IsArithV<I>) {
      return obj.im();
    } else {
      return false;
    }
  }

  std::size_t nc() const {
    if constexpr (!IsArithV<I>) {
      return obj.nc();
    } else {
      return 0;
    }
  }
  std::size_t nr() const {
    if constexpr (!IsArithV<I>) {
      return obj.nr();
    } else {
      return 0;
    }
  }

  UnaryOperation(const UnaryOperation &other) : obj(other.obj) {}
  UnaryOperation(const UnaryOperation &&other) : obj(other.obj) {}
  UnaryOperation(const I &obj_) : obj(obj_) {}
  template <typename IType, typename TraitOther>
  UnaryOperation(const UnaryOperation<IType, TraitOther> &other)
      : obj(other.obj) {}
  RetType operator[](std::size_t i) const {
    constexpr bool isDouble = IsArithV<I>;
    if constexpr (isDouble) {
      return Trait::f(obj);
    } else if constexpr (!isDouble) {
      return Trait::f(obj[i % obj.size()]);
    }
  }
  std::size_t size() const {
    if constexpr (IsArithV<I>) {
      return 1;
    } else if constexpr (!IsArithV<I>) {
      return obj.size();
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

  auto begin() const { return UnaryOpClassIterator<I, UTrait>{obj, 0}; }
  auto end() const { return UnaryOpClassIterator<I, UTrait>{obj, this->size()}; }
};

template <typename T> auto operator-(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), MinusUnaryTrait>>(
        UnaryOperation<decltype(obj.d), MinusUnaryTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<double, MinusUnaryTrait>>(
        UnaryOperation<double, MinusUnaryTrait>{obj});
  }
}

template <typename T> auto sinus(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), SinusTrait>>(
        UnaryOperation<decltype(obj.d), SinusTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, SinusTrait>>(
        UnaryOperation<T, SinusTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto sinus(const T &obj) -> double {
  return sin(obj);
}

template <typename T> auto sinush(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), SinusHTrait>>(
        UnaryOperation<decltype(obj.d), SinusHTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, SinusHTrait>>(
        UnaryOperation<T, SinusHTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto sinh(const T &obj) -> double {
  return SinusH(obj);
}

template <typename T> auto asinus(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), ASinusTrait>>(
        UnaryOperation<decltype(obj.d), ASinusTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, ASinusTrait>>(
        UnaryOperation<T, ASinusTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto asin(const T &obj) -> double {
  return ASinus(obj);
}

template <typename T> auto cosinus(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), CosinusTrait>>(
        UnaryOperation<decltype(obj.d), CosinusTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, CosinusTrait>>(
        UnaryOperation<T, CosinusTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto cos(const T &obj) -> double {
  return Cosinus(obj);
}

template <typename T> auto cosinush(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), CosinusHTrait>>(
        UnaryOperation<decltype(obj.d), CosinusHTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, CosinusHTrait>>(
        UnaryOperation<T, CosinusHTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto cosh(const T &obj) -> double {
  return CosinusH(obj);
}

template <typename T> auto acosinus(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), ACosinusTrait>>(
        UnaryOperation<decltype(obj.d), ACosinusTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, ACosinusTrait>>(
        UnaryOperation<T, ACosinusTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto acos(const T &obj) -> double {
  return ACosinus(obj);
}

template <typename T> auto tangens(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), TangensTrait>>(
        UnaryOperation<decltype(obj.d), TangensTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, TangensTrait>>(
        UnaryOperation<T, TangensTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto tan(const T &obj) -> double {
  return Tangens(obj);
}

template <typename T> auto tangensh(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), TangensHTrait>>(
        UnaryOperation<decltype(obj.d), TangensHTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, TangensHTrait>>(
        UnaryOperation<T, TangensHTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto tanh(const T &obj) -> double {
  return TangensH(obj);
}

template <typename T> auto atangens(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), ATangensTrait>>(
        UnaryOperation<decltype(obj.d), ATangensTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, ATangensTrait>>(
        UnaryOperation<T, ATangensTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto atan(const T &obj) -> double {
  return ATangens(obj);
}

template <typename T> auto ln(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), LogTrait>>(
        UnaryOperation<decltype(obj.d), LogTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, LogTrait>>(
        UnaryOperation<T, LogTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto ln(const T &obj) -> double {
  return log(obj);
}

template <typename T> auto sqroot(const T &obj) {
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), SquareRootTrait>>(
        UnaryOperation<decltype(obj.d), SquareRootTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, SquareRootTrait>>(
        UnaryOperation<T, SquareRootTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto sqroot(const T &obj) -> double {
  return sqrt(obj);
}

template <typename T>
auto exp(const T &obj) { // TODO: update this in the documentation
  constexpr bool isDouble = IsArithV<T>;
  if constexpr (!isDouble) {
    return Vec<typename ExtractDataType<T>::RetType,
               UnaryOperation<decltype(obj.d), ExpTrait>>(
        UnaryOperation<decltype(obj.d), ExpTrait>(obj.d));
  } else if constexpr (isDouble) {
    return Vec<T, UnaryOperation<T, ExpTrait>>(
        UnaryOperation<T, ExpTrait>{obj});
  }
}
template <typename T>
  requires IS<T, double>
auto exp(const T &obj) -> double {
  return exp(obj);
}

} // namespace etr

#endif
