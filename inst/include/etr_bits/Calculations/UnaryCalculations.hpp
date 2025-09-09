#ifndef UNARYOPERATION_ETR_HPP
#define UNARYOPERATION_ETR_HPP

namespace etr {

template <typename I, typename Trait>
struct UnaryOpClassIterator {
  ConstHolder<I> obj;
  size_t index;

  UnaryOpClassIterator(const I& obj_, size_t index = 0)
  : obj(obj_), index(index) {}

  UnaryOpClassIterator(I&& obj_, size_t index = 0)
  : obj(std::move(obj_)), index(index) {}

  auto operator*() const {
    if constexpr (IsArithV<I>) {
      return Trait::f(obj.get());
    } else {
      return Trait::f(obj.get()[safe_modulo(index, obj.get().size())]);
    }
  }

  UnaryOpClassIterator& operator++() {
    ++index;
    return *this;
  }

};

struct UnaryOpSentinel {
  size_t n;
};

template<typename I, typename Trait>
inline bool operator!=(const UnaryOpClassIterator<I, Trait>& it, const UnaryOpSentinel& s) {
  return it.index < s.n;
}

template <typename I, typename UTrait> struct UnaryOperation {
  using Trait = UTrait;
  using RetType = typename I::RetType;
  using Type = typename I::RetType;
  ConstHolder<I> obj;
  using typeTraitObj = I;
  using TypeTrait = UnaryTrait;

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

  // l value
  UnaryOperation(const I &obj_) : obj(obj_) {}
  // r value
  UnaryOperation(I &&obj_) : obj(std::move(obj_)) {}

  RetType operator[](std::size_t i) const {
    if constexpr (IsArithV<I>) {
      return Trait::f(obj.get());
    } else if constexpr (!IsArithV<I>) {
      return Trait::f(obj.get()[safe_modulo(i, obj.get().size())]);
    }
  }
  std::size_t size() const {
    if constexpr (IsArithV<I>) {
      return 1;
    } else if constexpr (!IsArithV<I>) {
      return obj.get().size();
    }
  }

  // Copy constructor
  UnaryOperation(const UnaryOperation& other) : obj(other.obj) {}
  // Copy assignment
  UnaryOperation& operator=(const UnaryOperation& other) = delete;
  // Move constructor
  UnaryOperation(UnaryOperation&& other) : obj(std::move(other.obj)) {}
  // Move assignment
  UnaryOperation& operator=(UnaryOperation&& other) = delete;

  auto begin() const { return UnaryOpClassIterator<I, UTrait>{obj.get(), 0}; }
  auto end() const { return UnaryOpSentinel{this->size()}; }
};

template <typename T> auto operator-(T&& obj) {
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, MinusUnaryTrait>>(
        UnaryOperation<I, MinusUnaryTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, MinusUnaryTrait>>(
        UnaryOperation<I, MinusUnaryTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, MinusUnaryTrait>>(
      UnaryOperation<T, MinusUnaryTrait>{obj});
  }
}

template <typename T> auto sinus(T &&obj) {
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, SinusTrait>>(
        UnaryOperation<I, SinusTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, SinusTrait>>(
        UnaryOperation<I, SinusTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, SinusTrait>>(
      UnaryOperation<T, SinusTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto sinus(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return sin(obj);
  } else {
    ass<"You can only apply sinus to numeric values">(false);
    return 0.0; // please compiler
  }
}

template <typename T> auto sinush(T &&obj) {
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, SinusHTrait>>(
        UnaryOperation<I, SinusHTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, SinusHTrait>>(
        UnaryOperation<I, SinusHTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, SinusHTrait>>(
      UnaryOperation<T, SinusHTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto sinh(const T &obj) -> double {
  return SinusH(obj);
}

template <typename T> auto asinus(T&& obj) { // ASinusTrait
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, ASinusTrait>>(
        UnaryOperation<I, ASinusTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, ASinusTrait>>(
        UnaryOperation<I, ASinusTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, ASinusTrait>>(
      UnaryOperation<T, ASinusTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto asin(const T &obj) -> double {
  return ASinus(obj);
}

template <typename T> auto cosinus(T&& obj) { // CosinusTrait
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, CosinusTrait>>(
        UnaryOperation<I, CosinusTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, CosinusTrait>>(
        UnaryOperation<I, CosinusTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, CosinusTrait>>(
      UnaryOperation<T, CosinusTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto cos(const T &obj) -> double {
  return Cosinus(obj);
}

template <typename T> auto cosinush(T&& obj) { // CosinusHTrait
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, CosinusHTrait>>(
        UnaryOperation<I, CosinusHTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, CosinusHTrait>>(
        UnaryOperation<I, CosinusHTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, CosinusHTrait>>(
      UnaryOperation<T, CosinusHTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto cosh(const T &obj) -> double {
  return CosinusH(obj);
}

template <typename T> auto acosinus(T&& obj) { // ACosinusTrait
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, ACosinusTrait>>(
        UnaryOperation<I, ACosinusTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, ACosinusTrait>>(
        UnaryOperation<I, ACosinusTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, ACosinusTrait>>(
      UnaryOperation<T, ACosinusTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto acos(const T &obj) -> double {
  return ACosinus(obj);
}

template <typename T> auto tangens(T&& obj) { // TangensTrait
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, TangensTrait>>(
        UnaryOperation<I, TangensTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, TangensTrait>>(
        UnaryOperation<I, TangensTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, TangensTrait>>(
      UnaryOperation<T, TangensTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto tan(const T &obj) -> double {
  return Tangens(obj);
}

template <typename T> auto tangensh(T&& obj) { // TangensHTrait
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, TangensHTrait>>(
        UnaryOperation<I, TangensHTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, TangensHTrait>>(
        UnaryOperation<I, TangensHTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, TangensHTrait>>(
      UnaryOperation<T, TangensHTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto tanh(const T &obj) -> double {
  return TangensH(obj);
}

template <typename T> auto atangens(T&& obj) { // ATangensTrait
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, ATangensTrait>>(
        UnaryOperation<I, ATangensTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, ATangensTrait>>(
        UnaryOperation<I, ATangensTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, ATangensTrait>>(
      UnaryOperation<T, ATangensTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto atan(const T &obj) -> double {
  return ATangens(obj);
}

template <typename T> auto ln(T&& obj) { // LogTrait
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, LogTrait>>(
        UnaryOperation<I, LogTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, LogTrait>>(
        UnaryOperation<I, LogTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, LogTrait>>(
      UnaryOperation<T, LogTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto ln(const T &obj) -> double {
  return log(obj);
}

template <typename T> auto sqroot(T&& obj) { // SquareRootTrait
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, SquareRootTrait>>(
        UnaryOperation<I, SquareRootTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, SquareRootTrait>>(
        UnaryOperation<I, SquareRootTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, SquareRootTrait>>(
      UnaryOperation<T, SquareRootTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto sqroot(const T &obj) -> double {
  return sqrt(obj);
}

template <typename T>
auto expo(T&& obj) { // ExpTrait
  using TD = std::decay_t<T>;
  if constexpr (!IsArithV<TD>) {
    using I = std::decay_t<decltype(obj.d)>;
    using E = typename ExtractDataType<I>::RetType;
    if constexpr(IsRvalueV<T&&>) {
      return Vec<E, UnaryOperation<I, ExpTrait>>(
        UnaryOperation<I, ExpTrait>(std::move(obj.d)));
    } else {
      return Vec<E, UnaryOperation<I, ExpTrait>>(
        UnaryOperation<I, ExpTrait>(obj.d));
    }
  } else {
    return Vec<T, UnaryOperation<T, ExpTrait>>(
      UnaryOperation<T, ExpTrait>{obj});
  }
}
template <typename T>
requires IsArithV<T>
auto exp(const T &obj) -> double {
  return exp(obj);
}

} // namespace etr

#endif
