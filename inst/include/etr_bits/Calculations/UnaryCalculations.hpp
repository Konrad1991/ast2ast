#ifndef UNARYOPERATION_ETR_HPP
#define UNARYOPERATION_ETR_HPP

namespace etr {

template <typename I, typename Trait>
struct UnaryOpClassIterator {
  const I& obj;
  size_t index;

  UnaryOpClassIterator(const I& obj_, size_t index = 0)
  : obj(obj_), index(index) {}

  auto operator*() const {
    if constexpr (IsArithV<I>) {
      return Trait::f(obj);
    } else {
      return Trait::f(obj[safe_modulo(index, obj.size())]);
    }
  }

  UnaryOpClassIterator& operator++() {
    ++index;
    return *this;
  }

};

struct UnaryOpSentinel {
  std::size_t n;
};

template<typename I, typename Trait>
inline bool operator!=(const UnaryOpClassIterator<I, Trait>& it, const UnaryOpSentinel& s) {
  return it.index < s.n;
}

template <typename I, typename UTrait> struct UnaryOperation {
  using Trait = UTrait;
  using value_type = typename I::value_type;
  using Type = typename I::value_type;
  ConstHolder<I> obj;
  using typeTraitObj = I;
  using TypeTrait = UnaryTrait;

  bool im() const {
    if constexpr (!IsArithV<I>) {
      return obj.get().im();
    } else {
      return false;
    }
  }

  std::size_t nc() const {
    if constexpr (!IsArithV<I>) {
      return obj.get().nc();
    } else {
      return 0;
    }
  }
  std::size_t nr() const {
    if constexpr (!IsArithV<I>) {
      return obj.get().nr();
    } else {
      return 0;
    }
  }

  // l value
  UnaryOperation(const I &obj_) : obj(obj_) {}
  // r value
  UnaryOperation(I &&obj_) : obj(std::move(obj_)) {}

  value_type operator[](std::size_t i) const {
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

template <typename T>
requires (!IsArithV<T>)
auto operator-(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, MinusUnaryTrait>>(
      UnaryOperation<I, MinusUnaryTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, MinusUnaryTrait>>(
      UnaryOperation<I, MinusUnaryTrait>(obj.d));
  }
}

template <typename T>
requires (!IsArithV<T>)
auto sinus(T &&obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, SinusTrait>>(
      UnaryOperation<I, SinusTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, SinusTrait>>(
      UnaryOperation<I, SinusTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto sinus(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return sin(obj);
  } else {
    ass<"You can only apply sin to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto sinush(T &&obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, SinusHTrait>>(
      UnaryOperation<I, SinusHTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, SinusHTrait>>(
      UnaryOperation<I, SinusHTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto sinush(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return sinh(obj);
  } else {
    ass<"You can only apply sinh to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto asinus(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, ASinusTrait>>(
      UnaryOperation<I, ASinusTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, ASinusTrait>>(
      UnaryOperation<I, ASinusTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto asinus(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return asin(obj);
  } else {
    ass<"You can only apply asin to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto cosinus(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, CosinusTrait>>(
      UnaryOperation<I, CosinusTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, CosinusTrait>>(
      UnaryOperation<I, CosinusTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto cosinus(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return cos(obj);
  } else {
    ass<"You can only apply cos to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto cosinush(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, CosinusHTrait>>(
      UnaryOperation<I, CosinusHTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, CosinusHTrait>>(
      UnaryOperation<I, CosinusHTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto cosinush(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return cosh(obj);
  } else {
    ass<"You can only apply cosh to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto acosinus(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, ACosinusTrait>>(
      UnaryOperation<I, ACosinusTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, ACosinusTrait>>(
      UnaryOperation<I, ACosinusTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto acosinus(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return acos(obj);
  } else {
    ass<"You can only apply acos to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto tangens(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, TangensTrait>>(
      UnaryOperation<I, TangensTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, TangensTrait>>(
      UnaryOperation<I, TangensTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto tangens(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return tan(obj);
  } else {
    ass<"You can only apply tan to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto tangensh(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, TangensHTrait>>(
      UnaryOperation<I, TangensHTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, TangensHTrait>>(
      UnaryOperation<I, TangensHTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto tangensh(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return tanh(obj);
  } else {
    ass<"You can only apply tanh to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto atangens(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, ATangensTrait>>(
      UnaryOperation<I, ATangensTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, ATangensTrait>>(
      UnaryOperation<I, ATangensTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto atangens(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return atan(obj);
  } else {
    ass<"You can only apply atan to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto ln(T&& obj) { // LogTrait
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, LogTrait>>(
      UnaryOperation<I, LogTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, LogTrait>>(
      UnaryOperation<I, LogTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto ln(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return log(obj);
  } else {
    ass<"You can only apply log to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto sqroot(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, SquareRootTrait>>(
      UnaryOperation<I, SquareRootTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, SquareRootTrait>>(
      UnaryOperation<I, SquareRootTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto sqroot(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return sqrt(obj);
  } else {
    ass<"You can only apply sqrt to numeric values">(false);
    return 0.0;
  }
}

template <typename T>
requires (!IsArithV<T>)
auto expo(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<I>::value_type;
  if constexpr(IsRvalueV<T&&>) {
    return Vec<E, UnaryOperation<I, ExpTrait>>(
      UnaryOperation<I, ExpTrait>(std::move(obj.d)));
  } else {
    return Vec<E, UnaryOperation<I, ExpTrait>>(
      UnaryOperation<I, ExpTrait>(obj.d));
  }
}
template <typename T>
requires IsArithV<T>
auto expo(const T &obj) -> double {
  if constexpr(IsFloat<T>) {
    return exp(obj);
  } else {
    ass<"You can only apply exp to numeric values">(false);
    return 0.0;
  }
}

} // namespace etr

#endif
