#ifndef UNARYOPERATION_ETR_HPP
#define UNARYOPERATION_ETR_HPP

namespace etr {

template <typename I, typename Trait>
struct UnaryOpClassIterator {
  const I& obj;
  size_t index;
  UnaryOpClassIterator(const I& obj_, size_t index_ = 0)
  : obj(obj_), index(index_) {}
  auto operator*() const {
    if constexpr (IsCppArithV<I>) {
      return Trait::f(obj);
    } else {
      return Trait::f(obj[index]);
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
  using Type = typename I::value_type;
  ConstHolder<I> obj;
  using typeTraitObj = I;
  using TypeTrait = UnaryTrait;

  // l value
  UnaryOperation(const I &obj_) : obj(obj_) {}
  // r value
  UnaryOperation(I &&obj_) : obj(std::move(obj_)) {}

  auto operator[](std::size_t i) const {
    return Trait::f(obj.get()[i]);
  }
  std::size_t size() const {
    return obj.get().size();
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

template<typename T, typename E, typename I, typename Trait>
inline auto create_unary(T&& obj) {
  if constexpr (IsRvalueV<T&&>) {
    return Array<E, UnaryOperation<I, Trait>>(UnaryOperation<I, Trait>(std::move(obj.d)), obj.get_dim());
  } else {
    return Array<E, UnaryOperation<I, Trait>>(UnaryOperation<I, Trait>(obj.d), obj.get_dim());
  }
}

template <typename T> requires IsArray<Decayed<T>>
inline auto sinus(T &&obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, SinusTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto operator-(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, MinusUnaryTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto sinush(T &&obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, SinusHTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto asinus(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, ASinusTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto cosinus(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, CosinusTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto cosinush(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, CosinusHTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto acosinus(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, ACosinusTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto tangens(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, TangensTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto tangensh(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, TangensHTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto atangens(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, ATangensTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto ln(T&& obj) { // LogTrait
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, LogTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto sqroot(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, SquareRootTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto expo(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  using E = typename ExtractDataType<Decayed<T>>::value_type;
  return create_unary<T,E, I, ExpTrait>(std::forward<T>(obj));
}

} // namespace etr

#endif
