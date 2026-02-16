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
        if constexpr (IsScalarLike<Decayed<I>>) {
            // Scalar input
            return Trait::f(obj);
        } else {
            // Array input
            return Trait::f(obj.get(index));
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
  ConstHolder<I> obj;
  using typeTraitObj = I;
  using TypeTrait = UnaryTrait;
  using value_type = decltype(Trait::f(obj.get().get(0)));

  // l value
  UnaryOperation(const I &obj_) : obj(obj_) {}
  // r value
  UnaryOperation(I &&obj_) : obj(std::move(obj_)) {}

  auto get(std::size_t i) const {
    return Trait::f(obj.get().get(i));
  }
  template<typename V> void set(std::size_t i, const V& val) = delete;

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

template<typename Scalar, typename Trait>
using unary_result_t = decltype( Trait::f( std::declval<Scalar>() ) );

template<typename T, typename I, typename Trait>
inline auto create_unary(T&& obj) {
  using InScalar  = typename ExtractDataType<Decayed<T>>::value_type;
  using E = unary_result_t<InScalar, Trait>;
  if constexpr (IsRvalueV<T&&>) {
    return Array<E, UnaryOperation<I, Trait>>(UnaryOperation<I, Trait>(std::move(obj.d)), obj.get_dim());
  } else {
    return Array<E, UnaryOperation<I, Trait>>(UnaryOperation<I, Trait>(obj.d), obj.get_dim());
  }
}

template <typename T> requires IsArray<Decayed<T>>
inline auto sin(T &&obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, SinusTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto operator-(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, MinusUnaryTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto sinh(T &&obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, SinusHTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto asin(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, ASinusTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto cos(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, CosinusTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto cosh(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, CosinusHTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto acos(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, ACosinusTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto tan(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, TangensTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto tanh(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, TangensHTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto atan(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, ATangensTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto log(T&& obj) { // LogTrait
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, LogTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto sqrt(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, SquareRootTrait>(std::forward<T>(obj));
}

template <typename T> requires IsArray<Decayed<T>>
inline auto exp(T&& obj) {
  using I = std::decay_t<decltype(obj.d)>;
  return create_unary<T, I, ExpTrait>(std::forward<T>(obj));
}

} // namespace etr

#endif
