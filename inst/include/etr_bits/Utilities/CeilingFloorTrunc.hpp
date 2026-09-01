#ifndef CEILING_FLOOR_TRUNC_ETR_HPP
#define CEILING_FLOOR_TRUNC_ETR_HPP

namespace etr {

// floor / ceiling / trunc. Derivative is 0 almost everywhere, so the AD value is
// emitted as a constant (Dual dot = 0, ReverseDouble as a tape constant). Like R,
// the result is always double.
template <typename Fn, typename T> requires IsScalarLike<Decayed<T>>
inline auto round_like(Fn fn, const T& obj) {
  auto v = get_scalar_val(obj); // ref -> value; ReverseDouble stays ReverseDouble
  using V = Decayed<decltype(v)>;
  if constexpr (IS<V, ReverseDouble>) {
    if (v.isNA()) return v;
    return ReverseDouble(fn(v.get_val_from_tape()));
  } else if constexpr (IS<V, Dual>) {
    if (v.isNA()) return v;
    return Dual(fn(v.val), 0.0);
  } else {
    if (v.isNA()) return Double::NA();
    return Double(fn(static_cast<double>(get_val(v))));
  }
}

template <typename Fn, typename T> requires IsArray<Decayed<T>>
inline auto round_like(Fn fn, const T& obj) {
  using Inner = typename ExtractDataType<Decayed<T>>::value_type;
  using ResType = std::conditional_t<IS<Inner, ReverseDouble>, ReverseDouble,
                  std::conditional_t<IS<Inner, Dual>, Dual, Double>>;
  Array<ResType, Buffer<ResType, RBufferTrait>> res(SI{obj.size()});
  res.dim = obj.get_dim(); // elementwise: keeps the input shape
  for (std::size_t i = 0; i < obj.size(); i++) {
    res.set(i, round_like(fn, obj.get(i)));
  }
  return res;
}

template <typename T> requires (IsScalarLike<Decayed<T>> || IsArray<Decayed<T>>)
inline auto floor(const T& obj) {
  return round_like([](double x) { return std::floor(x); }, obj);
}
template <typename T> requires (IsScalarLike<Decayed<T>> || IsArray<Decayed<T>>)
inline auto ceiling(const T& obj) {
  return round_like([](double x) { return std::ceil(x); }, obj);
}
template <typename T> requires (IsScalarLike<Decayed<T>> || IsArray<Decayed<T>>)
inline auto trunc(const T& obj) {
  return round_like([](double x) { return std::trunc(x); }, obj);
}
// std::nearbyint honours the default (to-nearest, ties-to-even) mode, matching R's round().
template <typename T> requires (IsScalarLike<Decayed<T>> || IsArray<Decayed<T>>)
inline auto round(const T& obj) {
  return round_like([](double x) { return std::nearbyint(x); }, obj);
}
// R's sign(): -1 / 0 / 1 as double, NaN kept (round_like already forwards NA).
template <typename T> requires (IsScalarLike<Decayed<T>> || IsArray<Decayed<T>>)
inline auto sign(const T& obj) {
  return round_like([](double x) {
    if (std::isnan(x)) return x;
    return static_cast<double>((x > 0.0) - (x < 0.0));
  }, obj);
}

}

#endif
