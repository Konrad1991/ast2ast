#ifndef CASTS_ETR_HPP
#define CASTS_ETR_HPP

namespace etr {

template<typename T> requires IsScalarLike<Decayed<T>> inline auto as_logical(const T& obj) {
  if constexpr (IS<Decayed<T>, Logical>) {
    return obj;
  } else {
    Logical l = obj;
    return l;
  }
}
template<typename T> requires IsScalarLike<Decayed<T>> inline auto as_integer(const T& obj) {
  if constexpr (IS<Decayed<T>, Integer>) {
    return obj;
  } else {
    Integer i = obj;
    return i;
  }
}
template<typename RealType, typename T> requires IsScalarLike<Decayed<T>> inline auto as_numeric(const T& obj) {
  if constexpr (IS<Decayed<T>, RealType>) {
    return obj;
  } else {
    RealType d = obj;
    return d;
  }
}

template<typename T> requires IsArray<Decayed<T>> inline auto as_logical(const T& obj) {
  using Inner = typename ExtractDataType<Decayed<T>>::value_type;
  if constexpr (IS<Inner, Logical>) {
    return obj;
  } else {
    Array<Logical, Buffer<Logical, RBufferTrait>> res(SI{obj.size()});
    res.dim = std::vector<std::size_t>{obj.size()}; // R's cast flattens
    for (std::size_t i = 0; i < obj.size(); i++) {
      res.set(i, Logical(obj.get(i)));
    }
    return res;
  }
}
template<typename T> requires IsArray<Decayed<T>> inline auto as_integer(const T& obj) {
  using Inner = typename ExtractDataType<Decayed<T>>::value_type;
  if constexpr (IS<Inner, Integer>) {
    return obj;
  } else {
    Array<Integer, Buffer<Integer, RBufferTrait>> res(SI{obj.size()});
    res.dim = std::vector<std::size_t>{obj.size()}; // R's cast flattens
    for (std::size_t i = 0; i < obj.size(); i++) {
      res.set(i, Integer(obj.get(i)));
    }
    return res;
  }
}
template<typename RealType, typename T> requires IsArray<Decayed<T>> inline auto as_numeric(const T& obj) {
  using Inner = typename ExtractDataType<Decayed<T>>::value_type;
  if constexpr (IS<Inner, RealType>) {
    return obj;
  } else {
    Array<RealType, Buffer<RealType, RBufferTrait>> res(SI{obj.size()});
    res.dim = std::vector<std::size_t>{obj.size()}; // R's cast flattens
    for (std::size_t i = 0; i < obj.size(); i++) {
      res.set(i, RealType(obj.get(i)));
    }
    return res;
  }
}

}

#endif
