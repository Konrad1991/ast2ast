#ifndef REV_ETR_HPP
#define REV_ETR_HPP

namespace etr {

template<typename T> requires IsScalarLike<Decayed<T>> inline auto rev(const T& obj) {
  return obj;
}

template<typename T> requires IsArray<Decayed<T>> inline auto rev(const T& obj) {
  using SType = typename ExtractDataType<Decayed<T>>::value_type;
  Array<SType, Buffer<SType, RBufferTrait>> res(SI{obj.size()});
  res.dim = std::vector<std::size_t>{obj.size()}; // R's rev flattens
  for (std::size_t i = 0; i < obj.size(); i++) {
    res.set(obj.size() - 1 - i, obj.get(i));
  }
  return res;
}

}

#endif
