#ifndef HELPER_ETR_HPP
#define HELPER_ETR_HPP

namespace etr {

// is.na
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsArithV<T> inline Logical isNA(const T inp) {
  return inp.isNA();
}
template <typename T> requires IsADType<T> inline Logical isNA(const T inp) {
  const auto val = get_scalar_val(inp);
  return val.isNA();
}
template <typename T> requires IsArray<T> inline auto isNA(const T &inp) {
  Array<Logical, Buffer<Logical, RBufferTrait>> res( SI{inp.size()});
  res.dim = std::vector<std::size_t>{inp.size()};
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, inp.get(i).isNA());
  }
  return res;
}
// is.nan
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsArithV<T> inline Logical isNaN(const T inp) {
  if constexpr (IsLogical<T> || IsInteger<T>) {
    return Logical(false);
  } else {
    return inp.isNaN();
  }
}
template <typename T> requires IsADType<T> inline Logical isNaN(const T inp) {
  const auto val = get_scalar_val(inp);
  return val.isNaN();
}
template <typename T> requires IsArray<T> inline auto isNaN(const T &inp) {
  Array<Logical, Buffer<Logical, RBufferTrait>> res( SI{inp.size()});
  res.dim = std::vector<std::size_t>{inp.size()};
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, inp.get(i).isNaN());
  }
  return res;
}
// is.infinite
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsArithV<T> inline Logical isInfinite(const T inp) {
  if constexpr (IsLogical<T> || IsInteger<T>) {
    return Logical(false);
  } else {
    return Logical(inp.isInfinite());
  }
}
template <typename T> requires IsADType<T> inline Logical isInfinite(const T inp) {
  const auto val = get_scalar_val(inp);
  return Logical(val.isInfinite());
}
template <typename T> requires IsArray<T> inline auto isInfinite(const T &inp) {
  Array<Logical, Buffer<Logical, RBufferTrait>> res( SI{inp.size()});
  res.dim = std::vector<std::size_t>{inp.size()};
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, inp.get(i).isInfinite());
  }
  return res;
}

// is.finite
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsArithV<T> inline Logical isFinite(const T inp) {
  if constexpr (IsLogical<T> || IsInteger<T>) {
    return Logical(false);
  } else {
    return Logical(inp.isFinite());
  }
}
template <typename T> requires IsADType<T> inline Logical isFinite(const T inp) {
  const auto val = get_scalar_val(inp);
  return Logical(val.isFinite());
}
template <typename T> requires IsArray<T> inline auto isFinite(const T &inp) {
  Array<Logical, Buffer<Logical, RBufferTrait>> res( SI{inp.size()});
  res.dim = std::vector<std::size_t>{inp.size()};
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, inp.get(i).isFinite());
  }
  return res;
}

// length
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires (!IsArray<T>) inline Integer length(const T inp) {
  return Integer(1);
}
template <typename T> requires IsArray<T> inline Integer length(const T &inp) {
  return Integer(static_cast<int>(inp.size()));
}
// dim
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires (!IsArray<T>) inline Integer dim(T inp) {
  return Integer(1);
}
template <typename T> requires IsArray<T> inline auto dim(const T &inp) {
  const auto d = dim_view(inp.dim);
  Array<Integer, Buffer<Integer, RBufferTrait>> res(SI{d.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, Integer(static_cast<int>(d[i])));
  }
  return res;
}

} // namespace etr
#endif
