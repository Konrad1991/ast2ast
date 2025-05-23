#ifndef HELPER_ETR_HPP
#define HELPER_ETR_HPP

namespace etr {

// is.na
// -----------------------------------------------------------------------------------------------------------
template <typename T>
  requires IsArithV<T>
inline bool isNA(T inp) {
  return std::isnan(inp);
}
template <typename T>
  requires IsArrayLike<T>
inline auto isNA(const T &&inp) {
  Vec<bool, Buffer<bool, RBufferTrait>> res( SI{inp.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res[i] = std::isnan(inp[i]);
  }
  return res;
}

// is.inf
// -----------------------------------------------------------------------------------------------------------
template <typename T>
  requires IsArithV<T>
inline bool isInfinite(T inp) {
  return std::isinf(inp);
}
template <typename T>
  requires IsArrayLike<T>
inline auto isInfinite(T &inp) {
  Vec<bool, Buffer<bool, RBufferTrait>> res( SI{inp.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res[i] = std::isinf(inp[i]);
  }
  return res;
}

// is.inf
// -----------------------------------------------------------------------------------------------------------
template <typename T>
  requires IsArithV<T>
inline bool isFinite(T inp) {
  return !std::isinf(inp);
}
template <typename T>
  requires IsArrayLike<T>
inline auto isFinite(T &inp) {
  Vec<bool, Buffer<bool, RBufferTrait>> res( SI{inp.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res[i] = !std::isinf(inp[i]);
  }
  return res;
}

// length
// -----------------------------------------------------------------------------------------------------------
template <typename T>
  requires IsArithV<T>
inline std::size_t length(T inp) {
  return 1;
}
template <typename T>
  requires (!IsArithV<T>)
inline int length(const T &inp) {
  return inp.size();
}

// dim
// -----------------------------------------------------------------------------------------------------------
template <typename T>
  requires IsArithV<T>
inline auto dim(T inp) {
  ass<"dim can only be called with matrix">(false);
}
template <typename T>
requires IsMat<Decayed<T>>
inline auto dim(const T &inp) {
  Vec<int> ret(SI{2});
  ret[0] = inp.nr();
  ret[1] = inp.nc();
  return ret;
}

} // namespace etr
#endif
