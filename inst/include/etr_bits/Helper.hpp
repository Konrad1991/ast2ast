#ifndef HELPER_ETR_H
#define HELPER_ETR_H

#include "BufferVector.hpp"
#include "Core.hpp"
#include <cstddef>

namespace etr {

// TODO: check how r values behave here

template <typename T>
  requires std::is_arithmetic_v<T>
inline bool ISNA(T inp) {
  return std::isnan(inp);
}

template <typename T>
  requires std::is_arithmetic_v<T>
inline bool ISInfinite(T inp) {
  return std::isinf(inp);
}

// TODO: can the vecotr operator= handle the result of the funciton?
template <typename T>
  requires std::is_arithmetic_v<T>
inline bool isNA(T inp) {
  return std::isnan(inp);
}

template <typename T>
  requires std::is_arithmetic_v<T>
inline bool isInf(T inp) {
  return std::isinf(inp);
}

template <typename T>
  requires IsVec<T>
inline auto isNA(T &inp) {
  Vec<bool, Buffer<bool, BufferTrait, RBufTrait>, RVecTrait> res(
      SI{inp.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res[i] = ISNA(inp[i]);
  }
  return res;
}

template <typename T>
  requires IsVec<T>
inline auto isNA(const T &&inp) {
  Vec<bool, Buffer<bool, BufferTrait, RBufTrait>, RVecTrait> res(
      SI{inp.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res[i] = ISNA(inp[i]);
  }
  return res;
}

// TODO: check that the same fct name is used in ast2ast
template <typename T>
  requires IsVec<T>
inline auto isInf(T &inp) {
  Vec<bool, Buffer<bool, BufferTrait, RBufTrait>, RVecTrait> res(
      SI{inp.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res[i] = ISInfinite(inp[i]);
  }
  return res;
}

template <typename T>
  requires IsVec<T>
inline auto isInf(const T &&inp) {
  Vec<bool, Buffer<bool, BufferTrait, RBufTrait>, RVecTrait> res(
      SI{inp.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res[i] = ISInfinite(inp[i]);
  }
  return res;
}

template <typename T>
  requires std::is_arithmetic_v<T>
inline std::size_t length(T inp) {
  return 1;
}

template <typename T>
  requires IsVec<T>
inline int length(T &inp) {
  return inp.size();
}

template <typename T>
  requires IsVec<T>
inline int length(const T &&inp) {
  return inp.size();
}

template <typename T>
  requires std::is_arithmetic_v<T>
inline auto dim(T inp) {
  ass(false, "dim can only be called with matrix");
}

template <typename T>
  requires(IsRVec<T> || IsSubVec<T> || OperationVec<T>)
inline const auto dim(const T &inp) {
  ass(inp.im(), "dim can only be called with matrix");
  Vec<int> ret(SI{2});
  ret[0] = inp.nr();
  ret[1] = inp.nc();
  return ret;
}

template <typename T>
  requires IsVec<T>
inline const auto dim(T &inp) {
  ass(inp.im(), "dim can only be called with matrix");
  Vec<int> ret(SI{2});
  ret[0] = inp.nr();
  ret[1] = inp.nc();
  return ret;
}

} // namespace etr
#endif
