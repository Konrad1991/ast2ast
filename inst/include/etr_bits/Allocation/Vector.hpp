#ifndef VECTOR_ETR_H
#define VECTOR_ETR_H

#include "AllocationUtils.hpp"

namespace etr {

template <typename T> inline auto vector_integer(const T &inp) {
  if constexpr (IS<T, std::size_t>) {
    return createRVec<int>(inp);
  } else if constexpr (IsFloatingPointV<T>) {
    return createRVec<int>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<int>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass<"invalid length argument">(inp.size() == 1);
    return createRVec<int>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <typename T> inline auto vector_integer(T &inp) {
  if constexpr (IS<T, std::size_t>) {
    return createRVec<int>(inp);
  } else if constexpr (IsFloatingPointV<T>) {
    return createRVec<int>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<int>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass<"invalid length argument">(inp.size() == 1);
    return createRVec<int>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <typename T> inline auto vector_logical(const T &inp) {
  if constexpr (IS<T, std::size_t>) {
    return createRVec<int>(inp);
  } else if constexpr (IsFloatingPointV<T>) {
    return createRVec<int>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<int>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass<"invalid length argument">(inp.size() == 1);
    return createRVec<int>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <typename T> inline auto vector_logical(T &inp) {
  if constexpr (IS<T, std::size_t>) {
    return createRVec<bool>(inp);
  } else if constexpr (IsFloatingPointV<T>) {
    return createRVec<bool>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<bool>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass<"invalid length argument">(inp.size() == 1);
    return createRVec<bool>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <typename T> inline auto vector_numeric(const T &inp) {
  if constexpr (IS<T, std::size_t>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (IsFloatingPointV<T>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass<"invalid length argument">(inp.size() == 1);
    return createRVec<BaseType>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <typename T> inline auto vector_numeric(T &inp) {
  if constexpr (IS<T, std::size_t>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (IsFloatingPointV<T>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass<"invalid length argument">(inp.size() == 1);
    return createRVec<BaseType>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

} // namespace etr

#endif
