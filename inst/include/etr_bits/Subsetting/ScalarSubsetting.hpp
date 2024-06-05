#ifndef SCALAR_SUBSETTING_HPP
#define SCALAR_SUBSETTING_HPP

#include "UtilsSubsetting.hpp"
#include <type_traits>

namespace etr {

template <typename T, typename I>
  requires std::is_arithmetic_v<T>
inline auto subset(T &inp, I &&idx) {
  static_assert(sizeof(T) == 0, "It is not possible to subset scalar values");
}

template <typename T, typename I1, typename I2>
  requires std::is_arithmetic_v<T>
inline auto subset(T &inp, I1 &&idx1, I2 &&idx2) {
  static_assert(sizeof(T) == 0, "It is not possible to subset scalar values");
}

} // namespace etr

#endif // !DEBUG
