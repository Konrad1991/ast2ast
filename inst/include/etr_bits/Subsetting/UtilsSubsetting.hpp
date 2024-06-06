#ifndef UTIL_SUBSETTING_HPP
#define UTIL_SUBSETTING_HPP

#include "../BinaryCalculations.hpp"
#include "../BufferVector.hpp"
#include "../Core.hpp"
#include "../UnaryCalculations.hpp"
#include <cstddef>

namespace etr {
template <typename T> inline auto convertSubset(T &obj) {
  using R = ExtractDType<T>::type;
  return Subset<R, SubsetTrait>(obj);
}

template <typename T> inline auto convertSubsetConst(const T &obj) {
  using R = ExtractDType<T>::type;
  return Subset<const R, SubsetTrait>(obj);
}

template <typename T> inline std::size_t convertSizeSubsetting(const T &inp) {
  if constexpr (std::is_same_v<T, std::size_t>) {
    ass(inp >= 1, "invalid index argument");
    return inp;
  } else if constexpr (std::is_floating_point_v<T>) {
    warn(isDoubleInt(inp),
         "The provided size is a floating-point number with non-zero decimal "
         "places. It has been floored to the nearest integer.");
    ass(inp >= 1.0, "invalid index argument");
    return static_cast<std::size_t>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    ass(inp >= 1, "invalid index argument");
    return static_cast<std::size_t>(inp);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in rep");
  }
}

} // namespace etr

#endif
