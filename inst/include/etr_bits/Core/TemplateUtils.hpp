
#ifndef TEMPLATE_UTILS_ETR_H
#define TEMPLATE_UTILS_ETR_H

#include "Header.hpp"
#include <type_traits>

namespace etr {

template <typename T, typename U>
inline constexpr bool IS = std::is_same_v<T, U>;

template <typename T> using IsArith = std::is_arithmetic<T>;
template <typename T> constexpr bool IsArithV = std::is_arithmetic_v<T>;

template <typename T> using Decayed = std::decay_t<T>;

template <typename T>
constexpr bool IsFloatingPointV = std::is_floating_point_v<T>;

template <typename T> constexpr bool IsIntegral = std::is_integral_v<T>;

template <typename T> constexpr bool IsBool = IS<T, bool>;

template <typename T> using ReRef = std::remove_reference<T>;

template <typename T> constexpr bool IsRvalue = std::is_rvalue_reference_v<T>;

} // namespace etr

#endif
