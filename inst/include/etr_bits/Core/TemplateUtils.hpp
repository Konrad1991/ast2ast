
#ifndef TEMPLATE_UTILS_ETR_H
#define TEMPLATE_UTILS_ETR_H

#include "Header.hpp"
#include <type_traits>

namespace etr {

template <typename T> using IsArith = std::is_arithmetic<T>;
template <typename T> constexpr bool IsArithV = std::is_arithmetic_v<T>;

} // namespace etr

#endif
