#ifndef SUBSETTING_AT_HPP
#define SUBSETTING_AT_HPP

#include "UtilsSubsetting.hpp"
#include <type_traits>

namespace etr {
/*
- int
- double
- int double
- int int
- double int
- double double
*/

template <typename T, typename R>
  requires(IsRVec<T> || IsSubVec<T> ||
           OperationVec<T> && std::is_arithmetic_v<R>)
inline auto at(const T &inp, R i) {
  if constexpr (std::is_integral_v<R>) {
    return inp[i];
  } else if constexpr (std::is_floating_point_v<R>) {
    return inp[d2i(i)];
  } else {
    static_assert(sizeof(T) == 0, "Unknown type for indexing found in fct at.");
  }
}

template <typename T, typename R>
  requires IsVec<T> && std::is_integral_v<R>
inline auto &at(T &inp, R i) {
  i--;
  ass(inp.d.allocated, "No memory was allocated");
  ass(i >= 0, "Error: out of boundaries --> value below 1");
  ass(i < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[i];
}

template <typename T, typename R>
  requires IsVec<T> && std::is_floating_point_v<R>
inline auto &at(T &inp, R i_) {
  int i = d2i(i_);
  i--;
  ass(inp.d.allocated, "No memory was allocated");
  ass(i >= 0, "Error: out of boundaries --> value below 1");
  ass(i < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[i];
}

template <typename T, typename R, typename C>
  requires(IsVec<T> && std::is_arithmetic_v<C>)
inline auto &at(T &inp, R r, C c) {
  ass(inp.im() == true, "Input is not a matrix!");
  ass(inp.d.allocated, "No memory was allocated");
  if constexpr (std::is_integral_v<R> && std::is_integral_v<C>) {
    r--;
    c--;
    ass((c * inp.nr() + r) >= 0, "Error: out of boundaries --> value below 1");
    ass((c * inp.nr() + r) < inp.size(),
        "Error: out of boundaries --> value beyond size of vector");
    return inp.d.p[c * inp.nr() + r];
  } else if constexpr (!std::is_integral_v<R> && std::is_integral_v<C>) {
    int r_ = d2i(r);
    r_--;
    c--;
    ass((c * inp.nr() + r_) >= 0, "Error: out of boundaries --> value below 1");
    ass((c * inp.nr() + r_) < inp.size(),
        "Error: out of boundaries --> value beyond size of vector");
    return inp.d.p[c * inp.nr() + r_];
  } else if constexpr (std::is_integral_v<R> && !std::is_integral_v<C>) {
    r--;
    int c_ = d2i(c);
    c_--;
    ass((c_ * inp.nr() + r) >= 0, "Error: out of boundaries --> value below 1");
    ass((c_ * inp.nr() + r) < inp.size(),
        "Error: out of boundaries --> value beyond size of vector");
    return inp.d.p[c_ * inp.nr() + r];
  } else {
    int r_ = d2i(r);
    int c_ = d2i(c);
    r_--;
    c_--;
    ass((c_ * inp.nr() + r_) >= 0,
        "Error: out of boundaries --> value below 1");
    ass((c_ * inp.nr() + r_) < inp.size(),
        "Error: out of boundaries --> value beyond size of vector");
    return inp.d.p[c_ * inp.nr() + r_];
  }
}

template <typename T, typename R, typename C>
  requires(IsRVec<T> || IsSubVec<T> ||
           OperationVec<T> && std::is_arithmetic_v<R> &&
               std::is_arithmetic_v<C>)
inline auto at(const T &inp, R r, C c) {
  ass(inp.im() == true, "Input is not a matrix!");
  if constexpr (std::is_integral_v<R> && std::is_integral_v<C>) {
    r--;
    c--;
    ass((c * inp.nr() + r) >= 0, "Error: out of boundaries --> value below 1");
    ass((c * inp.nr() + r) < inp.size(),
        "Error: out of boundaries --> value beyond size of vector");
    return inp[c * inp.nr() + r];
  } else if constexpr (!std::is_integral_v<R> && std::is_integral_v<C>) {
    int r_ = d2i(r);
    r_--;
    c--;
    ass((c * inp.nr() + r_) >= 0, "Error: out of boundaries --> value below 1");
    ass((c * inp.nr() + r_) < inp.size(),
        "Error: out of boundaries --> value beyond size of vector");
    return inp[c * inp.nr() + r_];
  } else if constexpr (std::is_integral_v<R> && !std::is_integral_v<C>) {
    r--;
    int c_ = d2i(c);
    c_--;
    ass((c_ * inp.nr() + r) >= 0, "Error: out of boundaries --> value below 1");
    ass((c_ * inp.nr() + r) < inp.size(),
        "Error: out of boundaries --> value beyond size of vector");
    return inp[c_ * inp.nr() + r];
  } else {
    int r_ = d2i(r);
    int c_ = d2i(c);
    r_--;
    c_--;
    ass((c_ * inp.nr() + r_) >= 0,
        "Error: out of boundaries --> value below 1");
    ass((c_ * inp.nr() + r_) < inp.size(),
        "Error: out of boundaries --> value beyond size of vector");
    return inp[c_ * inp.nr() + r_];
  }
}

} // namespace etr

#endif
