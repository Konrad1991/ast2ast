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
    return inp[i - 1];
  } else if constexpr (std::is_floating_point_v<R>) {
    return inp[d2i(i) - 1];
  } else {
    static_assert(sizeof(T) == 0, "Unknown type for indexing found in fct at.");
  }
}

template <typename T, typename R>
  requires IsVec<T> && std::is_integral_v<R>
inline auto &at(T &inp, R i) {
  i--;
  ass<"No memory was allocated">(inp.d.allocated);
  ass<"Error: out of boundaries --> value below 1">(i >= 0);
  ass<"Error: out of boundaries --> value beyond size of vector">(i <
                                                                  inp.size());
  return inp.d.p[i];
}

template <typename T, typename R>
  requires IsVec<T> && std::is_floating_point_v<R>
inline auto &at(T &inp, R i_) {
  int i = d2i(i_);
  i--;
  ass<"No memory was allocated">(inp.d.allocated);
  ass<"Error: out of boundaries --> value below 1">(i >= 0);
  ass<"Error: out of boundaries --> value beyond size of vector">(i <
                                                                  inp.size());
  return inp.d.p[i];
}

template <typename T, typename R, typename C>
  requires(IsVec<T> && std::is_arithmetic_v<C>)
inline auto &at(T &inp, R r, C c) {
  ass<"Input is not a matrix!">(inp.im() == true);
  ass<"No memory was allocated">(inp.d.allocated);
  if constexpr (std::is_integral_v<R> && std::is_integral_v<C>) {
    r--;
    c--;
    ass<"Error: out of boundaries --> value below 1">((c * inp.nr() + r) >= 0);
    ass<"Error: out of boundaries --> value beyond size of vector">(
        (c * inp.nr() + r) < inp.size());
    return inp.d.p[c * inp.nr() + r];
  } else if constexpr (!std::is_integral_v<R> && std::is_integral_v<C>) {
    int r_ = d2i(r);
    r_--;
    c--;
    ass<"Error: out of boundaries --> value below 1">((c * inp.nr() + r_) >= 0);
    ass<"Error: out of boundaries --> value beyond size of vector">(
        (c * inp.nr() + r_) < inp.size());
    return inp.d.p[c * inp.nr() + r_];
  } else if constexpr (std::is_integral_v<R> && !std::is_integral_v<C>) {
    r--;
    int c_ = d2i(c);
    c_--;
    ass<"Error: out of boundaries --> value below 1">((c_ * inp.nr() + r) >= 0);
    ass<"Error: out of boundaries --> value beyond size of vector">(
        (c_ * inp.nr() + r) < inp.size());
    return inp.d.p[c_ * inp.nr() + r];
  } else {
    int r_ = d2i(r);
    int c_ = d2i(c);
    r_--;
    c_--;
    ass<"Error: out of boundaries --> value below 1">((c_ * inp.nr() + r_) >=
                                                      0);
    ass<"Error: out of boundaries --> value beyond size of vector">(
        (c_ * inp.nr() + r_) < inp.size());
    return inp.d.p[c_ * inp.nr() + r_];
  }
}

template <typename T, typename R, typename C>
  requires(IsRVec<T> || IsSubVec<T> ||
           OperationVec<T> && std::is_arithmetic_v<R> &&
               std::is_arithmetic_v<C>)
inline auto at(const T &inp, R r, C c) {
  ass<"Input is not a matrix!">(inp.im() == true);
  if constexpr (std::is_integral_v<R> && std::is_integral_v<C>) {
    r--;
    c--;
    ass<"Error: out of boundaries --> value below 1">((c * inp.nr() + r) >= 0);
    ass<"Error: out of boundaries --> value beyond size of vector">(
        (c * inp.nr() + r) < inp.size());
    return inp[c * inp.nr() + r];
  } else if constexpr (!std::is_integral_v<R> && std::is_integral_v<C>) {
    int r_ = d2i(r);
    r_--;
    c--;
    ass<"Error: out of boundaries --> value below 1">((c * inp.nr() + r_) >= 0);
    ass<"Error: out of boundaries --> value beyond size of vector">(
        (c * inp.nr() + r_) < inp.size());
    return inp[c * inp.nr() + r_];
  } else if constexpr (std::is_integral_v<R> && !std::is_integral_v<C>) {
    r--;
    int c_ = d2i(c);
    c_--;
    ass<"Error: out of boundaries --> value below 1">((c_ * inp.nr() + r) >= 0);
    ass<"Error: out of boundaries --> value beyond size of vector">(
        (c_ * inp.nr() + r) < inp.size());
    return inp[c_ * inp.nr() + r];
  } else {
    int r_ = d2i(r);
    int c_ = d2i(c);
    r_--;
    c_--;
    ass<"Error: out of boundaries --> value below 1">((c_ * inp.nr() + r_) >=
                                                      0);
    ass<"Error: out of boundaries --> value beyond size of vector">(
        (c_ * inp.nr() + r_) < inp.size());
    return inp[c_ * inp.nr() + r_];
  }
}

} // namespace etr

#endif
