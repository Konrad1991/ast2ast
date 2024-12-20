#ifndef SUBSETTING_AT_HPP
#define SUBSETTING_AT_HPP

#include "MatrixSubsetting.hpp"
#include "UtilsSubsetting.hpp"
#include "VectorSubsetting.hpp"
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

#ifdef DERIV_ETR
template <typename V, typename I>
  requires IsVec<V>
inline auto at(V &vec, I &&idx) {
  using DataType = typename ExtractDataType<V>::RetType;
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndVector(vec, sub.ind, &idx);
  return Vec<DataType, decltype(convertSubset(vec)), SubVecTrait>(
      std::move(sub));
}

template <typename V, typename I>
  requires(IsRVec<V> || IsSubVec<V> || OperationVec<V>)
inline auto at(V &&vec, I &&idx) {
  using DataType = typename ExtractDataType<V>::RetType;
  Subset<const decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndVector(vec, sub.ind, &idx);
  return Vec<DataType, decltype(convertSubsetConst(vec)), SubVecTrait>(
      std::move(sub));
}

template <typename V, typename R, typename C>
  requires IsVec<V>
inline auto at(V &vec, R &&r, C &&c) {
  using DataType = typename ExtractDataType<V>::RetType;
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndMatrix(vec, sub.ind, sub.mp, &r, &c);
  return Vec<DataType, decltype(convertSubset(vec)), SubVecTrait>(
      std::move(sub));
}

template <typename V, typename R, typename C>
  requires(IsRBufferVec<V> || IsSubsetVec<V> || IsOpVec<V>)
inline const auto at(V &&vec, R &&r,
                     C &&c) { // TODO: check that calculations can be subsetted
  using DataType = typename ExtractDataType<V>::RetType;
  Subset<const decltype(convert(vec).d), SubsetTrait> sub(
      vec); // TODO: check whether a new trait SubsetTraitconst is needed
  calcIndMatrix(vec, sub.ind, sub.mp, &r, &c);
  return Vec<DataType, decltype(convertSubsetConst(vec)), SubVecTrait>(
      std::move(sub));
}

#else
template <typename T, typename R>
  requires(IsRBufferVec<T> || IsSubsetVec<T> ||
           IsOpVec<T> && IsArithV<R>)
inline auto at(const T &inp, R i) {
  if constexpr (std::is_integral_v<R>) {
    return inp[i - 1];
  } else if constexpr (IsFloatingPointV<R>) {
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
  requires IsVec<T> && IsFloatingPointV<R>
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
  requires(IsVec<T> && IsArithV<C>)
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
  requires(IsRBufferVec<T> || IsSubsetVec<T> ||
           IsOpVec<T> && IsArithV<R> &&
               IsArithV<C>)
inline const auto at(const T &inp, R r, C c) {
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
#endif

} // namespace etr

#endif
