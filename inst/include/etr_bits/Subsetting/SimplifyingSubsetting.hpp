#include "HelperSubsetting.hpp"

#ifndef SUBSETTING_SIMPLIFYING_ETR_HPP
#define SUBSETTING_SIMPLIFYING_ETR_HPP

namespace etr {

template<typename T>
inline size_t ExtractIndex(const T& obj) {
  using DecayedT = Decayed<T>;
  static_assert(IsCppFloat<DecayedT> || IsCppInt<DecayedT>, "at can only handle integer or double values");
  if constexpr(IsArithV<DecayedT>) {
    if constexpr (IsCppInt<DecayedT>) {
      return obj;
    } else if constexpr(IsCppFloat<DecayedT>) {
      return safe_index_from_double(obj);
    }
  } else {
    ass<"at accepts only vector of length 1">(obj.size() == 1);
    if constexpr (IsCppInt<DecayedT>) {
      return obj[0];
    } else if constexpr(IsCppFloat<DecayedT>) {
      return safe_index_from_double(obj[0]);
    }
  }
}

// direct access vector memory if possible. R is arithmetic
// -----------------------------------------------------------------------------------------------------------
template <typename T, typename R>
requires (
!IsBufferVec<T> && !IsBorrowVec<T> && !IsBufferMat<T> && !IsBorrowMat<T>
)
inline const auto at(const T &inp, R i) {
  return inp[ExtractIndex(i) - 1];
}
template <typename T, typename R>
requires (
IsBufferVec<T> || IsBorrowVec<T> || IsBufferMat<T> || IsBorrowMat<T>
)
inline auto &at(T &inp, R i) {
  std::size_t idx = ExtractIndex(i);
  idx--;
  ass<"No memory was allocated">(inp.d.allocated);
  ass<"Error: out of boundaries --> value below 1">(idx >= 0);
  ass<"Error: out of boundaries">(idx < inp.size());
  return inp.d.p[idx];
}

template <typename T, typename R, typename C>
requires (
!IsBufferVec<T> && !IsBorrowVec<T> && !IsBufferMat<T> && !IsBorrowMat<T>
)
inline const auto at(const T &inp, R row, C col) {
  std::size_t c = ExtractIndex(row);
  std::size_t r = ExtractIndex(col);
  return inp[(c - 1) * inp.nr() + (r - 1)];
}
template <typename T, typename R, typename C>
requires (
IsBufferVec<T> || IsBorrowVec<T> || IsBufferMat<T> || IsBorrowMat<T>
)
inline auto &at(T &inp, R row, C col) {
  std::size_t c = ExtractIndex(row);
  std::size_t r = ExtractIndex(col);
  std::size_t idx = (c - 1) * inp.nr() + (r - 1);
  ass<"No memory was allocated">(inp.d.allocated);
  ass<"Error: out of boundaries --> value below 1">(idx >= 0);
  ass<"Error: out of boundaries">(idx < inp.size());
  return inp.d.p[idx];
}

} // namespace etr

#endif
