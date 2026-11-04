#ifndef SORT_IFELSE_ETR_HPP
#define SORT_IFELSE_ETR_HPP

#include <algorithm>
#include "./RadixSort.hpp"
#include "./ShellSort.hpp"

namespace etr {

// sort -- ascending by default; drops NA and NaN (R's na.last = NA); returns a
// flat vector keeping the input base type.
// method: the R side maps "auto", "shell", "quick", "radix" to 0..3.
// Registered deriv_possible = FALSE, so only the plain (non-AD) types arrive.
// -----------------------------------------------------------------------------------------------------------
enum class SortMethod { Auto = 0, Shell = 1, Quick = 2, Radix = 3 };
// below this, radix passes + buffers cost more than insertion sort
inline constexpr std::size_t sort_insertion_threshold = 64;

template <typename T> inline auto sort_impl(const T &inp, bool decreasing, SortMethod method) {
  using Inner = typename ExtractDataType<Decayed<T>>::value_type;
  using Raw = decltype(get_val(Inner{}));
  std::vector<Raw> values;
  values.reserve(inp.size());
  for (std::size_t i = 0; i < inp.size(); i++) {
    const auto current = get_scalar_val(inp.get(i));
    if (is_na_or_nan(current)) continue;
    values.push_back(get_val(current));
  }
  auto less = [decreasing](const Raw &a, const Raw &b) { return decreasing ? a > b : a < b; };
  switch (method) {
  case SortMethod::Auto:
    if (values.size() < sort_insertion_threshold) {
      insertion_sort(values, less);
    } else {
      radix_sort(values, decreasing);
    }
    break;
  case SortMethod::Shell: shell_sort(values, less); break;
  case SortMethod::Quick: std::sort(values.begin(), values.end(), less); break;
  case SortMethod::Radix: radix_sort(values, decreasing); break;
  }
  Array<Inner, Buffer<Inner, RBufferTrait>> res(SI{values.size()});
  res.dim = Dim{values.size()};
  for (std::size_t i = 0; i < values.size(); i++) res.set(i, Inner(values[i]));
  return res;
}

template <typename M> inline SortMethod to_sort_method(const M &method) {
  const int m = get_val(get_scalar_val(method));
  ass<"Error in sort: unknown method">(m >= 0 && m <= 3);
  return static_cast<SortMethod>(m);
}

template <typename T> requires IsArray<Decayed<T>>
inline auto sort(const T &inp) { return sort_impl(inp, false, SortMethod::Auto); }
template <typename T, typename U> requires IsArray<Decayed<T>>
inline auto sort(const T &inp, const U &decreasing) {
  return sort_impl(inp, static_cast<bool>(get_val(get_scalar_val(decreasing))), SortMethod::Auto);
}
template <typename T, typename U, typename M> requires IsArray<Decayed<T>>
inline auto sort(const T &inp, const U &decreasing, const M &method) {
  return sort_impl(inp, static_cast<bool>(get_val(get_scalar_val(decreasing))), to_sort_method(method));
}

template <typename T> requires IsScalarLike<Decayed<T>>
inline auto sort(const T &inp) {
  using Inner = Decayed<decltype(get_scalar_val(inp))>;
  Array<Inner, Buffer<Inner, RBufferTrait>> res;
  auto v = get_scalar_val(inp);
  if (!v.isNA()) res.push_back(v);
  res.dim = Dim{res.size()};
  return res;
}
template <typename T, typename U> requires IsScalarLike<Decayed<T>>
inline auto sort(const T &inp, const U &) { return sort(inp); }
template <typename T, typename U, typename M> requires IsScalarLike<Decayed<T>>
inline auto sort(const T &inp, const U &, const M &method) {
  to_sort_method(method);
  return sort(inp);
}

// ifelse(test, yes, no) -- elementwise select, vectorised over `test`. `yes` /
// `no` must be a scalar or exactly length(test): no recycling. NA in `test`
// yields NA. The result keeps `test`'s shape and the common numeric type of
// `yes` / `no`; gradients flow through whichever branch each element took.
// -----------------------------------------------------------------------------------------------------------
template <typename X> inline auto ifelse_at(const X &x, std::size_t i) {
  if constexpr (IsArray<Decayed<X>>) {
    return x.get(i);
  } else {
    return get_scalar_val(x);
  }
}
template <typename X> inline std::size_t ifelse_len(const X &x) {
  if constexpr (IsArray<Decayed<X>>) return x.size();
  else return static_cast<std::size_t>(1);
}

template <typename TTest, typename TYes, typename TNo> requires IsScalarLike<Decayed<TTest>>
inline auto ifelse(const TTest &test, const TYes &yes, const TNo &no) {
  using Res = common_type_t<Decayed<decltype(ifelse_at(yes, 0))>,
                            Decayed<decltype(ifelse_at(no, 0))>>;
  const auto tv = get_scalar_val(test);
  if (tv.isNA()) return Res::NA();
  if (get_val(tv) != 0) return Res(ifelse_at(yes, 0));
  return Res(ifelse_at(no, 0));
}

template <typename TTest, typename TYes, typename TNo> requires IsArray<Decayed<TTest>>
inline auto ifelse(const TTest &test, const TYes &yes, const TNo &no) {
  using Res = common_type_t<Decayed<decltype(ifelse_at(yes, 0))>,
                            Decayed<decltype(ifelse_at(no, 0))>>;
  const std::size_t n = test.size();
  const std::size_t ny = ifelse_len(yes);
  const std::size_t nn = ifelse_len(no);
  ass<"Error in ifelse: 'yes' must be length 1 or length(test)">(ny == 1 || ny == n);
  ass<"Error in ifelse: 'no' must be length 1 or length(test)">(nn == 1 || nn == n);
  Array<Res, Buffer<Res, RBufferTrait>> res(SI{n});
  res.dim = test.get_dim();
  for (std::size_t i = 0; i < n; i++) {
    const auto tv = get_scalar_val(test.get(i));
    if (tv.isNA()) { res.set(i, Res::NA()); continue; }
    if (get_val(tv) != 0) {
      res.set(i, Res(ifelse_at(yes, ny == 1 ? 0 : i)));
    } else {
      res.set(i, Res(ifelse_at(no, nn == 1 ? 0 : i)));
    }
  }
  return res;
}

}

#endif
