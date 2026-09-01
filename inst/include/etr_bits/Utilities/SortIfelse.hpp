#ifndef SORT_IFELSE_ETR_HPP
#define SORT_IFELSE_ETR_HPP

#include <algorithm>

namespace etr {

// sort -- ascending by default; drops NA (R's na.last = NA); returns a flat
// vector keeping the input base type. std::stable_sort keeps ties in order.
// Registered deriv_possible = FALSE, so only the plain (non-AD) types arrive.
// -----------------------------------------------------------------------------------------------------------
template <typename T> inline auto sort_impl(const T &inp, bool decreasing) {
  using Inner = typename ExtractDataType<Decayed<T>>::value_type;
  std::vector<Inner> tmp;
  tmp.reserve(inp.size());
  for (std::size_t i = 0; i < inp.size(); i++) {
    Inner cur = inp.get(i);
    if (get_scalar_val(cur).isNA()) continue;
    tmp.push_back(cur);
  }
  std::stable_sort(tmp.begin(), tmp.end(), [decreasing](const Inner &a, const Inner &b) {
    return decreasing ? (get_val(a) > get_val(b)) : (get_val(a) < get_val(b));
  });
  Array<Inner, Buffer<Inner, RBufferTrait>> res(SI{tmp.size()});
  res.dim = std::vector<std::size_t>{tmp.size()};
  for (std::size_t i = 0; i < tmp.size(); i++) res.set(i, tmp[i]);
  return res;
}

template <typename T> requires IsArray<Decayed<T>>
inline auto sort(const T &inp) { return sort_impl(inp, false); }
template <typename T, typename U> requires IsArray<Decayed<T>>
inline auto sort(const T &inp, const U &decreasing) {
  return sort_impl(inp, static_cast<bool>(get_val(get_scalar_val(decreasing))));
}

template <typename T> requires IsScalarLike<Decayed<T>>
inline auto sort(const T &inp) {
  using Inner = Decayed<decltype(get_scalar_val(inp))>;
  Array<Inner, Buffer<Inner, RBufferTrait>> res;
  auto v = get_scalar_val(inp);
  if (!v.isNA()) res.push_back(v);
  res.dim = std::vector<std::size_t>{res.size()};
  return res;
}
template <typename T, typename U> requires IsScalarLike<Decayed<T>>
inline auto sort(const T &inp, const U &) { return sort(inp); }

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
