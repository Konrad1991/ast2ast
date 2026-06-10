#ifndef HELPER_ETR_HPP
#define HELPER_ETR_HPP

namespace etr {

// is.na
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsArithV<T> inline Logical isNA(const T inp) {
  const auto val = get_scalar_val(inp);
  return val.isNA();
}
template <typename T> requires IsArray<T> inline auto isNA(const T &inp) {
  Array<Logical, Buffer<Logical, RBufferTrait>> res( SI{inp.size()});
  res.dim = std::vector<std::size_t>{inp.size()};
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, get_scalar_val(inp.get(i)).isNA());
  }
  return res;
}
// is.nan
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsArithV<T> inline Logical isNaN(const T inp) {
  if constexpr (IsLogical<T> || IsInteger<T>) {
    return Logical(false);
  } else {
    return inp.isNaN();
  }
}
template <typename T> requires (IsReverseDouble<T> || IsArithRefV<T>) inline Logical isNaN(const T inp) {
  const auto val = get_scalar_val(inp);
  return val.isNaN();
}
template <typename T> requires IsArray<T> inline auto isNaN(const T &inp) {
  Array<Logical, Buffer<Logical, RBufferTrait>> res( SI{inp.size()});
  res.dim = std::vector<std::size_t>{inp.size()};
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, inp.get(i).isNaN());
  }
  return res;
}
// is.infinite
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsArithV<T> inline Logical isInfinite(const T inp) {
  if constexpr (IsLogical<T> || IsInteger<T>) {
    return Logical(false);
  } else {
    return Logical(inp.isInfinite());
  }
}
template <typename T> requires (IsReverseDouble<T> || IsArithRefV<T>) inline Logical isInfinite(const T inp) {
  const auto val = get_scalar_val(inp);
  return Logical(val.isInfinite());
}
template <typename T> requires IsArray<T> inline auto isInfinite(const T &inp) {
  Array<Logical, Buffer<Logical, RBufferTrait>> res( SI{inp.size()});
  res.dim = std::vector<std::size_t>{inp.size()};
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, inp.get(i).isInfinite());
  }
  return res;
}

// is.finite
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsArithV<T> inline Logical isFinite(const T inp) {
  if constexpr (IsLogical<T> || IsInteger<T>) {
    return Logical(false);
  } else {
    return Logical(inp.isFinite());
  }
}
template <typename T> requires (IsReverseDouble<T> || IsArithRefV<T>) inline Logical isFinite(const T inp) {
  const auto val = get_scalar_val(inp);
  return Logical(val.isFinite());
}
template <typename T> requires IsArray<T> inline auto isFinite(const T &inp) {
  Array<Logical, Buffer<Logical, RBufferTrait>> res( SI{inp.size()});
  res.dim = std::vector<std::size_t>{inp.size()};
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, inp.get(i).isFinite());
  }
  return res;
}

// length
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires (!IsArray<T>) inline Integer length(const T inp) {
  return Integer(1);
}
template <typename T> requires IsArray<T> inline Integer length(const T &inp) {
  return Integer(static_cast<int>(inp.size()));
}
// dim
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires (!IsArray<T>) inline Integer dim(T inp) {
  return Integer(1);
}
template <typename T> requires IsArray<T> inline auto dim(const T &inp) {
  const auto d = dim_view(inp.get_dim());
  Array<Integer, Buffer<Integer, RBufferTrait>> res(SI{d.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, Integer(static_cast<int>(d[i])));
  }
  res.dim = std::vector<std::size_t>{2};
  return res;
}
// ncol & nrow
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires (!IsArray<T>) inline Integer nrow(T inp) {
  return Integer(0);
}
template <typename T> requires (!IsArray<T>) inline Integer ncol(T inp) {
  return Integer(0);
}
template <typename T> requires IsArray<T> inline Integer nrow(const T& inp) {
  const auto d = dim_view(inp.get_dim());
  if (d.size() < 1) return Integer(0);
  return Integer(d[0]);
}
template <typename T> requires IsArray<T> inline Integer ncol(const T& inp) {
  const auto d = dim_view(inp.get_dim());
  if (d.size() < 2) return Integer(0);
  return Integer(d[1]);
}

// max & min -- return the selected element (keeps AD); any NA -> NA (na.rm = FALSE)
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsScalarLike<T> inline auto max(const T inp) { return inp; }
template <typename T> requires IsArray<T> inline auto max(const T &inp) {
  using Type = typename ExtractDataType<Decayed<T>>::value_type;
  ass<"Error in max: argument is of length 0">(inp.size() > 0);
  Type res = inp.get(0);
  if (get_scalar_val(res).isNA()) return res;
  for (std::size_t i = 1; i < inp.size(); i++) {
    Type cur = inp.get(i);
    if (get_scalar_val(cur).isNA()) return cur;
    if (get_val(cur) > get_val(res)) res = cur;
  }
  return res;
}
template <typename T> requires IsScalarLike<T> inline auto min(const T inp) { return inp; }
template <typename T> requires IsArray<T> inline auto min(const T &inp) {
  using Type = typename ExtractDataType<Decayed<T>>::value_type;
  ass<"Error in min: argument is of length 0">(inp.size() > 0);
  Type res = inp.get(0);
  if (get_scalar_val(res).isNA()) return res;
  for (std::size_t i = 1; i < inp.size(); i++) {
    Type cur = inp.get(i);
    if (get_scalar_val(cur).isNA()) return cur;
    if (get_val(cur) < get_val(res)) res = cur;
  }
  return res;
}

// which.max & which.min -- 1-based index of first extreme, NA skipped
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsScalarLike<T> inline Integer which_max(const T inp) { return Integer(1); }
template <typename T> requires IsArray<T> inline Integer which_max(const T &inp) {
  ass<"Error in which.max: argument is of length 0">(inp.size() > 0);
  bool found = false;
  std::size_t best_idx = 0;
  double best_val = 0.0;
  for (std::size_t i = 0; i < inp.size(); i++) {
    const auto cur = get_scalar_val(inp.get(i));
    if (cur.isNA()) continue;
    const double v = static_cast<double>(get_val(cur));
    if (!found || v > best_val) {
      found = true;
      best_idx = i;
      best_val = v;
    }
  }
  ass<"Error in which.max: all elements are NA">(found);
  return Integer(static_cast<int>(best_idx + 1));
}
template <typename T> requires IsScalarLike<T> inline Integer which_min(const T inp) { return Integer(1); }
template <typename T> requires IsArray<T> inline Integer which_min(const T &inp) {
  ass<"Error in which.min: argument is of length 0">(inp.size() > 0);
  bool found = false;
  std::size_t best_idx = 0;
  double best_val = 0.0;
  for (std::size_t i = 0; i < inp.size(); i++) {
    const auto cur = get_scalar_val(inp.get(i));
    if (cur.isNA()) continue;
    const double v = static_cast<double>(get_val(cur));
    if (!found || v < best_val) {
      found = true;
      best_idx = i;
      best_val = v;
    }
  }
  ass<"Error in which.min: all elements are NA">(found);
  return Integer(static_cast<int>(best_idx + 1));
}

// all & any -- three-valued logic, matches R (empty all -> TRUE, empty any -> FALSE)
// -----------------------------------------------------------------------------------------------------------
template <typename T> requires IsScalarLike<T> inline Logical all(const T inp) {
  if (get_scalar_val(inp).isNA()) return Logical::NA();
  return Logical(get_val(inp) != 0);
}
template <typename T> requires IsArray<T> inline Logical all(const T &inp) {
  bool seen_na = false;
  for (std::size_t i = 0; i < inp.size(); i++) {
    const auto cur = get_scalar_val(inp.get(i));
    if (cur.isNA()) { seen_na = true; continue; }
    if (get_val(cur) == 0) return Logical(false);
  }
  if (seen_na) return Logical::NA();
  return Logical(true);
}
template <typename T> requires IsScalarLike<T> inline Logical any(const T inp) {
  if (get_scalar_val(inp).isNA()) return Logical::NA();
  return Logical(get_val(inp) != 0);
}
template <typename T> requires IsArray<T> inline Logical any(const T &inp) {
  bool seen_na = false;
  for (std::size_t i = 0; i < inp.size(); i++) {
    const auto cur = get_scalar_val(inp.get(i));
    if (cur.isNA()) { seen_na = true; continue; }
    if (get_val(cur) != 0) return Logical(true);
  }
  if (seen_na) return Logical::NA();
  return Logical(false);
}

// stop -- abort with a message
// -----------------------------------------------------------------------------------------------------------
[[noreturn]] inline void stop(const char *msg) {
#ifdef STANDALONE_ETR
  throw std::runtime_error(msg);
#else
  Rcpp::stop(msg);
#endif
}
[[noreturn]] inline void stop(const std::string &msg) { stop(msg.c_str()); }

} // namespace etr
#endif
