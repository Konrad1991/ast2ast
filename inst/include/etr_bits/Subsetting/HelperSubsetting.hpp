#ifndef SUBSETTING_HELPER_ETR_HPP
#define SUBSETTING_HELPER_ETR_HPP

namespace etr {

inline std::size_t safe_index_from_double(double x) {
  ass<"invalid index argument">(std::isfinite(x));
  double t = std::trunc(x);
  ass<"invalid index argument">(t >= 1.0);
  return static_cast<std::size_t>(t);
}

} // namespace etr

#endif
