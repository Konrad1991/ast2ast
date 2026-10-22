#ifndef DENSITY_ETR_HPP
#define DENSITY_ETR_HPP

#include <cfloat>
#include <cmath>
#include <limits>
#include "Sample.hpp"

namespace etr {

namespace nmath {

constexpr double ln_sqrt_2pi = 0.918938533204672741780329736406;
constexpr double one_over_sqrt_2pi = 0.398942280401432677939946059934;

// R's dnorm4 (nmath/dnorm.c)
inline double normal_density(double x, double mu, double sigma, bool give_log) {
  constexpr double nan = std::numeric_limits<double>::quiet_NaN();
  constexpr double inf = std::numeric_limits<double>::infinity();
  const double zero = give_log ? -inf : 0.;
  if (std::isnan(x) || std::isnan(mu) || std::isnan(sigma)) return x + mu + sigma;
  if (sigma < 0) return nan;
  if (!std::isfinite(sigma)) return zero;
  if (!std::isfinite(x) && mu == x) return nan;
  if (sigma == 0) return (x == mu) ? inf : zero;
  x = (x - mu) / sigma;
  if (!std::isfinite(x)) return zero;
  x = std::fabs(x);
  if (x >= 2 * std::sqrt(DBL_MAX)) return zero;
  if (give_log) return -(ln_sqrt_2pi + 0.5 * x * x + std::log(sigma));
  if (x < 5) return one_over_sqrt_2pi * std::exp(-0.5 * x * x) / sigma;
  constexpr double ln2 = 0.693147180559945309417232121458;
  if (x > std::sqrt(-2 * ln2 * (DBL_MIN_EXP + 1 - DBL_MANT_DIG))) return 0.;
  const double x1 = std::ldexp(std::nearbyint(std::ldexp(x, 16)), -16);
  const double x2 = x - x1;
  return one_over_sqrt_2pi / sigma * (std::exp(-0.5 * x1 * x1) * std::exp((-0.5 * x2 - x1) * x2));
}

// R's dunif (nmath/dunif.c)
inline double uniform_density(double x, double a, double b, bool give_log) {
  if (std::isnan(x) || std::isnan(a) || std::isnan(b)) return x + a + b;
  if (b <= a) return std::numeric_limits<double>::quiet_NaN();
  if (a <= x && x <= b) return give_log ? -std::log(b - a) : 1. / (b - a);
  return give_log ? -std::numeric_limits<double>::infinity() : 0.;
}

} // namespace nmath

namespace density_internally {

template <typename T> inline double value_of(const T& s) {
  const auto sv = get_scalar_val(s);
  if (sv.isNA()) return na_real();
  return static_cast<double>(get_val(sv));
}

template <typename T> inline std::size_t length_of(const T& obj) {
  if constexpr (IsScalarLike<Decayed<T>>) {
    return 1;
  } else {
    return obj.size();
  }
}

template <typename T> inline auto element(const T& obj, std::size_t i) {
  if constexpr (IsScalarLike<Decayed<T>>) {
    return get_scalar_val(obj);
  } else {
    return get_scalar_val(obj.get(i % obj.size()));
  }
}

template <typename A, typename B, typename C>
using ResultType = common_type_t<common_type_t<common_type_t<
  decltype(sample_internally::data_type_of<A>()), decltype(sample_internally::data_type_of<B>())>,
  decltype(sample_internally::data_type_of<C>())>, Double>;

// NOTE: with derivatives the smooth formula is used where R's value is finite;
// for |z| >= 5 (log = FALSE) the value may differ from R in the last bits
template <typename T, typename X, typename M, typename S>
inline T dnorm_scalar(const X& x, const M& mu, const S& sigma, bool give_log) {
  const double value = nmath::normal_density(value_of(x), value_of(mu), value_of(sigma), give_log);
  if constexpr (std::is_same_v<T, Double>) {
    return Double(value);
  } else {
    const double s = value_of(sigma);
    if (!std::isfinite(value) || !(s > 0) || !std::isfinite(s) || value == 0.) return T(value);
    const T z = (T(x) - T(mu)) / T(sigma);
    if (give_log) return -(T(nmath::ln_sqrt_2pi) + T(0.5) * z * z + T(sigma).log());
    return T(nmath::one_over_sqrt_2pi) * (T(-0.5) * z * z).exp() / T(sigma);
  }
}

// inside the support the density depends only on min and max
template <typename T, typename X, typename A, typename B>
inline T dunif_scalar(const X& x, const A& a, const B& b, bool give_log) {
  const double value = nmath::uniform_density(value_of(x), value_of(a), value_of(b), give_log);
  if constexpr (std::is_same_v<T, Double>) {
    return Double(value);
  } else {
    if (!std::isfinite(value) || value == 0.) return T(value);
    const T width = T(b) - T(a);
    if (give_log) return -(width.log());
    return T(1.0) / width;
  }
}

template <typename L> inline bool log_as_bool(const L& l) {
  const auto lv = get_scalar_val(l);
  ass<"density: log has to be TRUE or FALSE">(!lv.isNA());
  return static_cast<bool>(get_val(lv));
}

// R's math3: recycling to the longest argument, empty if one is empty
template <typename F, typename A, typename B, typename C>
inline auto apply_recycled(const A& a, const B& b, const C& c, F&& f) {
  using T = ResultType<A, B, C>;
  constexpr bool all_scalar = IsScalarLike<Decayed<A>> && IsScalarLike<Decayed<B>> && IsScalarLike<Decayed<C>>;
  if constexpr (all_scalar) {
    const T res = f(element(a, 0), element(b, 0), element(c, 0));
    warn<"NaNs produced">(!(std::isnan(get_val(res)) && !std::isnan(value_of(a)) &&
                            !std::isnan(value_of(b)) && !std::isnan(value_of(c))));
    return res;
  } else {
    const std::size_t na = length_of(a), nb = length_of(b), nc = length_of(c);
    std::size_t n = std::max(na, std::max(nb, nc));
    if (na == 0 || nb == 0 || nc == 0) n = 0;
    auto res = createRVec<T>(Integer(static_cast<int>(n)));
    bool nan_produced = false;
    for (std::size_t i = 0; i < n; i++) {
      const auto ai = element(a, i);
      const auto bi = element(b, i);
      const auto ci = element(c, i);
      const T r = f(ai, bi, ci);
      if (std::isnan(get_val(r)) && !std::isnan(value_of(ai)) && !std::isnan(value_of(bi)) && !std::isnan(value_of(ci))) {
        nan_produced = true;
      }
      res.set(i, r);
    }
    warn<"NaNs produced">(!nan_produced);
    return res;
  }
}

} // namespace density_internally

template <typename X, typename M, typename S, typename L>
inline auto normal_density(const X& x, const M& mean, const S& sd, const L& log) {
  using T = density_internally::ResultType<X, M, S>;
  const bool give_log = density_internally::log_as_bool(log);
  return density_internally::apply_recycled(x, mean, sd, [give_log](const auto& xi, const auto& mi, const auto& si) {
    return density_internally::dnorm_scalar<T>(xi, mi, si, give_log);
  });
}

template <typename X, typename A, typename B, typename L>
inline auto uniform_density(const X& x, const A& min, const B& max, const L& log) {
  using T = density_internally::ResultType<X, A, B>;
  const bool give_log = density_internally::log_as_bool(log);
  return density_internally::apply_recycled(x, min, max, [give_log](const auto& xi, const auto& ai, const auto& bi) {
    return density_internally::dunif_scalar<T>(xi, ai, bi, give_log);
  });
}

} // namespace etr

#endif
