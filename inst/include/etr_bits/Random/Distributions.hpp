#ifndef DISTRIBUTIONS_ETR_HPP
#define DISTRIBUTIONS_ETR_HPP

#include <cmath>
#include <limits>
#include "MersenneTwister.hpp"

namespace etr {

namespace nmath {

// R's qnorm5 (nmath/qnorm.c, Wichura AS241 + asymptotic expansion for r > 27)
inline double qnorm(double p, double mu, double sigma, bool lower_tail, bool log_p) {
  constexpr double nan = std::numeric_limits<double>::quiet_NaN();
  constexpr double inf = std::numeric_limits<double>::infinity();
  constexpr double sqrt2 = 1.414213562373095048801688724210;
  constexpr double two_pi = 6.283185307179586476925286766559;
  if (std::isnan(p) || std::isnan(mu) || std::isnan(sigma)) return p + mu + sigma;
  if (log_p) {
    if (p > 0) return nan;
    if (p == 0) return lower_tail ? inf : -inf;
    if (p == -inf) return lower_tail ? -inf : inf;
  } else {
    if (p < 0 || p > 1) return nan;
    if (p == 0) return lower_tail ? -inf : inf;
    if (p == 1) return lower_tail ? inf : -inf;
  }
  if (sigma < 0) return nan;
  if (sigma == 0) return mu;

  const double p_ = log_p ? (lower_tail ? std::exp(p) : -std::expm1(p))
                          : (lower_tail ? p : (0.5 - p + 0.5));
  const double q = p_ - 0.5;
  double r, val;
  if (std::fabs(q) <= .425) {
    r = .180625 - q * q;
    val =
        q * (((((((r * 2509.0809287301226727 +
                   33430.575583588128105) * r + 67265.770927008700853) * r +
                 45921.953931549871457) * r + 13731.693765509461125) * r +
               1971.5909503065514427) * r + 133.14166789178437745) * r +
             3.387132872796366608)
        / (((((((r * 5226.495278852854561 +
                 28729.085735721942674) * r + 39307.89580009271061) * r +
               21213.794301586595867) * r + 5394.1960214247511077) * r +
             687.1870074920579083) * r + 42.313330701600911252) * r + 1.);
    return mu + sigma * val;
  }
  double lp;
  if (log_p && ((lower_tail && q <= 0) || (!lower_tail && q > 0))) {
    lp = p;
  } else {
    const double one_minus_p = log_p ? (lower_tail ? -std::expm1(p) : std::exp(p))
                                     : (lower_tail ? (0.5 - p + 0.5) : p);
    lp = std::log((q > 0) ? one_minus_p : p_);
  }
  r = std::sqrt(-lp);
  if (r <= 5.) {
    r += -1.6;
    val = (((((((r * 7.7454501427834140764e-4 +
               .0227238449892691845833) * r + .24178072517745061177) *
             r + 1.27045825245236838258) * r +
            3.64784832476320460504) * r + 5.7694972214606914055) *
          r + 4.6303378461565452959) * r +
         1.42343711074968357734)
        / (((((((r *
                 1.05075007164441684324e-9 + 5.475938084995344946e-4) *
                r + .0151986665636164571966) * r +
               .14810397642748007459) * r + .68976733498510000455) *
             r + 1.6763848301838038494) * r +
            2.05319162663775882187) * r + 1.);
  } else if (r <= 27) {
    r += -5.;
    val = (((((((r * 2.01033439929228813265e-7 +
               2.71155556874348757815e-5) * r +
              .0012426609473880784386) * r + .026532189526576123093) *
            r + .29656057182850489123) * r +
           1.7848265399172913358) * r + 5.4637849111641143699) *
         r + 6.6579046435011037772)
        / (((((((r *
                 2.04426310338993978564e-15 + 1.4215117583164458887e-7) *
                r + 1.8463183175100546818e-5) * r +
               7.868691311456132591e-4) * r + .0148753612908506148525)
             * r + .13692988092273580531) * r +
            .59983220655588793769) * r + 1.);
  } else if (r >= 6.4e8) {
    val = r * sqrt2;
  } else {
    const double s2 = -std::ldexp(lp, 1);
    double x2 = s2 - std::log(two_pi * s2);
    if (r < 36000.) {
      x2 = s2 - std::log(two_pi * x2) - 2. / (2. + x2);
      if (r < 840.) {
        x2 = s2 - std::log(two_pi * x2) + 2 * std::log1p(-(1 - 1 / (4 + x2)) / (2. + x2));
        if (r < 109.) {
          x2 = s2 - std::log(two_pi * x2) +
               2 * std::log1p(-(1 - (1 - 5 / (6 + x2)) / (4. + x2)) / (2. + x2));
          if (r < 55.) {
            x2 = s2 - std::log(two_pi * x2) +
                 2 * std::log1p(-(1 - (1 - (5 - 9 / (8. + x2)) / (6. + x2)) / (4. + x2)) / (2. + x2));
          }
        }
      }
    }
    val = std::sqrt(x2);
  }
  if (q < 0.0) val = -val;
  return mu + sigma * val;
}

} // namespace nmath

// slots (mean, sd, min, max) are Double; the engine is not a slot
template <typename Engine> struct normal_distribution {
  Engine engine;
  Double mean;
  Double sd;
  normal_distribution() : mean(0.0), sd(1.0) {}
  template <typename M, typename S>
  normal_distribution(const Engine& engine_, const M& mean_, const S& sd_)
      : engine(engine_), mean(static_cast<double>(get_val(mean_))), sd(static_cast<double>(get_val(sd_))) {}

  // R's norm_rand() with normal.kind = "Inversion" (nmath/snorm.c)
  double norm_rand() {
    constexpr double big = 134217728; // 2^27
    double u1 = engine.runif();
    u1 = static_cast<int>(big * u1) + engine.runif();
    return nmath::qnorm(u1 / big, 0.0, 1.0, true, false);
  }
  // R's rnorm(mu, sigma) (nmath/rnorm.c)
  double draw_one() {
    const double mu = get_val(mean);
    const double sigma = get_val(sd);
    if (std::isnan(mu) || !std::isfinite(sigma) || sigma < 0.) return std::numeric_limits<double>::quiet_NaN();
    if (sigma == 0. || !std::isfinite(mu)) return mu;
    return mu + sigma * norm_rand();
  }
};

template <typename Engine> struct uniform_distribution {
  Engine engine;
  Double min;
  Double max;
  uniform_distribution() : min(0.0), max(1.0) {}
  template <typename L, typename U>
  uniform_distribution(const Engine& engine_, const L& min_, const U& max_)
      : engine(engine_), min(static_cast<double>(get_val(min_))), max(static_cast<double>(get_val(max_))) {}

  // R's runif(a, b) (nmath/runif.c)
  double draw_one() {
    const double a = get_val(min);
    const double b = get_val(max);
    if (!std::isfinite(a) || !std::isfinite(b) || b < a) return std::numeric_limits<double>::quiet_NaN();
    if (a == b) return a;
    double u;
    do { u = engine.runif(); } while (u <= 0 || u >= 1);
    return a + (b - a) * u;
  }
};

using normal = normal_distribution<mersenne_twister>;
using uniform = uniform_distribution<mersenne_twister>;

template <typename S> inline mersenne_twister make_mersenne_twister(const S& seed) {
  const double s = static_cast<double>(get_val(seed));
  ass<"mersenne_twister: seed has to be a finite number">(std::isfinite(s));
  return mersenne_twister(static_cast<int>(s));
}
template <typename M = Double, typename S = Double>
inline normal make_normal(const mersenne_twister& engine, const M& mean = Double(0.0), const S& sd = Double(1.0)) {
  return normal(engine, mean, sd);
}
template <typename L = Double, typename U = Double>
inline uniform make_uniform(const mersenne_twister& engine, const L& min = Double(0.0), const U& max = Double(1.0)) {
  return uniform(engine, min, max);
}

// Dist&&: draws advance the engine of a named distribution, temporaries are fine too
template <typename Dist, typename N> inline auto draw(Dist&& dist, const N& n) {
  auto res = createRVec<Double>(n);
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, Double(dist.draw_one()));
  }
  return res;
}

} // namespace etr

#endif
