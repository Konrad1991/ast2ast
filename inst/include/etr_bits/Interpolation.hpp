#ifndef INTERPOLATION_ETR_HPP
#define INTERPOLATION_ETR_HPP

namespace etr {

// t and the values of parVec carry derivatives; the time knots and the segment search stay double
template <typename S, typename B, typename C>
inline auto cmrInternal(const S& tInp, const B& timeVec, const C& parVec) {
  using P = decltype(get_scalar_val(parVec.get(0)));
  using T = common_type_t<common_type_t<S, P>, Double>;
  auto y_at = [&](std::size_t i) { return T(get_scalar_val(parVec.get(i))); };

  ass<"time_vec and param_vec differ in length in cmr">(timeVec.size() == parVec.size());
  ass<"time and parameter require at least 4 elements">(timeVec.size() >= 4);

  const std::size_t n = timeVec.size();

  // strictly increasing time
  for (std::size_t i = 0; i + 1 < n; ++i) {
    ass<"timeVec must be strictly increasing">(
      get_val(timeVec.get(i + 1)) > get_val(timeVec.get(i))
    );
  }

  const T t = T(tInp);
  const double t_val = static_cast<double>(get_val(tInp));

  // Clamp outside domain
  const double t_first = get_val(timeVec.get(0));
  const double t_last  = get_val(timeVec.get(n - 1));

  if (t_val <= t_first) return y_at(0);
  if (t_val >= t_last)  return y_at(n - 1);

  // Find segment i such that time[i] <= t < time[i+1], with i in [0, n-2]
  std::size_t i_seg = 0;
  for (std::size_t i = 0; i + 1 < n; i++) {
    const double ti  = get_val(timeVec.get(i));
    const double tip = get_val(timeVec.get(i + 1));
    if (t_val >= ti && t_val < tip) { i_seg = i; break; }
  }

  // Indices for the segment
  const std::size_t idx1 = i_seg;
  const std::size_t idx2 = i_seg + 1;

  // Values at idx1/idx2
  const double t1 = get_val(timeVec.get(idx1));
  const T y1 = y_at(idx1);
  const double t2 = get_val(timeVec.get(idx2));
  const T y2 = y_at(idx2);

  // Endpoint handling (non-periodic): "ghost" points via linear extrapolation
  double t0, t3;
  T y0, y3;

  if (idx1 == 0) {
    // left ghost from first two points
    t0 = 2.0 * get_val(timeVec.get(0)) - get_val(timeVec.get(1));
    y0 = Double(2.0) * y_at(0) - y_at(1);
  } else {
    t0 = get_val(timeVec.get(idx1 - 1));
    y0 = y_at(idx1 - 1);
  }

  if (idx2 + 1 >= n) {
    // right ghost from last two points
    t3 = 2.0 * get_val(timeVec.get(n - 1)) - get_val(timeVec.get(n - 2));
    y3 = Double(2.0) * y_at(n - 1) - y_at(n - 2);
  } else {
    t3 = get_val(timeVec.get(idx2 + 1));
    y3 = y_at(idx2 + 1);
  }

  // Hermite form with slopes from neighbors (Catmull–Rom style)
  const T x  = (t - Double(t1)) / Double(t2 - t1);
  const T m1 = (y2 - y0) / Double(t2 - t0);
  const T m2 = (y3 - y1) / Double(t3 - t1);

  const T x2 = x * x;
  const T x3 = x2 * x;
  const Double h(t2 - t1);

  return T(
      (Double(2.0) * x3 - Double(3.0) * x2 + Double(1.0)) * y1 +
      (x3 - Double(2.0) * x2 + x) * h * m1 +
      (Double(-2.0) * x3 + Double(3.0) * x2) * y2 +
      (x3 - x2) * h * m2);
}
template <typename A, typename B, typename C>
  requires IsArray<A>
inline auto cmr(const A &tInp, const B &timeVec, const C &parVec) {
  static_assert(IsArray<B>, "time vector has to be a vector");
  static_assert(IsArray<C>, "parameter vector has to be a vector");
  ass<"In cmr: the first argument cannot have more than one element">(tInp.size() <= 1);
  return cmr(get_scalar_val(tInp.get(0)), timeVec, parVec);
}

template <typename A, typename B, typename C>
requires (!IsArray<A>)
inline auto cmr(A tInp, const B &timeVec, const C &parVec) {
  static_assert(IsArray<B>, "time vector has to be a vector");
  static_assert(IsArray<C>, "parameter vector has to be a vector");
  return cmrInternal(get_scalar_val(tInp), timeVec, parVec);
}

} // namespace etr

#endif
