#ifndef INTERPOLATION_ETR_HPP
#define INTERPOLATION_ETR_HPP

namespace etr {

template <typename B, typename C>
inline Double cmrInternal(const double& t, const B& timeVec, const C& parVec) {

  ass<"time_vec and param_vec differ in length in cmr">(timeVec.size() == parVec.size());
  ass<"time and parameter require at least 4 elements">(timeVec.size() >= 4);

  const std::size_t n = timeVec.size();

  // Optional safety: strictly increasing time
  // (recommended; otherwise division by zero / negative intervals)
  for (std::size_t i = 0; i + 1 < n; ++i) {
    ass<"timeVec must be strictly increasing">(
      get_val(timeVec.get(i + 1)) > get_val(timeVec.get(i))
    );
  }

  // Clamp outside domain
  const double t_first = get_val(timeVec.get(0));
  const double t_last  = get_val(timeVec.get(n - 1));

  if (t <= t_first) return Double(get_val(parVec.get(0)));
  if (t >= t_last)  return Double(get_val(parVec.get(n - 1)));

  // Find segment i such that time[i] <= t < time[i+1], with i in [0, n-2]
  std::size_t i_seg = 0;
  for (std::size_t i = 0; i + 1 < n; ++i) {
    const double ti  = get_val(timeVec.get(i));
    const double tip = get_val(timeVec.get(i + 1));
    if (t >= ti && t < tip) { i_seg = i; break; }
  }

  // Indices for the segment
  const std::size_t idx1 = i_seg;
  const std::size_t idx2 = i_seg + 1;

  // Values at idx1/idx2
  const double t1 = get_val(timeVec.get(idx1));
  const double y1 = get_val(parVec.get(idx1));
  const double t2 = get_val(timeVec.get(idx2));
  const double y2 = get_val(parVec.get(idx2));

  // Endpoint handling (non-periodic): use "ghost" points via linear extrapolation
  double t0, y0, t3, y3;

  if (idx1 == 0) {
    // left ghost from first two points
    const double tA = get_val(timeVec.get(0));
    const double yA = get_val(parVec.get(0));
    const double tB = get_val(timeVec.get(1));
    const double yB = get_val(parVec.get(1));
    t0 = 2.0 * tA - tB;
    y0 = 2.0 * yA - yB;
  } else {
    t0 = get_val(timeVec.get(idx1 - 1));
    y0 = get_val(parVec.get(idx1 - 1));
  }

  if (idx2 + 1 >= n) {
    // right ghost from last two points
    const double tA = get_val(timeVec.get(n - 1));
    const double yA = get_val(parVec.get(n - 1));
    const double tB = get_val(timeVec.get(n - 2));
    const double yB = get_val(parVec.get(n - 2));
    t3 = 2.0 * tA - tB;
    y3 = 2.0 * yA - yB;
  } else {
    t3 = get_val(timeVec.get(idx2 + 1));
    y3 = get_val(parVec.get(idx2 + 1));
  }

  // Hermite form with slopes from neighbors (Catmull–Rom style)
  const double x  = (t - t1) / (t2 - t1);
  const double m1 = (y2 - y0) / (t2 - t0);
  const double m2 = (y3 - y1) / (t3 - t1);

  const double x2 = x * x;
  const double x3 = x2 * x;

  const double res =
      (2.0 * x3 - 3.0 * x2 + 1.0) * y1 +
      (x3 - 2.0 * x2 + x) * (t2 - t1) * m1 +
      (-2.0 * x3 + 3.0 * x2) * y2 +
      (x3 - x2) * (t2 - t1) * m2;

  return Double(res);
}
template <typename A, typename B, typename C>
  requires IsArray<A>
inline Double cmr(const A &tInp, const B &timeVec, const C &parVec) {
  static_assert(IsArray<B>, "time vector has to be a vector");
  static_assert(IsArray<C>, "parameter vector has to be a vector");
  warn<"time point has more than one element only the first one is used">(tInp.size() > 1);
  return cmrInternal(get_val(tInp.get(0)), timeVec, parVec);
}

template <typename A, typename B, typename C>
requires (!IsArray<A>)
inline Double cmr(A tInp, const B &timeVec, const C &parVec) {
  static_assert(IsArray<B>, "time vector has to be a vector");
  static_assert(IsArray<C>, "parameter vector has to be a vector");
  if constexpr (!IsDouble<A>) {
    double val = static_cast<double>(get_val(tInp));
    return cmrInternal(val, timeVec, parVec);
  } else {
    return cmrInternal(get_val(tInp), timeVec, parVec);
  }
}

} // namespace etr

#endif
