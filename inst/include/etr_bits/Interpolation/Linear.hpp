#ifndef LINEAR_INTERPOLATION_ETR_HPP
#define LINEAR_INTERPOLATION_ETR_HPP

namespace etr {

template <typename A, typename B, typename C>
inline double li(const A &t_, const B &timeVec, const C &parVec) {

  if constexpr (IsVec<A> && IsVec<B> && IsVec<C>) {
    ass<"timepoint input has to have length == 1">(t_.size() == 1);
    double t = t_[0];
    double t0, t1;
    double y0, y1, deltaPar, deltaT, m;
    double ret;
    ass<"x and y differ in length">(timeVec.size() == parVec.size());
    // not in boundaries
    if (t >= timeVec[timeVec.size()]) {
      ret = parVec[parVec.size()];
      return ret;
    } else if (t <= timeVec[0]) {
      ret = parVec[0];
      return ret;
    }
    // in boundaries
    for (std::size_t i = 0; i < timeVec.size(); i++) {
      if (t >= timeVec[i] && t < timeVec[i + 1]) {
        t0 = timeVec[i];
        t1 = timeVec[i + 1];
        y0 = parVec[i];
        y1 = parVec[i + 1];
        deltaPar = y1 - y0;
        deltaT = t1 - t0;
        ret = m * (t - t0) + y0;
      }
    }
    return (ret);
  } else if constexpr (!IsVec<A> && IsVec<B> && IsVec<C>) {
    double t = static_cast<double>(t_);
    double t0, t1;
    double y0, y1, deltaPar, deltaT, m;
    double ret;
    ass<"x and y differ in length">(timeVec.size() == parVec.size());
    // not in boundaries
    if (t >= timeVec[timeVec.size()]) {
      ret = parVec[parVec.size()];
      return ret;
    } else if (t <= timeVec[0]) {
      ret = parVec[0];
      return ret;
    }
    // in boundaries
    for (std::size_t i = 0; i < timeVec.size(); i++) {
      if (t >= timeVec[i] && t < timeVec[i + 1]) {
        t0 = timeVec[i];
        t1 = timeVec[i + 1];
        y0 = parVec[i];
        y1 = parVec[i + 1];
        deltaPar = y1 - y0;
        deltaT = t1 - t0;
        ret = m * (t - t0) + y0;
      }
    }
    return (ret);
  } else {
    ass<"Input for interpolation has to be scalar, vec, vec">(false);
  }
}

} // namespace etr

#endif
