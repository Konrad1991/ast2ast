#ifndef INTERPOLATION_H
#define INTERPOLATION_H

#include "BufferVector.hpp"

namespace etr {

template <typename A, typename B, typename C>
inline double li(const A &t_, const B &timeVec, const C &parVec) {
  using typeTraitA = std::remove_reference<decltype(t_)>::type::TypeTrait;
  using typeTraitB = std::remove_reference<decltype(timeVec)>::type::TypeTrait;
  using typeTraitC = std::remove_reference<decltype(parVec)>::type::TypeTrait;
  using isVecA = std::is_same<typeTraitA, VectorTrait>;
  using isVecB = std::is_same<typeTraitB, VectorTrait>;
  using isVecC = std::is_same<typeTraitC, VectorTrait>;

  if constexpr (isVecA::value && isVecB::value && isVecC::value) {
    ass(t_.size() == 1, "timepoint input has to have length == 1");
    double t = t_[0];
    double t0, t1;
    double y0, y1, deltaPar, deltaT, m;
    double ret;
    ass(timeVec.size() == parVec.size(), "x and y differ in length");
    // not in boundaries
    if (t >= timeVec[timeVec.size()]) {
      ret = parVec[parVec.size()];
      return ret;
    } else if (t <= timeVec[0]) {
      ret = parVec[0];
      return ret;
    }
    // in boundaries
    for (size_t i = 0; i < timeVec.size(); i++) {
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
  } else if constexpr (!isVecA::value && isVecB::value && isVecC::value) {
    double t = static_cast<BaseType>(t_);
    double t0, t1;
    double y0, y1, deltaPar, deltaT, m;
    double ret;
    ass(timeVec.size() == parVec.size(), "x and y differ in length");
    // not in boundaries
    if (t >= timeVec[timeVec.size()]) {
      ret = parVec[parVec.size()];
      return ret;
    } else if (t <= timeVec[0]) {
      ret = parVec[0];
      return ret;
    }
    // in boundaries
    for (size_t i = 0; i < timeVec.size(); i++) {
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
    ass(false, "Input for interpolation has to be scalar, vec, vec");
  }
}

template <typename A, typename B, typename C>
inline double cmrInternal(const A &tInp, const B &timeVec, const C &parVec) {
  using typeTraitA = std::remove_reference<decltype(tInp)>::type::TypeTrait;
  using typeTraitB = std::remove_reference<decltype(timeVec)>::type::TypeTrait;
  using typeTraitC = std::remove_reference<decltype(parVec)>::type::TypeTrait;
  using isVecA = std::is_same<typeTraitA, VectorTrait>;
  using isVecB = std::is_same<typeTraitB, VectorTrait>;
  using isVecC = std::is_same<typeTraitC, VectorTrait>;

  double t = 0.0;
  if constexpr (isVecA::value && isVecB::value && isVecC::value) {
    t = tInp[0];
  } else if constexpr (!isVecA::value && isVecB::value && isVecC::value) {
    t = static_cast<BaseType>(tInp);
  } else {
    ass(false, "Input for interpolation has to be scalar, vec, vec");
  }

  int idx0, idx1, idx2, idx3;
  double t0, t1, t2, t3;
  double y0, y1, y2, y3;
  idx0 = 0;
  idx1 = 0;
  idx2 = 0;
  idx3 = 0;
  t0 = t1 = t2 = t3 = 0.;
  y0 = y1 = y2 = y3 = 0.;
  ass(timeVec.size() == parVec.size(), "x and y differ in length");
  if (t < timeVec[0]) {
    return parVec[0];
  } else if (t > timeVec[timeVec.size() - 1]) {
    return parVec[parVec.size() - 1];
  }
  for (size_t i = 0; i <= timeVec.size(); i++) {
    if (i == (timeVec.size() - 1)) {
      idx0 = timeVec.size() - 2;
      t0 = timeVec[idx0];
      y0 = parVec[idx0];
      idx1 = timeVec.size() - 1;
      t1 = timeVec[idx1];
      y1 = parVec[idx1];
      idx2 = timeVec.size() - timeVec.size();
      t2 = timeVec[idx2];
      y2 = parVec[idx2];
      idx3 = timeVec.size() + 1 - timeVec.size();
      t3 = timeVec[idx3];
      y3 = parVec[idx3];
      break;
    } else if (t >= timeVec[i] && t < timeVec[i + 1]) {
      if (i == 0) {
        idx0 = timeVec.size() - 1;
        t0 = timeVec[idx0];
      } else {
        idx0 = i - 1;
        t0 = timeVec[idx0];
      }
      y0 = parVec[idx0];
      idx1 = i;
      t1 = timeVec[idx1];
      y1 = parVec[idx1];
      if (i == timeVec.size() - 1) {
        idx2 = 0;
        t2 = timeVec[idx2] + timeVec.back();
      } else {
        idx2 = i + 1;
        t2 = timeVec[idx2];
      }
      y2 = parVec[idx2];
      if (i == timeVec.size() - 2) {
        idx3 = 0;
        t3 = timeVec[idx3] + timeVec.back();
      } else if (i == timeVec.size() - 1) {
        idx3 = 1;
        t3 = timeVec[idx3] + timeVec.back();
      } else {
        idx3 = i + 2;
        t3 = timeVec[idx3];
      }
      y3 = parVec[idx3];
      break;
    }
  }
  double x = (t - t1) / (t2 - t1);
  double m1 = (y2 - y0) / (t2 - t0);
  double m2 = (y3 - y1) / (t3 - t1);

  double res = ((2. * pow(x, 3) - 3. * pow(x, 2) + 1.) * y1 +
                (pow(x, 3) - 2. * pow(x, 2) + x) * (t2 - t1) * m1 +
                (-2. * pow(x, 3) + 3. * pow(x, 2)) * y2 +
                (pow(x, 3) - pow(x, 2)) * (t2 - t1) * m2);
  return res;
}

template <typename A, typename B, typename C>
inline double cmr(const A &tInp, const B &timeVec, const C &parVec) {
  if constexpr (std::is_arithmetic_v<A>) {
    return cmrInternal(Vec<BaseType>(tInp), timeVec, parVec);
  } else {
    return cmrInternal(tInp, timeVec, parVec);
  }
}

} // namespace etr

#endif