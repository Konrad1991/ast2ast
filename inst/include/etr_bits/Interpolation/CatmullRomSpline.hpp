#ifndef CATMULL_ROM_SPLINE_ETR_HPP
#define CATMULL_ROM_SPLINE_ETR_HPP

namespace etr {

template <typename A, typename B, typename C>
inline double cmrInternal(const A *tInp, const B *timeVec, const C *parVec) {

  double t = *tInp;
  int idx0, idx1, idx2, idx3;
  double t0, t1, t2, t3;
  double y0, y1, y2, y3;
  idx0 = 0;
  idx1 = 0;
  idx2 = 0;
  idx3 = 0;
  t0 = t1 = t2 = t3 = 0.;
  y0 = y1 = y2 = y3 = 0.;
  ass<"x and y differ in length">(timeVec->size() == parVec->size());
  ass<"time and parameter require at least 4 elements">(timeVec->size() >= 4);
  if (t < (*timeVec)[0]) {
    return (*parVec)[0];
  } else if (t > (*timeVec)[timeVec->size() - 1]) {
    return (*parVec)[parVec->size() - 1];
  }
  for (std::size_t i = 0; i <= timeVec->size(); i++) {
    if (i == (timeVec->size() - 1)) {
      idx0 = timeVec->size() - 2;
      t0 = (*timeVec)[idx0];
      y0 = (*parVec)[idx0];
      idx1 = timeVec->size() - 1;
      t1 = (*timeVec)[idx1];
      y1 = (*parVec)[idx1];
      idx2 = timeVec->size() - timeVec->size();
      t2 = (*timeVec)[idx2];
      y2 = (*parVec)[idx2];
      idx3 = timeVec->size() + 1 - timeVec->size();
      t3 = (*timeVec)[idx3];
      y3 = (*parVec)[idx3];
      break;
    } else if (t >= (*timeVec)[i] && t < (*timeVec)[i + 1]) {
      if (i == 0) {
        idx0 = timeVec->size() - 1;
        t0 = (*timeVec)[idx0];
      } else {
        idx0 = i - 1;
        t0 = (*timeVec)[idx0];
      }
      y0 = (*parVec)[idx0];
      idx1 = i;
      t1 = (*timeVec)[idx1];
      y1 = (*parVec)[idx1];
      if (i == timeVec->size() - 1) {
        idx2 = 0;
        t2 = (*timeVec)[idx2] + timeVec[timeVec->size() - 1];
      } else {
        idx2 = i + 1;
        t2 = (*timeVec)[idx2];
      }
      y2 = (*parVec)[idx2];
      if (i == timeVec->size() - 2) {
        idx3 = 0;
        t3 = (*timeVec)[idx3] + timeVec[timeVec->size() - 1];
      } else if (i == timeVec->size() - 1) {
        idx3 = 1;
        t3 = (*timeVec)[idx3] + timeVec[timeVec->size() - 1];
      } else {
        idx3 = i + 2;
        t3 = (*timeVec)[idx3];
      }
      y3 = (*parVec)[idx3];
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
  requires IsVec<A>
inline double cmr(const A &tInp, const B &timeVec, const C &parVec) {
  static_assert(IsVec<B>,
                "time vector has to be a vector");
  static_assert(IsVec<C>,
                "parameter vector has to be a vector");
  using DataTypeTime = typename ReRef<A>::type::value_type;
  static_assert(IS<DataTypeTime, double>,
                "time vector does not include doubles");
  using DataTypeParams = typename ReRef<C>::type::value_type;
  static_assert(IS<DataTypeParams, double>,
                "parameter vector does not include doubles");
  using DataTypeT = typename ReRef<A>::type::value_type;
  static_assert(IS<DataTypeT, double>,
                "time is not of type double");
  warn<"time point has more than one element only the first one is used">(
      tInp.size() > 1);
  return cmrInternal(&tInp, &timeVec, &parVec);
}

template <typename A, typename B, typename C>
  requires IsArithV<A>
inline double cmr(A tInp, const B &timeVec, const C &parVec) {
  static_assert(IsVec<B>,
                "time vector has to be a vector");
  static_assert(IsVec<C>,
                "parameter vector has to be a vector");
  using DataTypeTime = typename ExtractDataType<Decayed<B>>::value_type;
  static_assert(IS<DataTypeTime, double>,
                "time vector does not include doubles");
  using DataTypeParams = typename ReRef<C>::type::value_type;
  static_assert(IS<DataTypeParams, double>,
                "parameter vector does not include doubles");
  if constexpr (!IS<A, double>) {
    double val = static_cast<double>(tInp);
    const double *ptr = &val;
    return cmrInternal(ptr, &timeVec, &parVec);
  } else {
    const double *ptr = &tInp;
    return cmrInternal(ptr, &timeVec, &parVec);
  }
}

} // namespace etr

#endif
