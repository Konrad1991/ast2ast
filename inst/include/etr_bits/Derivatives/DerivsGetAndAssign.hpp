#ifndef GETASSIGN_DERIVS_ETR_H
#define GETASSIGN_DERIVS_ETR_H

#include "../BinaryCalculations.hpp"
#include "../Core.hpp"
#include "../UnaryCalculations.hpp"
#include <cstddef>

namespace etr {

#ifdef DERIV_ETR

template <typename T>
  requires IsVec<T>
inline auto get_deriv(T &v) {
  Vec<double, Buffer<double>> ret{v.deriv};
  return ret;
}

template <typename T>
  requires IsSubVec<T>
inline auto get_deriv(T &&v) {
  Buffer<double> deriv(v.size());
  for(size_t i = 0; i < deriv.size(); i++) {
    deriv[i] = v[i];
  }
  Vec<double, Buffer<double>> ret{deriv};
  ret.dep_var = true;
  return ret;
}


template <typename T>
  requires IsVec<T>
inline auto set_indep(T &v) {
  v.dep_var = true;
  v.deriv.resize(v.d.size());
  v.deriv.fill(1.0);
}

template <typename L, typename R> inline void assign_deriv(L &&l, const R &r) {
  using DataTypeOtherVec = typename etr::ExtractDataType<
      std::remove_reference_t<decltype(r)>>::RetType;
  l.deriv.resize(r.size());
  for (std::size_t i = 0; i < r.size(); i++) {
    // if constexpr (is<DataTypeOtherVec, double>) {
    //   l.deriv[i] = r[i];
    // } else {
    //    l.deriv[i] = static_cast<double>(r[i]);
    //  }
  }
}


#endif

} // namespace etr

#endif
