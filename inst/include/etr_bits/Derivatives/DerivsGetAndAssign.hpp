#ifndef GETASSIGN_DERIVS_ETR_H
#define GETASSIGN_DERIVS_ETR_H

#include "../BinaryCalculations.hpp"
#include "../Core.hpp"
#include "../UnaryCalculations.hpp"
#include <cstddef>
#include <type_traits>

namespace etr {

#ifdef DERIV_ETR

template <typename T> inline auto get_deriv(T &&v) {
  Vec<double, Buffer<double>> ret{v.deriv};
  return ret;
}

// TODO: add missing get_deriv for int etc.
inline auto get_deriv(const double v) {
  Vec<double, Buffer<double>> ret(SI{1});
  return ret;
}

template <typename T>
  requires IsSubVec<T>
inline auto get_deriv(T &&v) {
  Buffer<double> deriv(v.size());
  for (size_t i = 0; i < deriv.size(); i++) {
    deriv[i] = v.d.get_deriv(i);
  }
  Vec<double, Buffer<double>> ret{deriv};
  ret.indep_var = v.indep_var;
  return ret;
}

template <typename T>
  requires IsVec<T>
inline auto set_indep(T &v) {
  v.indep_var = true;
  v.deriv.resize(v.d.size());
  v.deriv.fill(1.0);
}

template <typename T>
  requires IsSubVec<T>
inline auto set_indep(T &&v) {
  for (std::size_t i = 0; i < v.size(); i++) {
    v.d.get_deriv(i) = 1.0;
  }
}

template <typename T>
  requires IsVec<T>
inline auto unset_indep(T &v) {
  v.indep_var = true;
  v.deriv.resize(v.d.size());
  v.deriv.fill(0.0);
}

template <typename T>
  requires IsSubVec<T>
inline auto unset_indep(T &&v) {
  for (std::size_t i = 0; i < v.size(); i++) {
    v.d.get_deriv(i) = 0.0;
  }
}

template <typename L, typename Expr, typename ExprDeriv>
inline void assign(L &l, const Expr &expr, const ExprDeriv &r) {
  if constexpr (std::is_arithmetic_v<ExprDeriv>) {
    l.deriv.resize(1);
    l.deriv[0] = r;
  } else {
    using DataTypeOtherVec = typename etr::ExtractDataType<
        std::remove_reference_t<decltype(r)>>::RetType;
    l.temp.resize(r.size());
    for (std::size_t i = 0; i < r.size(); i++) {
      if constexpr (is<DataTypeOtherVec, double>) {
        l.temp[i] = r[i];
      } else {
        l.temp[i] = static_cast<double>(r[i]);
      }
    }
    l.deriv.resize(r.size());
    for (std::size_t i = 0; i < l.temp.size(); i++) {
      l.deriv[i] = l.temp[i];
    }
  }
  l = expr;
}

template <typename L, typename Expr, typename ExprDeriv>
  requires IsSubVec<L>
inline void assign(L &&l, const Expr &expr, const ExprDeriv &r) {

  if constexpr (std::is_arithmetic_v<ExprDeriv>) {
    l.deriv.resize(1);
    l.deriv[0] = r;
  } else {
    using DataTypeOtherVec = typename etr::ExtractDataType<
        std::remove_reference_t<decltype(r)>>::RetType;
    l.temp.resize(r.size());
    for (std::size_t i = 0; i < r.size(); i++) {
      if constexpr (is<DataTypeOtherVec, double>) {
        l.temp[i] = r[i];
      } else {
        l.temp[i] = static_cast<double>(r[i]);
      }
    }
    for (std::size_t i = 0; i < l.temp.size(); i++) {
      l.d.get_deriv(i) = l.temp[i];
    }
  }
  l = expr;
}

#endif

} // namespace etr

#endif
