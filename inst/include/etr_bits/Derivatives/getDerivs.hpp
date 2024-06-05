#ifndef GET_DERIVS_ETR_H
#define GET_DERIVS_ETR_H

#include "../BinaryCalculations.hpp"
#include "../Core.hpp"
#include "../UnaryCalculations.hpp"
#include <cstddef>

namespace etr {

// NOTE: this fct extracts the derivative
template <typename T>
  requires IsVariableTypeTrait<T>
inline auto get_derivs(T &v) {
  Vec<BaseType> res(SI{v.size()});
  for (size_t i = 0; i < v.d.getSize(v.d.AllVarsRef); i++) {
    res[i] = v.d.getDeriv(v.d.AllVarsRef, i);
  }
  return res;
}

// TODO: find a better way instead of copying.
} // namespace etr

#endif
