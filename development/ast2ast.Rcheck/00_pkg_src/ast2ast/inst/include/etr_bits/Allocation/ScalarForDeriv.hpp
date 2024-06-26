#ifndef SCALAR_DERIV_ETR_H
#define SCALAR_DERIV_ETR_H

#include "../Core.hpp"

namespace etr {

template <int Idx, typename AV, typename T>
  requires std::is_arithmetic_v<T>
inline auto scalarDeriv(AV &av, T s) {
  av.varConstants[Idx].resize(1);
  av.varConstants[Idx][0] = s;
  Vec<T, VarPointer<decltype(av), Idx, -1, ConstantTypeTrait>,
      ConstantTypeTrait>
      ret(av);
  return ret;
}

} // namespace etr

#endif
