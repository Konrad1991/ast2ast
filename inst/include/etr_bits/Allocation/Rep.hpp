#ifndef REP_ETR_H
#define REP_ETR_H

#include "../Core/Types.hpp"
#include "AllocationUtils.hpp"
#include <type_traits>

/*
        Var1       Var2
1 arithmetic arithmetic done
2        Vec arithmetic done
3  const Vec arithmetic done
4 arithmetic        Vec done
5        Vec        Vec done
6  const Vec        Vec done
7 arithmetic  const Vec done
8        Vec  const Vec done
9  const Vec  const Vec done

const Vec is an Operation
*/

namespace etr {

template <typename L, typename R>
inline auto repInternal(const L *inp, const R *s) {
  if constexpr (std::is_arithmetic_v<L> && std::is_arithmetic_v<R>) {
    std::size_t length = convertSize(*s);
    Vec<L, Buffer<L, BufferTrait, RBufTrait>, RVecTrait> ret(SI{length});
    ret.fill(*inp);
    return ret;
  } else if constexpr (!std::is_arithmetic_v<L> && std::is_arithmetic_v<R>) {
    std::size_t length = convertSize(*s) * inp->size();
    using DataType = ExtractDataType<L>::RetType;
    Vec<DataType, Buffer<DataType, BufferTrait, RBufTrait>, RVecTrait> ret(
        SI{length});
    std::size_t counter = 0;
    for (std::size_t i = 0; i < ret.size(); i++) {
      ret[i] = (*inp)[counter];
      counter++;
      if (counter >= inp->size())
        counter = 0;
    }
    return ret;
  } else if constexpr (std::is_arithmetic_v<L> && !std::is_arithmetic_v<R>) {
    warn(!(s->size() > 1),
         "times in fct rep has more than one element. Only the "
         "first one is used");
    std::size_t length = convertSize((*s)[0]);
    Vec<L, Buffer<L, BufferTrait, RBufTrait>, RVecTrait> ret(SI{length});
    ret.fill(*inp);
    return ret;
  } else if constexpr (!std::is_arithmetic_v<L> && !std::is_arithmetic_v<R>) {
    warn(!(s->size() > 1),
         "times in fct rep has more than one element. Only the "
         "first one is used");
    std::size_t length = convertSize((*s)[0]) * inp->size();
    using DataType = ExtractDataType<L>::RetType;
    Vec<DataType, Buffer<DataType, BufferTrait, RBufTrait>, RVecTrait> ret(
        SI{length});
    std::size_t counter = 0;
    for (std::size_t i = 0; i < ret.size(); i++) {
      ret[i] = (*inp)[counter];
      counter++;
      if (counter >= inp->size())
        counter = 0;
    }
    return ret;
  }
}

template <typename L, typename R> inline auto rep(L &&inp, R &&s) {
  return repInternal(&inp, &s);
}

template <typename L, typename R> inline auto rep(L &inp, R &s) {
  return repInternal(&inp, &s);
}

} // namespace etr

#endif
