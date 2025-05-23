#ifndef COLON_ETR_H
#define COLON_ETR_H

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
#include "../DataStructures.hpp"
#include "../Core/Traits.hpp"
#include <type_traits>

namespace etr {

template<typename T>
inline auto ConvertValueColon(const T& obj) {
  using DecayedT = Decayed<T>;

  // TODO: adapt other static_assert messages in the same way
  static_assert(!IsBool<DecayedT>,
  "\n\n[etr::colon error]\n"
  "You passed a boolean value to the ':' operator.\n"
  "This is not allowed in R and not supported here.\n"
  "Please use integer or double values only.\n\n");

  if constexpr(IsArithV<DecayedT>) {
    return obj;
  } else {
    ass<": accepts only vector of length 1">(obj.size() == 1);
    return obj[0];
  }
}

template <typename DataType, typename S, typename E> inline auto colonInternal(S& start, E& end) {
  if (start < end) {
    std::size_t length = static_cast<std::size_t>(end - start + 1);
    ass<"invalid start or end values as argument to colon(:)">(length >= 1);
    Vec<DataType, Buffer<DataType, RBufferTrait>> ret(SI{length});
    std::size_t counter = 0;
    while (start <= end) {
      ret.d.p[counter] = start;
      start++;
      counter++;
    }
    return ret;
  } else {
    std::size_t length = static_cast<std::size_t>(start - end + 1);
    ass<"invalid start or end values as argument to colon(:)">(length >= 1);
    Vec<DataType, Buffer<DataType, RBufferTrait>> ret(SI{length});
    std::size_t counter = 0;
    while (end <= start) {
      ret.d.p[counter] = start;
      start--;
      counter++;
    }
    return ret;
  }
}

template <typename A, typename O>
inline auto colon(const A& start,const O& end) {
  auto s = ConvertValueColon(start);
  auto e = ConvertValueColon(end);
  using DataType = typename std::common_type<decltype(s), decltype(e)>::type;
  return colonInternal<DataType>(s, e);
}

} // namespace etr

#endif
