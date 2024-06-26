#ifndef ALLOCATION_UTILS_ETR_H
#define ALLOCATION_UTILS_ETR_H

/*
        Var1       Var2
1 arithmetic arithmetic  done
2        Vec arithmetic  done
3  Operation arithmetic  done
4 arithmetic        Vec  done
5        Vec        Vec  done
6  Operation        Vec  done
7 arithmetic  Operation  done
8        Vec  Operation
9  Operation  Operation

In Operation also Rvecs are included
*/
#include "../BufferVector.hpp"
#include "../Core.hpp"

namespace etr {

template <typename T> inline std::size_t convertSize(const T &inp) {
  if constexpr (std::is_same_v<T, std::size_t>) {
    ass<"invalid times argument">(inp >= 1);
    return inp;
  } else if constexpr (std::is_floating_point_v<T>) {
    warn(isDoubleInt(inp),
         "The provided size is a floating-point number with non-zero decimal "
         "places. It has been floored to the nearest integer.");
    ass<"invalid times argument">(inp >= 1.0);
    return static_cast<std::size_t>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    ass<"invalid times argument">(inp >= 1);
    return static_cast<std::size_t>(inp);
  } else if constexpr (IsVecRorCalc<T>) { // TODO: check does this really
                                          // work for normal l vectors?
    ass<"invalid times argument">(inp.size() == 1 && inp[0] >= 1);
    return static_cast<std::size_t>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in rep");
  }
}

template <typename T> inline auto createLVec(T s) {
  std::size_t size = convertSize(s);
  ass<"invalid length argument">(size > 0);
  return Vec<BaseType, Buffer<BaseType, BufferTrait, RBufTrait>, RVecTrait>(
      SI{size});
}

template <typename T, typename VecType = BaseType> inline auto createRVec(T s) {
  std::size_t size = convertSize(s);
  ass<"invalid length argument">(size > 0);
  return Vec<VecType, Buffer<BaseType, BufferTrait, RBufTrait>, RVecTrait>(
      SI{size});
}

} // namespace etr

#endif
