#ifndef MATRIX_ETR_H
#define MATRIX_ETR_H

#include "AllocationUtils.hpp"
#include <cstddef>
#include <type_traits>

namespace etr {

// NOTE: the matrix calls with only one or two arguments are "filled up" with
// the default values at the R level

// NOTE: the second argument is always handled as constant object. As only the
// first element is used and the remaining elements are ignored. Therefore, this
// simplification is added as it is unlikely that large vectors are used as a
// second argument.

/*
TODO: Matrix missing implementation with 3 args
   Var1  Var2  Var3
1 arith arith arith done
2   vec arith arith done
3 arith   vec arith
4   vec   vec arith
5 arith arith   vec
6   vec arith   vec
7 arith   vec   vec
8   vec   vec   vec
*/

template <typename V, typename R, typename C>
inline auto matrix(const V &&inp, const R &&nrows, const C &&ncols) {
  if constexpr (std::is_arithmetic_v<R> && std::is_arithmetic_v<C>) {
    if constexpr (std::is_arithmetic_v<V>) {
      Vec<V, Buffer<V, BufferTrait, RBufTrait>, RVecTrait> ret(
          convertSize(nrows), convertSize(ncols));
      ret.d.fill(inp);
      return ret;
    } else {
      using DataType = ExtractDataType<std::remove_reference_t<V>>::RetType;
      Vec<DataType, Buffer<DataType, BufferTrait, RBufTrait>, RVecTrait> ret(
          convertSize(nrows), convertSize(ncols));
      ret.d.fill(inp);
      return ret;
    }
  } else if constexpr (!std::is_arithmetic_v<R> && std::is_arithmetic_v<C>) {
    if constexpr (std::is_arithmetic_v<V>) {
      warn(nrows.size() > 1, "nrows in fct matrix has more than one element. "
                             "Only the first one is used");
      Vec<V, Buffer<V, BufferTrait, RBufTrait>, RVecTrait> ret(
          convertSize(nrows[0]), convertSize(ncols));
      ret.d.fill(inp);
      return ret;
    } else {
      using DataType = ExtractDataType<std::remove_reference_t<V>>::RetType;
      warn(nrows.size() > 1, "nrows in fct matrix has more than one element. "
                             "Only the first one is used");
      Vec<DataType, Buffer<DataType, BufferTrait, RBufTrait>, RVecTrait> ret(
          convertSize(nrows[0]), convertSize(ncols));
      ret.d.fill(inp);
      return ret;
    }
  } else if constexpr (std::is_arithmetic_v<R> && !std::is_arithmetic_v<C>) {
    if constexpr (std::is_arithmetic_v<V>) {
      warn(ncols.size() > 1, "ncols in fct matrix has more than one element. "
                             "Only the first one is used");
      Vec<V, Buffer<V, BufferTrait, RBufTrait>, RVecTrait> ret(
          convertSize(nrows), convertSize(ncols[0]));
      ret.d.fill(inp);
      return ret;
    } else {
      using DataType = ExtractDataType<std::remove_reference_t<V>>::RetType;
      warn(ncols.size() > 1, "ncols in fct matrix has more than one element. "
                             "Only the first one is used");
      Vec<DataType, Buffer<DataType, BufferTrait, RBufTrait>, RVecTrait> ret(
          convertSize(nrows), convertSize(ncols[0]));
      ret.d.fill(inp);
      return ret;
    }
  } else if constexpr (!std::is_arithmetic_v<R> && !std::is_arithmetic_v<C>) {
    if constexpr (std::is_arithmetic_v<V>) {
      warn(ncols.size() > 2, "ncols in fct matrix has more than one element. "
                             "Only the first one is used");
      warn(nrows.size() > 1, "nrows in fct matrix has more than one element. "
                             "Only the first one is used");
      Vec<V, Buffer<V, BufferTrait, RBufTrait>, RVecTrait> ret(
          convertSize(nrows[0]), convertSize(ncols[0]));
      ret.d.fill(inp);
      return ret;
    } else {
      using DataType = ExtractDataType<std::remove_reference_t<V>>::RetType;
      warn(ncols.size() > 2, "ncols in fct matrix has more than one element. "
                             "Only the first one is used");
      warn(nrows.size() > 1, "nrows in fct matrix has more than one element. "
                             "Only the first one is used");
      Vec<DataType, Buffer<DataType, BufferTrait, RBufTrait>, RVecTrait> ret(
          convertSize(nrows[0]), convertSize(ncols[0]));
      ret.d.fill(inp);
      return ret;
    }
  } else {
    static_assert(sizeof(V) == 0,
                  "Unknown input for subsetting of matrix found");
  }
}

} // namespace etr

#endif
