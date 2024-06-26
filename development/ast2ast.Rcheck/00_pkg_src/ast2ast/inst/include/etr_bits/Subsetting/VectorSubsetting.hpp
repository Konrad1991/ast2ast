#ifndef VECTOR_SUBSETTING_HPP
#define VECTOR_SUBSETTING_HPP

#include "UtilsSubsetting.hpp"
#include <type_traits>

namespace etr {
/*
bool
int
double
vec
vecbool
*/

template <typename T, typename I>
inline void calcIndVector(T &vec, Indices &ind, const I *idx) {
  if constexpr (is<I, bool>) {
    if (*idx) {
      ind.resize(vec.size());
      for (std::size_t i = 0; i < vec.size(); i++)
        ind[i] = i;
      return;
    } else {
      ass<"Variable[FALSE] subsetting is not supported. Sorry">(false);
      return;
    }
  } else if constexpr (is<I, int>) {
    ind.resize(1);
    ind[0] = *idx - 1;
    ass<"invalid index argument">(ind[0] >= 0);
    return;
  } else if constexpr (is<I, double>) {
    ind.resize(1);
    ind[0] = convertSizeSubsetting(*idx) - 1;
    return;
  } else if constexpr (IsAV<I>) {

    using IndexType = typename ExtractDataType<I>::RetType;
    if constexpr (is<IndexType, bool>) {
      std::size_t sizeTrue = 0;
      for (std::size_t i = 0; i < vec.size(); i++)
        if ((*idx)[i % idx->size()])
          sizeTrue++;
      ind.resize(sizeTrue);
      std::size_t counter = 0;
      for (std::size_t i = 0; i < vec.size(); i++) {
        if ((*idx)[i % idx->size()]) {
          ind[counter] = i;
          counter++;
        }
      }
    } else if constexpr (is<IndexType, int>) {
      ind.resize(idx->size());
      for (std::size_t i = 0; i < idx->size(); i++) {
        std::size_t sizeTIdx = (*idx)[i] - 1;
        ind[i] = sizeTIdx;
      }
    } else if constexpr (is<IndexType, double>) {
      ind.resize(idx->size());
      for (std::size_t i = 0; i < idx->size(); i++) {
        std::size_t sizeTIdx = static_cast<std::size_t>((*idx)[i]) - 1;
        ind[i] = sizeTIdx;
      }
    } else {
      static_assert(sizeof(T) == 0, "Unknown type of index variable");
    }
  }
}

template <typename V, typename I>
  requires IsVec<V>
inline auto subset(V &vec, I &&idx) {
  using DataType = typename ExtractDataType<V>::RetType;
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndVector(vec, sub.ind, &idx);
  return Vec<DataType, decltype(convertSubset(vec)), SubVecTrait>(
      std::move(sub));
}

template <typename V, typename I>
  requires(IsRVec<V> || IsSubVec<V> || OperationVec<V>)
inline auto subset(V &&vec, I &&idx) {
  using DataType = typename ExtractDataType<V>::RetType;
  Subset<const decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndVector(vec, sub.ind, &idx);
  return Vec<DataType, decltype(convertSubsetConst(vec)), SubVecTrait>(
      std::move(sub));
}

} // namespace etr

#endif // !DEBUG
