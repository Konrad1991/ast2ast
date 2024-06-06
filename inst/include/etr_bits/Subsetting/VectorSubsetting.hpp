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
      ass(false, "Variable[FALSE] subsetting is not supported. Sorry");
      return;
    }
  } else if constexpr (is<I, int>) {
    ind.resize(1);
    ind[0] = *idx - 1;
    ass(ind[0] >= 0, "invalid index argument");
    return;
  } else if constexpr (is<I, double>) {
    ind.resize(1);
    ind[0] = convertSizeSubsetting(*idx) - 1;
    return;
  } else if constexpr (IsAV<I>) {

    using IndexType = ExtractDataType<I>::RetType;
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
  using DataType = ExtractDataType<V>::RetType;
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndVector(vec, sub.ind, &idx);
  return Vec<DataType, decltype(convertSubset(vec)), SubVecTrait>(
      std::move(sub));
}

template <typename V, typename I>
  requires(IsRVec<V> || IsSubVec<V> || OperationVec<V>)
inline auto subset(V &&vec, I &&idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<const decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndVector(vec, sub.ind, &idx);
  return Vec<DataType, decltype(convertSubsetConst(vec)), SubVecTrait>(
      std::move(sub));
}

/*
// NOTE: bool
template <typename T, typename I>
inline void calcIndBool(T &vec, Indices &ind, const I *idx) {
  if (*idx) {
    ind.resize(vec.size());
    for (std::size_t i = 0; i < vec.size(); i++)
      ind[i] = i;
    return;
  } else {
    ass(false, "Variable[FALSE] subsetting is not supported. Sorry");
    return;
  }
}

template <typename V, typename I>
  requires(IsVec<V> && std::is_same_v<std::remove_reference_t<I>, bool>)
inline auto subset(V &vec, const I idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndBool(vec, sub.ind, &idx);
  sub.setMatrix(false, 0, 0);
  return Vec<DataType, decltype(convertSubset(vec)), SubVecTrait>(
      std::move(sub)); // TODO: the buffer/borrow does not have the triat
                       // RBuffer is this a problem?
}

template <typename V>
  requires(IsRVec<V> || IsSubVec<V> || OperationVec<V>)
inline const auto subset(V &&vec, const bool idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<const decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndBool(vec, sub.ind, &idx);
  sub.setMatrix(false, 0, 0);
  return Vec<DataType, decltype(convertSubsetConst(vec)), SubVecTrait>(
      std::move(sub));
}

// NOTE: int
template <typename T, typename I>
inline void calcIndInt(T &vec, Indices &ind, const I *idx) {
  ind.resize(1);
  ind[0] = *idx - 1;
  ass(ind[0] >= 0, "invalid index argument");
  return;
}

template <typename V, typename I>
  requires(IsVec<V> && std::is_same_v<std::remove_reference_t<I>, int>)
inline auto subset(V &vec, const I idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndInt(vec, sub.ind, &idx);
  sub.setMatrix(false, 0, 0);
  return Vec<DataType, decltype(convertSubset(vec)), SubVecTrait>(
      std::move(sub));
}

template <typename V>
  requires(IsRVec<V> || IsSubVec<V> || OperationVec<V>)
inline const auto subset(V &&vec, const int idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<const decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndInt(vec, sub.ind, &idx);
  sub.setMatrix(false, 0, 0);
  return Vec<DataType, decltype(convertSubsetConst(vec)), SubVecTrait>(
      std::move(sub));
}

// NOTE: double
template <typename T, typename I>
inline void calcIndDouble(T &vec, Indices &ind, const I *idx) {
  ind.resize(1);
  ind[0] = convertSizeSubsetting(*idx) - 1;
  return;
}

template <typename V, typename I>
  requires(IsVec<V> && std::is_same_v<std::remove_reference_t<I>, double>)
inline auto subset(V &vec, const I idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndDouble(vec, sub.ind, &idx);
  sub.setMatrix(false, 0, 0);
  return Vec<DataType, decltype(convertSubset(vec)), SubVecTrait>(
      std::move(sub)); // TODO: the buffer/borrow does not have the triat
                       // RBuffer is this a problem?
}

template <typename V>
  requires(IsRVec<V> || IsSubVec<V> || OperationVec<V>)
inline const auto subset(V &&vec, const double idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<const decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndDouble(vec, sub.ind, &idx);
  sub.setMatrix(false, 0, 0);
  return Vec<DataType, decltype(convertSubsetConst(vec)), SubVecTrait>(
      std::move(sub));
}

// NOTE: vector
template <typename T, typename I>
inline void calcIndVec(T &vec, Indices &ind, const I *idx) {
  using vecTrait = std::remove_reference<decltype(*idx)>::type::TypeTrait;
  using whichType = std::remove_reference<decltype(*idx)>::type::Type;
  using isBool = std::is_same<whichType, bool>;
  if constexpr (isBool::value) {
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
  } else if constexpr (std::is_same_v<whichType, BaseType>) {
    ind.resize(idx->size());
    for (std::size_t i = 0; i < idx->size(); i++) {
      std::size_t sizeTIdx = static_cast<std::size_t>((*idx)[i]) - 1;
      ind[i] = sizeTIdx;
    }
  } else if constexpr (std::is_same_v<whichType, int>) {
    ind.resize(idx->size());
    for (std::size_t i = 0; i < idx->size(); i++) {
      std::size_t sizeTIdx = (*idx)[i] - 1;
      ind[i] = sizeTIdx;
    }
  } else {
    static_assert(sizeof(T) == 0, "Unknown type of index variable");
  }
}

template <typename V, typename I>
  requires(IsVec<V> && IsRVec<I> || IsSubVec<I> || OperationVec<I>)
inline auto subset(V &vec, I &&idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndVec(vec, sub.ind, &idx);
  sub.setMatrix(false, 0, 0);
  return Vec<DataType, decltype(convertSubset(vec)), SubVecTrait>(
      std::move(sub));
}

template <typename V, typename I>
  requires(IsVec<V> && IsVec<I>)
inline auto subset(V &vec, I &idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndVec(vec, sub.ind, &idx);
  sub.setMatrix(false, 0, 0);
  return Vec<DataType, decltype(convertSubset(vec)), SubVecTrait>(
      std::move(sub));
}

template <typename V, typename I>
  requires(IsRVec<V> || IsSubVec<V> || OperationVec<V> && IsRVec<I> ||
           IsSubVec<I> || OperationVec<I>)
inline const auto subset(V &&vec, I &&idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<const decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndVec(vec, sub.ind, &idx);
  sub.setMatrix(false, 0, 0);
  return Vec<DataType, decltype(convertSubset(vec)), SubVecTrait>(
      std::move(sub));
}

template <typename V, typename I>
  requires(IsRVec<V> || IsSubVec<V> || OperationVec<V> && IsVec<I>)
inline const auto subset(V &&vec, I &&idx) {
  using DataType = ExtractDataType<V>::RetType;
  Subset<const decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcIndVec(vec, sub.ind, &idx);
  sub.setMatrix(false, 0, 0);
  return Vec<DataType, decltype(convertSubset(vec)),
             SubVecTrait>( // TODO: check whether here convertSubsetConst is
                           // required
      std::move(sub));
}
*/
} // namespace etr

#endif // !DEBUG
