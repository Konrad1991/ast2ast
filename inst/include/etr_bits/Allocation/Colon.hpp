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
#include "../BufferVector.hpp"
#include "../Core/Traits.hpp"
#include "AllocationUtils.hpp"
#include <type_traits>

namespace etr {

template <typename T> inline auto colonInternal(T start, T end) {
  if (start < end) {
    std::size_t length = convertSize(end - start + 1);
    Vec<T, Buffer<T, RBufTrait>, RVecTrait> ret(SI{length});
    std::size_t counter = 0;
    while (start <= end) {
      ret.d.p[counter] = start;
      start++;
      counter++;
    }
    return ret;
  } else {
    std::size_t length = convertSize(start - end + 1);
    Vec<T, Buffer<T, RBufTrait>, RVecTrait> ret(SI{length});
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
  requires std::is_arithmetic_v<A> && std::is_arithmetic_v<O>
inline auto colon(A start, O end) {
  using DataType = typename std::common_type<A, O>::type;
  if constexpr (std::is_same_v<A, DataType> && std::is_same_v<O, DataType>) {
    return colonInternal(start, end);
  } else if constexpr (!std::is_same_v<A, DataType> &&
                       std::is_same_v<O, DataType>) {
    return colonInternal(static_cast<DataType>(start), end);
  } else if constexpr (std::is_same_v<A, DataType> &&
                       !std::is_same_v<O, DataType>) {
    return colonInternal(start, static_cast<DataType>(end));
  }
}

template <typename A, typename O>
  requires IsVec<A> && std::is_arithmetic_v<O>
inline auto colon(A &start, O end) {
  warn<"expression has more than one element only the first is used">(
      start.size() == 1);
  using DataTypeA = typename ExtractDataType<A>::RetType;
  using DataType = typename std::common_type<DataTypeA, O>::type;
  if constexpr (std::is_same_v<DataTypeA, DataType> &&
                std::is_same_v<O, DataType>) {
    return colonInternal(start[0], end);
  } else if constexpr (!std::is_same_v<DataTypeA, DataType> &&
                       std::is_same_v<O, DataType>) {
    return colonInternal(static_cast<DataType>(start[0]), end);
  } else if constexpr (std::is_same_v<DataTypeA, DataType> &&
                       !std::is_same_v<O, DataType>) {
    return colonInternal(start[0], static_cast<DataType>(end));
  }
}

template <typename A, typename O>
  requires(IsVecRorCalc<A> || IsVec<A>) && std::is_arithmetic_v<O>
inline auto colon(const A &start, O end) {
  warn<"expression has more than one element only the first is used">(
      start.size() == 1);
  using DataTypeA = typename ExtractDataType<A>::RetType;
  using DataType = typename std::common_type<DataTypeA, O>::type;
  if constexpr (std::is_same_v<DataTypeA, DataType> &&
                std::is_same_v<O, DataType>) {
    DataType temp = start[0];
    return colonInternal(temp, end);
  } else if constexpr (!std::is_same_v<DataTypeA, DataType> &&
                       std::is_same_v<O, DataType>) {
    DataType temp = static_cast<DataType>(start[0]);
    return colonInternal(temp, end);
  } else if constexpr (std::is_same_v<DataTypeA, DataType> &&
                       !std::is_same_v<O, DataType>) {
    DataType temp = start[0];
    return colonInternal(temp, static_cast<DataType>(end));
  }
}

template <typename A, typename O>
  requires std::is_arithmetic_v<A> && IsVec<O>
inline auto colon(A start, O &end) {
  warn<"expression has more than one element only the first is used">(
      end.size() == 1);

  using DataTypeO = typename ExtractDataType<O>::RetType;
  using DataType = typename std::common_type<A, DataTypeO>::type;
  if constexpr (std::is_same_v<A, DataType> &&
                std::is_same_v<DataTypeO, DataType>) {
    return colonInternal(start, end[0]);
  } else if constexpr (!std::is_same_v<A, DataType> &&
                       std::is_same_v<DataTypeO, DataType>) {
    return colonInternal(static_cast<DataType>(start), end[0]);
  } else if constexpr (std::is_same_v<A, DataType> &&
                       !std::is_same_v<DataTypeO, DataType>) {
    return colonInternal(start, static_cast<DataType>(end[0]));
  }
}

template <typename A, typename O>
  requires std::is_arithmetic_v<A> && (IsVecRorCalc<O> || IsVec<O>)
inline auto colon(A start, const O &end) {
  warn<"expression has more than one element only the first is used">(
      end.size() == 1);
  using DataTypeO = typename ExtractDataType<O>::RetType;
  using DataType = typename std::common_type<A, DataTypeO>::type;
  if constexpr (std::is_same_v<A, DataType> &&
                std::is_same_v<DataTypeO, DataType>) {
    DataType temp = end[0];
    return colonInternal(start, temp);
  } else if constexpr (!std::is_same_v<A, DataType> &&
                       std::is_same_v<DataTypeO, DataType>) {
    DataType temp = end[0];
    return colonInternal(static_cast<DataType>(start), temp);
  } else if constexpr (std::is_same_v<A, DataType> &&
                       !std::is_same_v<DataTypeO, DataType>) {
    DataType temp = end[0];
    return colonInternal(start, static_cast<DataType>(temp));
  }
}

template <typename A, typename O>
  requires IsVec<A> && IsVec<O>
inline auto colon(A &start, O &end) {
  warn<"expression has more than one element only the first is used">(
      start.size() == 1);
  warn<"expression has more than one element only the first is used">(
      end.size() == 1);
  using DataTypeA = typename ExtractDataType<A>::RetType;
  using DataTypeO = typename ExtractDataType<O>::RetType;
  using DataType = typename std::common_type<DataTypeA, DataTypeO>::type;
  if constexpr (std::is_same_v<DataTypeA, DataType> &&
                std::is_same_v<DataTypeO, DataType>) {
    return colonInternal(start[0], end[0]);
  } else if constexpr (!std::is_same_v<DataTypeA, DataType> &&
                       std::is_same_v<DataTypeO, DataType>) {
    return colonInternal(static_cast<DataType>(start[0]), end[0]);
  } else if constexpr (std::is_same_v<DataTypeA, DataType> &&
                       !std::is_same_v<DataTypeO, DataType>) {
    return colonInternal(start[0], static_cast<DataType>(end[0]));
  }
}

template <typename A, typename O>
  requires(IsVecRorCalc<A> || IsVec<A>) && IsVec<O>
inline auto colon(const A &start, O &end) {
  warn<"expression has more than one element only the first is used">(
      start.size() == 1);
  warn<"expression has more than one element only the first is used">(
      end.size() == 1);
  using DataTypeA = typename ExtractDataType<A>::RetType;
  using DataTypeO = typename ExtractDataType<O>::RetType;
  using DataType = typename std::common_type<DataTypeA, DataTypeO>::type;
  if constexpr (std::is_same_v<DataTypeA, DataType> &&
                std::is_same_v<DataTypeO, DataType>) {
    DataType tempStart = start[0];
    DataType tempEnd = end[0];
    return colonInternal(tempStart, tempEnd);
  } else if constexpr (!std::is_same_v<DataTypeA, DataType> &&
                       std::is_same_v<DataTypeO, DataType>) {
    DataType tempStart = start[0];
    DataType tempEnd = end[0];
    return colonInternal(static_cast<DataType>(tempStart), tempEnd);
  } else if constexpr (std::is_same_v<DataTypeA, DataType> &&
                       !std::is_same_v<DataTypeO, DataType>) {
    DataType tempStart = start[0];
    DataType tempEnd = end[0];
    return colonInternal(tempStart, static_cast<DataType>(tempEnd));
  }
}

template <typename A, typename O>
  requires IsVec<A> && (IsVecRorCalc<O> || IsVec<O>)
inline auto colon(A &start, const O &end) {
  warn<"expression has more than one element only the first is used">(
      start.size() == 1);
  warn<"expression has more than one element only the first is used">(
      end.size() == 1);
  using DataTypeA = typename ExtractDataType<A>::RetType;
  using DataTypeO = typename ExtractDataType<O>::RetType;
  using DataType = typename std::common_type<DataTypeA, DataTypeO>::type;
  if constexpr (std::is_same_v<DataTypeA, DataType> &&
                std::is_same_v<DataTypeO, DataType>) {
    DataType tempEnd = end[0];
    return colonInternal(start[0], tempEnd);
  } else if constexpr (!std::is_same_v<DataTypeA, DataType> &&
                       std::is_same_v<DataTypeO, DataType>) {
    DataType tempEnd = end[0];
    return colonInternal(static_cast<DataType>(start[0]), tempEnd);
  } else if constexpr (std::is_same_v<DataTypeA, DataType> &&
                       !std::is_same_v<DataTypeO, DataType>) {
    DataType tempEnd = end[0];
    return colonInternal(start[0], static_cast<DataType>(tempEnd));
  }
}

template <typename A, typename O>
  requires(IsVec<A> || IsVecRorCalc<A>) && (IsVecRorCalc<O> || IsVec<O>)
inline auto colon(const A &start, const O &end) {
  warn<"expression has more than one element only the first is used">(
      start.size() == 1);
  warn<"expression has more than one element only the first is used">(
      end.size() == 1);
  using DataTypeA = typename ExtractDataType<A>::RetType;
  using DataTypeO = typename ExtractDataType<O>::RetType;
  using DataType = typename std::common_type<DataTypeA, DataTypeO>::type;
  if constexpr (std::is_same_v<DataTypeA, DataType> &&
                std::is_same_v<DataTypeO, DataType>) {
    DataType tempStart = start[0];
    DataType tempEnd = end[0];
    return colonInternal(tempStart, tempEnd);
  } else if constexpr (!std::is_same_v<DataTypeA, DataType> &&
                       std::is_same_v<DataTypeO, DataType>) {
    DataType tempStart = start[0];
    DataType tempEnd = end[0];
    return colonInternal(static_cast<DataType>(tempStart), tempEnd);
  } else if constexpr (std::is_same_v<DataTypeA, DataType> &&
                       !std::is_same_v<DataTypeO, DataType>) {
    DataType tempStart = start[0];
    DataType tempEnd = end[0];
    return colonInternal(tempStart, static_cast<DataType>(tempEnd));
  }
}

} // namespace etr

#endif
