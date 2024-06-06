#ifndef VECTOR_ETR_H
#define VECTOR_ETR_H

#include "AllocationUtils.hpp"

// TODO: implement vecotr_integer, vector_logical and vector_numeric

namespace etr {

template <typename T> inline auto vector_integer(const T &inp) {
  if constexpr (std::is_same_v<T, std::size_t>) {
    return createRVec<int>(inp);
  } else if constexpr (std::is_floating_point_v<T>) {
    return createRVec<int>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<int>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    return createRVec<int>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <typename T> inline auto vector_integer(T &inp) {
  if constexpr (std::is_same_v<T, std::size_t>) {
    return createRVec<int>(inp);
  } else if constexpr (std::is_floating_point_v<T>) {
    return createRVec<int>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<int>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    return createRVec<int>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <typename T> inline auto vector_logical(const T &inp) {
  if constexpr (std::is_same_v<T, std::size_t>) {
    return createRVec<int>(inp);
  } else if constexpr (std::is_floating_point_v<T>) {
    return createRVec<int>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<int>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    return createRVec<int>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <typename T> inline auto vector_logical(T &inp) {
  if constexpr (std::is_same_v<T, std::size_t>) {
    return createRVec<bool>(inp);
  } else if constexpr (std::is_floating_point_v<T>) {
    return createRVec<bool>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<bool>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    return createRVec<bool>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <typename T> inline auto vector_numeric(const T &inp) {
  if constexpr (std::is_same_v<T, std::size_t>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (std::is_floating_point_v<T>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    return createRVec<BaseType>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <typename T> inline auto vector_numeric(T &inp) {
  if constexpr (std::is_same_v<T, std::size_t>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (std::is_floating_point_v<T>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (std::is_integral_v<T>) {
    return createRVec<BaseType>(inp);
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    return createRVec<BaseType>(inp[0]);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

// TODO: remove typename AV. Instead use the AllVarsRef variable stored in inp
// THis has to be done for all functions where a constant is created
template <int Idx, typename AV, typename T>
inline auto vector_integer(AV &av, const T &inp) {
  using VecType = int;
  if constexpr (std::is_same_v<T, std::size_t>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_floating_point_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_integral_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    av.varConstants[Idx].resize(convertSize(inp[0]));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <int Idx, typename AV, typename T>
inline auto vector_integer(AV &av, T &inp) {
  using VecType = int;
  if constexpr (std::is_same_v<T, std::size_t>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_floating_point_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_integral_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    av.varConstants[Idx].resize(convertSize(inp[0]));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <int Idx, typename AV, typename T>
inline auto vector_logical(AV &av, const T &inp) {
  using VecType = bool;
  if constexpr (std::is_same_v<T, std::size_t>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_floating_point_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_integral_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    av.varConstants[Idx].resize(convertSize(inp[0]));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <int Idx, typename AV, typename T>
inline auto vector_logical(AV &av, T &inp) {
  using VecType = bool;
  if constexpr (std::is_same_v<T, std::size_t>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_floating_point_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_integral_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    av.varConstants[Idx].resize(convertSize(inp[0]));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <int Idx, typename AV, typename T>
inline auto vector_numeric(AV &av, const T &inp) {
  using VecType = BaseType;
  if constexpr (std::is_same_v<T, std::size_t>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_floating_point_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_integral_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    av.varConstants[Idx].resize(convertSize(inp[0]));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

template <int Idx, typename AV, typename T>
inline auto vector_numeric(AV &av, T &inp) {
  using VecType = BaseType;
  if constexpr (std::is_same_v<T, std::size_t>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_floating_point_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (std::is_integral_v<T>) {
    av.varConstants[Idx].resize(convertSize(inp));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else if constexpr (IsVecRorCalc<T> || IsVec<T>) {
    ass(inp.size() == 1, "invalid length argument");
    av.varConstants[Idx].resize(convertSize(inp[0]));
    Vec<VecType, VarPointer<decltype(av), Idx, -1>, ConstantTypeTrait> ret(av);
    return ret;
  } else {
    static_assert(sizeof(T) == 0, "Unsupported type in vector");
  }
}

} // namespace etr

#endif
