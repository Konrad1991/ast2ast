#ifndef TYPES_ETR_H
#define TYPES_ETR_H

#include "Header.hpp"
#include "Traits.hpp"
#include "Utils.hpp"
#include <type_traits>

namespace etr {
template <typename T, typename BaseTrait = BaseStoreTrait> struct BaseStore;
template <typename T = double, typename BorrowSEXPTrait = BorrowSEXPTrait>
struct BorrowSEXP;
template <typename T, typename BorrowTrait = BorrowTrait> struct Borrow;
template <typename T, typename SubsetTrait = SubsetTrait> struct Subset;
template <typename T, typename Trait = BufferTrait,
          typename CTrait = VariableTrait>
struct Buffer;
template <typename T, typename R = Buffer<T>, typename Trait = VectorTrait>
struct Vec;
template <typename L, typename R, typename Trait = BinaryTrait,
          typename CTrait = BinaryTrait>
struct BinaryOperation;
template <typename I, typename Trait = UnaryTrait, typename CTrait = UnaryTrait>
struct UnaryOperation;

template <typename L, typename R, typename Trait = BinaryTrait,
          typename CTrait = BinaryTrait>
struct BinaryOperationDeriv;

template <typename T> struct ExtractDataType;
template <typename T, typename R, typename Trait>
struct ExtractDataType<Vec<T, R, Trait>> {
  using RetType = T;
};
template <typename T, typename R, typename Trait>
struct ExtractDataType<const Vec<T, R, Trait>> {
  using RetType = T const;
};
template <typename T>
using ExtractedTypeData = typename ExtractDataType<T>::RetType;

template <typename T> struct ExtractDataType;
template <typename T, typename R, typename Trait>
struct ExtractDataType<Buffer<T, R, Trait>> {
  using RetType = T;
};
template <typename T, typename R, typename Trait>
struct ExtractDataType<const Buffer<T, R, Trait>> {
  using RetType = T const;
};

template <typename T, typename Trait> struct ExtractDataType<Borrow<T, Trait>> {
  using RetType = T;
};
template <typename T, typename Trait>
struct ExtractDataType<const Borrow<T, Trait>> {
  using RetType = T const;
};

#ifdef STANDALONE_ETR
#else
/*
template <> struct ExtractDataType<Rboolean> {
  using RetType = bool;
};
template <> struct ExtractDataType<const Rboolean> {
  using RetType = bool const;
};
*/
#endif

template <typename Trait = DoubleTrait> struct doubleWrapper {
  using TypeTrait = Trait;
  using Type = DoubleTrait;
  BaseType d;
};

template <typename T>
  requires std::is_same_v<T, double>
constexpr doubleWrapper<DoubleTrait> convert(const T &obj) {
  return doubleWrapper<DoubleTrait>(obj);
}

template <typename T>
  requires std::is_same_v<T, int>
constexpr doubleWrapper<IntTrait> convert(const T &obj) {
  return doubleWrapper<IntTrait>(obj);
}

template <typename T>
  requires std::is_same_v<T, bool>
constexpr doubleWrapper<BoolTrait> convert(const T &obj) {
  return doubleWrapper<BoolTrait>(obj);
}

template <typename T> constexpr T convert(const T &obj) {
  return obj;
  // return std::forward(obj);
}

template <typename L, typename R, typename LDeriv, typename RDeriv,
          typename Trait, typename OpTrait>
struct QuarternyType;

template <typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
struct BinaryType;

template <typename Deriv, typename Trait, typename OpTrait> struct UnaryType;

template <typename T, typename Trait = VariableTypeTrait> struct VariableType;

template <typename T, int Idx, int TypeIdx, typename Trait = VarPointerTrait>
struct VarPointer;

template <typename T, int Idx, typename Trait = ConstantTypeTrait>
struct Constants;

} // namespace etr

#endif
