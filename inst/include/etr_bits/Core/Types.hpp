// TODO: add the iterator to each class: Buffer, Subset, Binary, Unary, Borrow, BorrowSEXP
// Directly use the iterator methods begin, end etc. in Vec
// Replace BaseStore with Buffer.
// Each class has to possess RetType and Trait


#ifndef TYPES_ETR_H
#define TYPES_ETR_H

#include "Header.hpp"
#include "TemplateUtils.hpp"
#include "Traits.hpp"
#include "Utils.hpp"
#include <type_traits>

namespace etr {
template <typename T, typename BaseTrait = BaseStoreTrait> struct BaseStore;
template <typename T = double, typename BorrowSEXPTrait = BorrowSEXPTrait>
struct BorrowSEXP;
template <typename T, typename BorrowTrait = BorrowTrait> struct Borrow;
template <typename T, typename SubsetTrait = SubsetTrait> struct Subset;
template <typename L, typename R, typename Trait> struct SubsetClass;
template <typename T, typename BufferTrait = LBufferTrait>
struct Buffer;
template <typename T, typename R = Buffer<T>>
struct Vec;
template <typename L, typename R, typename BTrait = BinaryTrait>
struct BinaryOperation;
template <typename I, typename UTrait = UnaryTrait>
struct UnaryOperation;

// Extract RetType
template <typename T> struct ExtractDataType;
template <typename T, typename R>
struct ExtractDataType<Vec<T, R>> {
  using RetType = T;
};
template <typename T, typename R>
struct ExtractDataType<const Vec<T, R>> {
  using RetType = T const;
};
template <typename T>
using ExtractedTypeData = typename ExtractDataType<T>::RetType;


// Extract RType
template <typename T> struct ExtractRType;
template <typename T, typename R>
struct ExtractRType<Vec<T, R>> {
  using RType = R;
};
template <typename T, typename R>
struct ExtractRType<const Vec<T, R>> {
  using RType = R const;
};
template <typename T>
using ExtractedRType= typename ExtractRType<T>::RType;

// Extract RetType from Buffer, etc.
template <typename T> struct ExtractDataType;
template <typename T, typename Trait>
struct ExtractDataType<Buffer<T, Trait>> {
  using RetType = T;
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
  requires IS<T, double>
constexpr doubleWrapper<DoubleTrait> convert(const T &obj) {
  return doubleWrapper<DoubleTrait>(obj);
}

template <typename T>
  requires IS<T, int>
constexpr doubleWrapper<IntTrait> convert(const T &obj) {
  return doubleWrapper<IntTrait>(obj);
}

template <typename T>
  requires IS<T, bool>
constexpr doubleWrapper<BoolTrait> convert(const T &obj) {
  return doubleWrapper<BoolTrait>(obj);
}

template <typename T> constexpr T convert(const T &obj) {
  return obj;
  // return std::forward(obj);
}

} // namespace etr

#endif
