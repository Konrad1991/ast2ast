// TODO: add the iterator to each class: Buffer, Subset, Binary, Unary, Borrow, BorrowSEXP
// Directly use the iterator methods begin, end etc. in Vec
// Replace BaseStore with Buffer.
// Each class has to possess RetType and Trait


#ifndef TYPES_ETR_H
#define TYPES_ETR_H

#include "Header.hpp"
#include "Traits.hpp"
#include "Utils.hpp"
#include <type_traits>

namespace etr {


// Inner data structs
// -----------------------------------------------------------------------------------------------------------
template <typename T, typename  BufferTrait = LBufferTrait> struct Buffer;
template <typename T, typename BorrowTrait = BorrowTrait> struct Borrow;
template <typename L, typename R, typename Trait> struct SubsetClass;
template <typename L, typename R, typename BTrait = BinaryTrait>
struct BinaryOperation;
template <typename I, typename UTrait = UnaryTrait>
struct UnaryOperation;
/*
Each inner data struct requires:
- copy constructor
- copy assignment
- move constructor
- move assignment
- size
- im
- nr
- nc
- const auto& operator[]
- auto& operator[]
- begin
- end
Classes                     Status
Buffer                      Done
Borrow                      Done
BinaryOperation             Done
UnaryOperation              Done with constructors and assignments
SubsetClass                 Done with constructors and assignments

*/

// Outer data structs
// -----------------------------------------------------------------------------------------------------------
template <typename T, typename R = Buffer<T>>
struct Vec;
template <typename T, typename R = Buffer<T>>
struct Mat;

// Extract data type from inner data structs
// -----------------------------------------------------------------------------------------------------------
template <typename T> struct ExtractDataType;
template <typename T, typename Trait>
struct ExtractDataType<Buffer<T, Trait>> {
  using RetType = T;
};
template <typename T, typename Trait>
struct ExtractDataType<const Buffer<T, Trait>> {
  using RetType = T;
};
template <typename T, typename Trait>
struct ExtractDataType<Borrow<T, Trait>> {
  using RetType = T;
};
template <typename T, typename Trait>
struct ExtractDataType<const Borrow<T, Trait>> {
  using RetType = T const;
};
template <typename T, typename R, typename Trait>
struct ExtractDataType<SubsetClass<T, R, Trait>> {
  using RetType = T;
};
template <typename T, typename R, typename Trait>
struct ExtractDataType<const SubsetClass<T, R, Trait>> {
  using RetType = T;
};
template <typename T, typename Trait>
struct ExtractDataType<UnaryOperation<T, Trait>> {
  using RetType = T;
};
template <typename T, typename Trait>
struct ExtractDataType<const UnaryOperation<T, Trait>> {
  using RetType = T;
};
template <typename T, typename R, typename Trait>
struct ExtractDataType<BinaryOperation<T, R, Trait>> {
  using RetType = T;
};
template <typename T, typename R, typename Trait>
struct ExtractDataType<const BinaryOperation<T, R, Trait>> {
  using RetType = T;
};

// Extract data type from outer data structs
// -----------------------------------------------------------------------------------------------------------
template <typename T> struct ExtractDataType;
template <typename T, typename R>
struct ExtractDataType<Vec<T, R>> {
  using RetType = T;
};
template <typename T, typename R>
struct ExtractDataType<const Vec<T, R>> {
  using RetType = T const;
};
template <typename T, typename R>
struct ExtractDataType<Mat<T, R>> {
  using RetType = T;
};
template <typename T, typename R>
struct ExtractDataType<const Mat<T, R>> {
  using RetType = T const;
};

template <typename T>
using ExtractedTypeData = typename ExtractDataType<T>::RetType;

// Extract inner data structs from outer data structs
// -----------------------------------------------------------------------------------------------------------
template <typename T> struct ExtractRType;
template <typename T, typename R>
struct ExtractRType<Vec<T, R>> {
  using RType = R;
};
template <typename T, typename R>
struct ExtractRType<const Vec<T, R>> {
  using RType = R const;
};
template <typename T, typename R>
struct ExtractRType<Mat<T, R>> {
  using RType = R;
};
template <typename T, typename R>
struct ExtractRType<const Mat<T, R>> {
  using RType = R const;
};
template <typename T>
using ExtractedRType= typename ExtractRType<T>::RType;

} // namespace etr

#endif
