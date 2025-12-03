#ifndef TYPES_ETR_H
#define TYPES_ETR_H

namespace etr {

// Inner data structs
// -----------------------------------------------------------------------------------------------------------
/*
Each inner data struct requires:
- copy constructor
- copy assignment
- move constructor
- move assignment
- size
- const auto& operator[]
- auto& operator[]
- begin
- end
- Trait
- value_type

Classes                     Status
Buffer                      Done
Borrow                      Done
UnaryOperation              Done
BinaryOperation             Done
SubsetView                  Done
*/
template <typename T, typename Trait = LBufferTrait> struct Buffer;
template <typename T, typename Trait = BorrowTrait> struct Borrow;
template <typename L, typename Trait = UnaryTrait> struct UnaryOperation;
template <typename L, typename R, typename Trait = BinaryTrait> struct BinaryOperation;
template<typename O, std::size_t N, typename Trait = SubsetViewTrait> struct SubsetView;

// Outer data structs
// -----------------------------------------------------------------------------------------------------------
template<typename T, typename R>
struct Array { static_assert(sizeof(R) == 0, "No generic Array<T,R> implementation"); };
template <typename T, typename Trait> struct Array<T, Buffer<T, Trait>>;
template <typename T, typename Trait> struct Array<T, Borrow<T, Trait>>;
template <typename T, typename I, typename Trait> struct Array<T, UnaryOperation<I, Trait>>;
template <typename T, typename L, typename R, typename Trait> struct Array<T, BinaryOperation<L, R, Trait>>;
template <typename T, typename O, std::size_t N, typename Trait> struct Array<T, SubsetView<O, N, Trait>>;

// Extract data type from inner data structs
// -----------------------------------------------------------------------------------------------------------
template <typename T> struct ExtractDataType;
template <typename T, typename Trait>
struct ExtractDataType<Buffer<T, Trait>> {
  using value_type = T;
};
template <typename T, typename Trait>
struct ExtractDataType<const Buffer<T, Trait>> {
  using value_type = T;
};

template <typename T, typename Trait>
struct ExtractDataType<Borrow<T, Trait>> {
  using value_type = T;
};
template <typename T, typename Trait>
struct ExtractDataType<const Borrow<T, Trait>> {
  using value_type = T const;
};

// TODO: does this work for SubsetView, UnaryOperation and BinaryOperation
// because here T is not a template parameter
template <typename O, std::size_t N, typename Trait>
struct ExtractDataType<SubsetView<O, N, Trait>> {
  using value_type = T;
};
template <typename O, std::size_t N, typename Trait>
struct ExtractDataType<const SubsetView<O, N, Trait>> {
  using value_type = T;
};

template <typename T, typename Trait>
struct ExtractDataType<UnaryOperation<T, Trait>> {
  using value_type = T;
};
template <typename T, typename Trait>
struct ExtractDataType<const UnaryOperation<T, Trait>> {
  using value_type = T;
};

template <typename T, typename R, typename Trait>
struct ExtractDataType<BinaryOperation<T, R, Trait>> {
  using value_type = T;
};
template <typename T, typename R, typename Trait>
struct ExtractDataType<const BinaryOperation<T, R, Trait>> {
  using value_type = T;
};

// Extract data type from outer data structs
// -----------------------------------------------------------------------------------------------------------
template <typename T, typename R>
struct ExtractDataType<Array<T, R>> {
  using value_type = T;
};
template <typename T, typename R>
struct ExtractDataType<const Array<T, R>> {
  using value_type = T const;
};
template <typename T> using ExtractedTypeData = typename ExtractDataType<T>::value_type;

// Extract inner data structs from outer data structs
// -----------------------------------------------------------------------------------------------------------
template <typename T> struct ExtractRType;
template <typename T, typename R>
struct ExtractRType<Array<T, R>> {
  using RType = R;
};
template <typename T, typename R>
struct ExtractRType<const Array<T, R>> {
  using RType = R const;
};
template <typename T>
using ExtractedRType= typename ExtractRType<T>::RType;

} // namespace etr

#endif
