#ifndef TYPES_ETR_H
#define TYPES_ETR_H

namespace etr {

// Scalar types
// --------------------------------------------------------------------------------------------------
struct Logical {
  bool val;
  Logical() : val(false) {}
  Logical(bool v) : val(v) {}
  friend std::ostream& operator<<(std::ostream& os, const Logical& x) {
    return os << x.val;
  }
};
struct Integer {
  int val;
  Integer() : val(0) {}
  Integer(int v) : val(v) {}
  friend std::ostream& operator<<(std::ostream& os, const Integer& x) {
    return os << x.val;
  }
};
struct Double {
  double val;
  Double() : val(0.0) {}
  Double(double v) : val(v) {}
  friend std::ostream& operator<<(std::ostream& os, const Double& x) {
    return os << x.val;
  }
};

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
- begin and end
- Trait
- value_type --> FIX: has to be removed in all inner data structs. the outer Array should possess it

Classes Buffer, Borrow, UnaryOperation, BinaryOperation, SubsetView
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

// Extract data type from outer data structs
// -----------------------------------------------------------------------------------------------------------
template <typename T> struct ExtractDataType;

template <typename T, typename R>
struct ExtractDataType<Array<T, R>> {
  using value_type = T;
};
template <typename T, typename R>
struct ExtractDataType<const Array<T, R>> {
  using value_type = T const;
};
template <typename T> using ExtractedTypeData = typename ExtractDataType<T>::value_type;

} // namespace etr

#endif
