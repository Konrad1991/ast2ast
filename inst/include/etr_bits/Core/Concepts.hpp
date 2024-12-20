#ifndef CONCEPTS_ETR_H
#define CONCEPTS_ETR_H

#include "Header.hpp"
#include "Traits.hpp"
#include <type_traits>

template <typename T> struct AddConst {
  using type = const T;
};

namespace etr {
template <typename T>
concept isBID = requires {
  requires IS<T, bool> || IS<T, int> ||
               IS<T, double>;
};

// Test R class
// ===================================================================
template <typename T>
concept IsSubset = requires {
    typename ReRef<T>::type::Trait;
    requires IS<typename ReRef<T>::type::Trait, SubsetTrait>;
};
template <typename T>
concept IsSubsetClass = requires {
    typename ReRef<T>::type::Trait;
    requires IS<typename ReRef<T>::type::Trait, SubsetClassTrait>;
};
template <typename T>
concept IsLBuffer = requires {
    typename ReRef<T>::type::Trait;
    requires IS<typename ReRef<T>::type::Trait, LBufferTrait>;
};
template <typename T>
concept IsRBuffer = requires {
    typename ReRef<T>::type::Trait;
    requires IS<typename ReRef<T>::type::Trait, RBufferTrait>;
};
template <typename T>
concept IsBorrow = requires {
    typename ReRef<T>::type::Trait;
    requires IS<typename ReRef<T>::type::Trait, BorrowTrait>;
};
template <typename T>
concept IsBorrowSEXP = requires {
    typename ReRef<T>::type::Trait;
    requires IS<typename ReRef<T>::type::Trait, BorrowSEXPTrait>;
};

template <typename T>
concept IsUnary = requires {
    typename ReRef<decltype(T{})>::type::Trait;
    requires IS<typename ReRef<decltype(T{})>::type::Trait, SinusTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, ASinusTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, SinusHTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, CosinusTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, ACosinusTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, CosinusHTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, TangensTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, ATangensTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, TangensHTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, ExpTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, LogTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, SquareRootTrait> ||
        IS<typename ReRef<decltype(T{})>::type::Trait, MinusUnaryTrait>;
};
template <typename T>
concept IsBinary = requires {
    typename ReRef<T>::type::Trait;
    requires IS<typename ReRef<T>::type::Trait, PlusTrait> ||
        IS<typename ReRef<T>::type::Trait, MinusTrait> ||
        IS<typename ReRef<T>::type::Trait, TimesTrait> ||
        IS<typename ReRef<T>::type::Trait, DivideTrait> ||
        IS<typename ReRef<T>::type::Trait, PowTrait> ||
        IS<typename ReRef<T>::type::Trait, EqualTrait> ||
        IS<typename ReRef<T>::type::Trait, SmallerTrait> ||
        IS<typename ReRef<T>::type::Trait, SmallerEqualTrait> ||
        IS<typename ReRef<T>::type::Trait, LargerTrait> ||
        IS<typename ReRef<T>::type::Trait, LargerEqualTrait> ||
        IS<typename ReRef<T>::type::Trait, UnEqualTrait>;
};

// Test Vec class
// ===================================================================
template <typename T>
concept IsLBufferVec = requires(T t) {
  typename T::DType;
  requires IsLBuffer<typename T::DType>;
};
template <typename T>
concept IsRBufferVec = requires(T t) {
  typename T::DType;
  requires IsRBuffer<typename T::DType>;
};
template <typename T>
concept IsSubsetVec = requires(T t) {
  typename T::DType;
  requires IsSubset<typename T::DType>;
};

// Test Input class
// ===================================================================
// Vector
template <typename T>
struct is_vec: std::false_type {};

template <typename T>
struct is_vec<Vec<T>> : std::true_type {};

template <typename T>
inline constexpr bool is_vec_v = is_vec <T>::value;
template <typename T>
concept IsVec = is_vec_v<T>;

template <typename T>
concept IsRVec = requires(T t) {
  typename T::DType;
  requires IsVec<T> && IsRBuffer<typename T::DType>;
};


// Binary
template <typename T>
struct is_binary_operation : std::false_type {};

template <typename L2, typename R2, typename OperationTrait>
struct is_binary_operation<BinaryOperation<L2, R2, OperationTrait>>
    : std::bool_constant<IsBinary<OperationTrait>> {};

template <typename T>
inline constexpr bool is_binary_operation_v = is_binary_operation<T>::value;

template <typename T>
concept IsBinaryOperation = is_binary_operation_v<T>;



} // namespace etr

#endif
