#ifndef CONCEPTS_ETR_H
#define CONCEPTS_ETR_H

#include "Header.hpp"
#include "Traits.hpp"
#include <type_traits>

namespace etr {

// Determine type for scalars or the inner type in case of other data structures
// -----------------------------------------------------------------------------------------------------------
template <typename T>
concept isBID =
    requires { requires IS<T, bool> || IS<T, int> || IS<T, double>; };
// Float
template <typename T> struct is_float_type : std::is_floating_point<T> {};
template <typename T> struct is_float_type_with_type : is_float_type<typename T::Type> {};
template <typename T, typename = void> struct is_float_dispatch : is_float_type<T> {};
template <typename T> struct is_float_dispatch<T, std::void_t<typename T::Type>> : is_float_type_with_type<T> {};
template <typename T> inline constexpr bool IsFloat = is_float_dispatch<T>::value;
// Integer
template <typename T> struct is_integer_type : std::is_integral<T> {};
template <typename T> struct is_integer_type_with_type : is_integer_type<typename T::Type> {};
template <typename T, typename = void> struct is_integer_dispatch : is_integer_type<T> {};
template <typename T> struct is_integer_dispatch<T, std::void_t<typename T::Type>> : is_integer_type_with_type<T> {};
template <typename T> inline constexpr bool IsInteger = is_integer_dispatch<T>::value;
// Bool (exactly bool, not all integrals)
template <typename T> struct is_bool_type : std::is_same<T, bool> {};
template <typename T> struct is_bool_type_with_type : is_bool_type<typename T::Type> {};
template <typename T, typename = void> struct is_bool_dispatch : is_bool_type<T> {};
template <typename T> struct is_bool_dispatch<T, std::void_t<typename T::Type>> : is_bool_type_with_type<T> {};
template <typename T> inline constexpr bool IsBool = is_bool_dispatch<T>::value;

// Calculation
// -----------------------------------------------------------------------------------------------------------
template <typename T>
concept IsUnary = requires {
  typename ReRef<T>::type::Trait;
  requires IS<typename ReRef<T>::type::Trait, SinusTrait> ||
               IS<typename ReRef<T>::type::Trait, ASinusTrait> ||
               IS<typename ReRef<T>::type::Trait, SinusHTrait> ||
               IS<typename ReRef<T>::type::Trait, CosinusTrait> ||
               IS<typename ReRef<T>::type::Trait, ACosinusTrait> ||
               IS<typename ReRef<T>::type::Trait, CosinusHTrait> ||
               IS<typename ReRef<T>::type::Trait, TangensTrait> ||
               IS<typename ReRef<T>::type::Trait, ATangensTrait> ||
               IS<typename ReRef<T>::type::Trait, TangensHTrait> ||
               IS<typename ReRef<T>::type::Trait, ExpTrait> ||
               IS<typename ReRef<T>::type::Trait, LogTrait> ||
               IS<typename ReRef<T>::type::Trait,
                  SquareRootTrait> ||
               IS<typename ReRef<T>::type::Trait, MinusUnaryTrait>;
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
template <typename T>
concept IsComparison = requires {
  typename ReRef<T>::type::Trait;
  requires IS<typename ReRef<T>::type::Trait, EqualTrait> ||
               IS<typename ReRef<T>::type::Trait, SmallerTrait> ||
               IS<typename ReRef<T>::type::Trait, SmallerEqualTrait> ||
               IS<typename ReRef<T>::type::Trait, LargerTrait> ||
               IS<typename ReRef<T>::type::Trait, LargerEqualTrait> ||
               IS<typename ReRef<T>::type::Trait, UnEqualTrait>;
};

// Input class (outer data structures)
// -----------------------------------------------------------------------------------------------------------
// Vector
template <typename T> struct is_any_vec : std::false_type {};
template <typename T, typename R> struct is_any_vec<Vec<T, R>> : std::true_type {};
template <typename T> inline constexpr bool is_any_vec_v = is_any_vec<T>::value;
template <typename T> concept IsVec = is_any_vec_v<T>;

template <typename T> struct is_vec : std::false_type {};
template <typename T> struct is_vec<Vec<T>> : std::true_type {};
template <typename T> inline constexpr bool is_vec_v = is_vec<T>::value;
template <typename T> concept IsBufferVec = is_vec_v<T>;

template <typename T> struct is_vec_r : std::false_type {};
template <typename T> struct is_vec_r<Vec<T, Buffer<T, RBufferTrait>>> : std::true_type {};
template <typename T> inline constexpr bool is_vec_r_v = is_vec_r<T>::value;
template <typename T> concept IsRVec = is_vec_r_v<T>;

template <typename T> struct is_vec_b : std::false_type {};
template <typename T> struct is_vec_b<Vec<T, Borrow<T, BorrowTrait>>> : std::true_type {};
template <typename T> inline constexpr bool is_vec_b_v = is_vec_b<T>::value;
template <typename T> concept IsBorrowVec = is_vec_b_v<T>;

template <typename T> struct is_vec_s : std::false_type {};
template <typename T, typename X, typename Y, typename Trait>
struct is_vec_s<Vec<T, SubsetClass<X, Y, Trait>>> : std::bool_constant<std::is_same_v<Trait, SubsetClassTrait>> {};
template <typename T> inline constexpr bool is_vec_s_v = is_vec_s<T>::value;
template <typename T> concept IsSubsetVec = is_vec_s_v<T>;

template <typename T> concept IsUnaryVec = IsVec<T> && IsUnary<typename T::DType>;
template <typename T> concept IsBinaryVec = IsVec<T> && IsBinary<typename T::DType>;
template <typename T> concept IsComparisonVec = IsVec<T> && IsComparison<typename T::DType>;

template <typename T>
concept IsOperationVec = requires(T t) {
  typename T::DType;
  requires IsUnary<typename T::DType> || IsBinary<typename T::DType> || IsComparison<typename T::DType>;
};

template <typename T>
concept IsROrCalculationVec = requires(T t) {
  typename T::DType;
  requires IsOperationVec<T> || IsRVec<T>;
};

// Matrix
template <typename T> struct is_any_mat : std::false_type {};
template <typename T, typename R> struct is_any_mat<Mat<T, R>> : std::true_type {};
template <typename T> inline constexpr bool is_any_mat_v = is_any_mat<T>::value;
template <typename T> concept IsMat = is_any_mat_v<T>;

template <typename T> struct is_mat : std::false_type {};
template <typename T> struct is_mat<Mat<T>> : std::true_type {};
template <typename T> inline constexpr bool is_mat_v = is_mat<T>::value;
template <typename T> concept IsBufferMat = is_mat_v<T>;

template <typename T> struct is_mat_r : std::false_type {};
template <typename T> struct is_mat_r<Mat<T, Buffer<T, RBufferTrait>>> : std::true_type {};
template <typename T> inline constexpr bool is_mat_r_v = is_mat_r<T>::value;
template <typename T> concept IsRMat = is_mat_r_v<T>;

template <typename T> struct is_mat_b : std::false_type {};
template <typename T> struct is_mat_b<Mat<T, Borrow<T, BorrowTrait>>> : std::true_type {};
template <typename T> inline constexpr bool is_mat_b_v = is_mat_b<T>::value;
template <typename T> concept IsBorrowMat = is_mat_b_v<T>;

template <typename T> struct is_mat_s : std::false_type {};
template <typename X, typename Y, typename Trait>
struct is_mat_s<Mat<X, SubsetClass<X, Y, Trait>>> : std::bool_constant<std::is_same_v<Trait, SubsetClassTrait>> {};
template <typename T> inline constexpr bool is_mat_s_v = is_vec_s<T>::value;
template <typename T> concept IsSubsetMat = is_mat_s_v<T>;

// Array like (Vec or Mat)
template <typename T>
concept IsArrayLike = requires(T t) {
  typename T;
  requires IsVec<T> || IsMat<T>;
};
template <typename T>
concept IsRArrayLike = requires(T t) {
  typename T;
  requires IsRVec<T> || IsRMat<T>;
};

// Inner data structures
// -----------------------------------------------------------------------------------------------------------
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

} // namespace etr

#endif
