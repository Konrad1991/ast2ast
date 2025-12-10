#ifndef CONCEPTS_ETR_H
#define CONCEPTS_ETR_H

namespace etr {

// Common type of scalar types
// --------------------------------------------------------------------------------------------------
template<typename T>
using bare_t = std::remove_cv_t<std::remove_reference_t<T>>;
// Primary template
template<typename L, typename R>
struct common_type_internal {
  static_assert(!std::is_same_v<L,L>, "No common_type_internal< L, R > specialization provided.");
};
// 1. Same type
template<typename T>
struct common_type_internal<T, T> { using type = T; };

// 2.1 Logical with anything
template<>
struct common_type_internal<Logical, bool> { using type = Logical; };
template<>
struct common_type_internal<Logical, int> { using type = Integer; };
template<>
struct common_type_internal<Logical, Integer> { using type = Integer; };
template<>
struct common_type_internal<Logical, double> { using type = Double; };
template<>
struct common_type_internal<Logical, Double> { using type = Double; };

// 2.2 Logical with anything
template<>
struct common_type_internal<bool, Logical> { using type = Logical; };
template<>
struct common_type_internal<int, Logical> { using type = Integer; };
template<>
struct common_type_internal<Integer, Logical> { using type = Integer; };
template<>
struct common_type_internal<double, Logical> { using type = Double; };
template<>
struct common_type_internal<Double, Logical> { using type = Double; };

// 3.1 Int & Double --> Double
template<>
struct common_type_internal<Integer, Double> { using type = Double; };
template<>
struct common_type_internal<Integer, double> { using type = Double; };
// 3.2 Int & Double --> Double
template<>
struct common_type_internal<Double, Integer> { using type = Double; };
template<>
struct common_type_internal<double, Integer> { using type = Double; };

// Convenience alias
template<typename L, typename R>
struct common_type {
  using type = typename common_type_internal<bare_t<L>, bare_t<R>>::type;
};

template<typename L, typename R>
using common_type_t = typename common_type<L, R>::type;
// Concept to detect scalars
template<typename T> concept IsArith =
    std::same_as<bare_t<T>, Logical> ||
    std::same_as<bare_t<T>, Integer> ||
    std::same_as<bare_t<T>, Double>;
template <typename T> constexpr bool IsArithV = IsArith<T>;

template<typename T> concept IsDouble = std::same_as<T, Double>;
template<typename T> concept IsInteger = std::same_as<T, Integer>;
template<typename T> concept IsLogical = std::same_as<T, Logical>;

// Determine type for literal bools, ints or doubles
// -----------------------------------------------------------------------------------------------------------
// Float
template <typename T> struct is_float_type : std::is_floating_point<T> {};
template <typename T> struct is_float_type_with_type : is_float_type<typename T::Type> {};
template <typename T, typename = void> struct is_float_dispatch : is_float_type<T> {};
template <typename T> struct is_float_dispatch<T, std::void_t<typename T::Type>> : is_float_type_with_type<T> {};
template <typename T> inline constexpr bool IsCppDouble = is_float_dispatch<T>::value;
// Integer
template <typename T> struct is_integer_type : std::is_integral<T> {};
template <typename T> struct is_integer_type_with_type : is_integer_type<typename T::Type> {};
template <typename T, typename = void> struct is_integer_dispatch : is_integer_type<T> {};
template <typename T> struct is_integer_dispatch<T, std::void_t<typename T::Type>> : is_integer_type_with_type<T> {};
template <typename T> inline constexpr bool IsCppInteger = is_integer_dispatch<T>::value;
// Bool (exactly bool, not all integrals)
template <typename T> struct is_bool_type : std::is_same<T, bool> {};
template <typename T> struct is_bool_type_with_type : is_bool_type<typename T::Type> {};
template <typename T, typename = void> struct is_bool_dispatch : is_bool_type<T> {};
template <typename T> struct is_bool_dispatch<T, std::void_t<typename T::Type>> : is_bool_type_with_type<T> {};
template <typename T> inline constexpr bool IsCppLogical = is_bool_dispatch<T>::value;

// Calculation & Inner data structures
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
               IS<typename ReRef<T>::type::Trait, SquareRootTrait> ||
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
               IS<typename ReRef<T>::type::Trait, UnEqualTrait> ||
               IS<typename ReRef<T>::type::Trait, AndTrait> ||
               IS<typename ReRef<T>::type::Trait, OrTrait>;
};
template <typename T>
concept IsComparisonTrait = requires(T t) { // required as in binary operation the trait is directly tested
  typename T;
  requires IS<T, EqualTrait> ||
               IS<T, SmallerTrait> ||
               IS<T, SmallerEqualTrait> ||
               IS<T, LargerTrait> ||
               IS<T, LargerEqualTrait> ||
               IS<T, UnEqualTrait> ||
               IS<T, AndTrait> ||
               IS<T, OrTrait>;
};

template <typename T>
concept IsSubsetView = requires {
  typename ReRef<T>::type::TypeTrait;
  requires IS<typename ReRef<T>::type::TypeTrait, SubsetViewTrait>;
};
// extract N from SubsetView
template<typename T>
struct subsetview_traits;

template<typename O, std::size_t N, typename Trait>
struct subsetview_traits<SubsetView<O, N, Trait>> {
    static constexpr std::size_t value = N;
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

// Input class (outer data structures)
// -----------------------------------------------------------------------------------------------------------
// Array
template <typename T> struct is_any_array : std::false_type {};
template <typename T, typename R> struct is_any_array<Array<T, R>> : std::true_type {};
template <typename T> inline constexpr bool is_any_array_v = is_any_array<T>::value;
template <typename T> concept IsArray = is_any_array_v<T>;

template <typename T> struct is_array_l : std::false_type {};
template <typename T> struct is_array_l<Array<T, Buffer<T, LBufferTrait>>> : std::true_type {};
template <typename T> inline constexpr bool is_array_l_v = is_array_l<T>::value;
template <typename T> concept IsLBufferArray = is_array_l_v<T>;

template <typename T> struct is_array_r : std::false_type {};
template <typename T> struct is_array_r<Array<T, Buffer<T, RBufferTrait>>> : std::true_type {};
template <typename T> inline constexpr bool is_array_r_v = is_array_r<T>::value;
template <typename T> concept IsRArray = is_array_r_v<T>;

template <typename T> struct is_array_b : std::false_type {};
template <typename T> struct is_array_b<Array<T, Borrow<T, BorrowTrait>>> : std::true_type {};
template <typename T> inline constexpr bool is_array_b_v = is_array_b<T>::value;
template <typename T> concept IsBorrowArray = is_array_b_v<T>;

template <typename T> struct is_array_s : std::false_type {};
template <typename T, typename O, std::size_t N, typename Trait>
struct is_array_s<Array<T, SubsetView<O, N, Trait>>> : std::bool_constant<std::is_same_v<Trait, SubsetViewTrait>> {};
template <typename T> inline constexpr bool is_array_s_v = is_array_s<T>::value;
template <typename T> concept IsSubsetArray = is_array_s_v<T>;

template <typename T> concept IsUnaryArray = IsArray<T> && IsUnary<typename T::DType>;
template <typename T> concept IsBinaryArray = IsArray<T> && IsBinary<typename T::DType>;
template <typename T> concept IsComparisonArray = IsArray<T> && IsComparison<typename T::DType>;

template <typename T>
concept IsOperationArray =
    IsArray<T> && (
        IsUnary<typename T::DType> ||
        IsBinary<typename T::DType> ||
        IsComparison<typename T::DType> ||
        IsSubsetView<typename T::DType>
    );
template <typename T>
concept IsROrCalculationArray = requires(T t) {
  typename T::DType;
  requires IsOperationArray<T> || IsRArray<T>;
};

} // namespace etr

#endif
