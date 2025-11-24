#ifndef CONCEPTS_ETR_H
#define CONCEPTS_ETR_H

namespace etr {

// Determine type for scalars or the inner type in case of other data structures
// -----------------------------------------------------------------------------------------------------------
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
template <typename T, typename X, typename Y, typename Trait>
struct is_array_s<Array<T, SubsetView<X, Y, Trait>>> : std::bool_constant<std::is_same_v<Trait, SubsetViewTrait>> {};
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
