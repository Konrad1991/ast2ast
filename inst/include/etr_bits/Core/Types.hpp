#ifndef TYPES_ETR_H
#define TYPES_ETR_H

#include <cmath>
#include <limits>

#ifdef STANDALONE_ETR
#include "stddef.h"
#include <cxxabi.h>
// Standalone mode still uses BLAS for matmul; link to openblas required
extern "C" void dgemm_(const char* transa, const char* transb,
                       const int* m, const int* n, const int* k,
                       const double* alpha, const double* a, const int* lda,
                       const double* b, const int* ldb,
                       const double* beta, double* c, const int* ldc);
#define F77_CALL(x) x##_
#define FCONE
#else
#include <R_ext/BLAS.h>
#include <R_ext/RS.h>
#ifndef FCONE
#define FCONE
#endif
#endif

#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <array>
#include <ios>
#include <iostream>
#include <iterator>
#include <math.h>
#include <memory>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>
#include <functional>
#include <numeric>

namespace etr {

/*
--------------------------------------------------------------------------------------------------------------
Messages/Warnings and Errors
--------------------------------------------------------------------------------------------------------------
*/
inline void ass(bool inp, const std::string &message) {
#ifdef STANDALONE_ETR
  if (!inp)
    throw std::runtime_error(message);
#else
  if (!inp)
    Rcpp::stop(message);
#endif
}

// https://ctrpeach.io/posts/cpp20-string-literal-template-parameters/
template <std::size_t N> struct string_literal {
  constexpr string_literal(const char (&str)[N]) {
    std::copy_n(str, N, value.begin());
  }
  std::array<char, N> value;
};

template <string_literal msg> inline void ass(bool inp) {
#ifdef STANDALONE_ETR
  if (!inp) throw std::runtime_error(msg.value.data());
#else
  if (!inp) Rcpp::stop(msg.value.data());
#endif
}

inline void warn(bool inp, std::string message) {
#ifdef STANDALONE_ETR
  if (!inp) std::cerr << "Warning: " + message << std::endl;
#else
  if (!inp) Rcpp::warning("Warning: " + message);
#endif
}

template <string_literal msg> inline void warn(bool inp) {
#ifdef STANDALONE_ETR
  if (!inp) std::cerr << msg.value.data() << std::endl;
#else
  if (!inp) Rcpp::warning(msg.value.data());
#endif
}

/*
-----------------------------------------------------------------------------------------------------------
Holder stores references to L values.
Instead R values are kept alive by storing them on the heap using ConstHolder
-----------------------------------------------------------------------------------------------------------
*/
template <class T> class ConstHolder {
  std::shared_ptr<T> owned; // used only when constructed from rvalue
  const T* ptr = nullptr;

public:
  ConstHolder(const T& ref) : ptr(&ref) {}

  ConstHolder(T&& r) : owned(std::make_shared<T>(std::move(r))), ptr(owned.get()) {}

  ConstHolder(const ConstHolder&) = default;
  ConstHolder(ConstHolder&&) noexcept = default;
  ConstHolder& operator=(const ConstHolder&) = default;
  ConstHolder& operator=(ConstHolder&&) noexcept = default;

  const T& get() const { return *ptr; }
};


template <typename T> class Holder {
  std::optional<T> val;
  T* ptr = nullptr;
public:
  // Construct from lvalue
  Holder(T& ref) : ptr(&ref) {}
  // Construct from rvalue
  Holder(T&& r) {
    val.emplace(std::move(r));
    ptr = &val.value();
  }
  // Copy constructor
  Holder(const Holder& other) {
    if (other.val.has_value()) {
      val.emplace(*other.val);
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }
  // Copy assignment
  Holder& operator=(const Holder& other) {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }
  // Move constructor
  Holder(Holder&& other) noexcept {
    if (other.val.has_value()) {
      val.emplace(std::move(*other.val));
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }
  // Move assignment
  Holder& operator=(Holder&& other) noexcept {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }
  T& get() const {
    return *ptr;
  }
};

/*
-----------------------------------------------------------------------------------------------------------
Helper:
-----------------------------------------------------------------------------------------------------------
*/
// 1. some own names for members of the type trait library
template <typename T, typename U> inline constexpr bool IS = std::is_same_v<T, U>;

template <typename T> using IsCppArith = std::is_arithmetic<T>;
template <typename T> constexpr bool IsCppArithV = std::is_arithmetic_v<T>;

template <typename T> using Decayed = std::decay_t<T>;

template <typename T> using ReRef = std::remove_reference<T>;

template <typename T> constexpr bool IsRvalueV = std::is_rvalue_reference_v<T>;

// 2. The struct SI which is used to initialize Arrays of a respective size
struct SI {
  std::size_t sz{0};

  SI() = default;
  explicit SI(std::size_t n) : sz(n) {
    ass<"Size has to be larger than 0!">(sz >= 1);
  }
  // signed integers (e.g., int, long long)
  template <std::signed_integral I>
  explicit SI(I n) {
    ass<"Size has to be larger than 0!">(n >= 1);
    using U = std::make_unsigned_t<I>;
    const U un = static_cast<U>(n);
    ass<"Requested size too large">(un <= static_cast<U>(std::numeric_limits<std::size_t>::max()));
    sz = static_cast<std::size_t>(un);
  }
  // floating point (floor, then validate)
  template <std::floating_point F>
  explicit SI(F n) {
    ass<"Size is not finite">(std::isfinite(n));
    const F f = std::floor(n);
    ass<"Size has to be larger than 0!">(f >= static_cast<F>(1));
    const long double ld = static_cast<long double>(f);
    ass<"Requested size too large">(ld <= static_cast<long double>(std::numeric_limits<std::size_t>::max()));
    sz = static_cast<std::size_t>(ld);
  }
};

// 3. safe modulo which works for std::size_t --> used in preserved subsetting
inline std::size_t safe_modulo(std::size_t idx, std::size_t sz) {
  return sz ? (idx % sz) : 0;  // no UB if n==0
}

// 4. helper to loop over variadic templates 
// Used in preserved and simplified subsetting and in allocation::c
template <class F, class... Args> inline F forEachArg(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...); return f;
}

// 5. extract dimension from an array instance
// Used in subsetting and printing
template<typename Dim> auto&& dim_view(Dim& d) {
  if constexpr (requires { d.get(); }) {
    return d.get();
  } else {
    return d;
  }
}

// 6. conversion of double to an index
inline std::size_t safe_index_from_double(double x) {
  ass<"invalid index argument">(std::isfinite(x));
  double t = std::trunc(x);
  ass<"Negative indices are not supported">(t >= 1);
  return static_cast<std::size_t>(t);
}

// 7. struct Dim. Currently this is not used.
// Potentially this replaces later std::vector<std::size_t> dim
// The idea is that Dim stores the first two dimensions on the stack
// Starting from the third dimension the dimension are put on the heap
struct Dim {
  std::size_t rank = 1;
  std::size_t nrow = 1;
  std::size_t ncol = 0;
  std::unique_ptr<std::size_t[]> other_dims;

  Dim() = default;
  Dim(const Dim& other) { *this = other; }
  Dim(Dim&& other) noexcept { *this = std::move(other); }
  Dim& operator=(const Dim& other) {
    if (this == &other) return *this;
    rank = other.rank;
    nrow = other.nrow;
    ncol = (other.rank >= 2) ? other.ncol : 0;
    if (rank > 2) {
      other_dims = std::make_unique<std::size_t[]>(rank - 2);
      std::copy_n(other.other_dims.get(), rank - 2, other_dims.get());
    } else {
      other_dims.reset();
    }
    return *this;
  }

  Dim& operator=(Dim&& other) noexcept {
    if (this == &other) return *this;
    rank = other.rank;
    nrow = other.nrow;
    ncol = other.ncol;
    other_dims = std::move(other.other_dims);
    other.rank = 1;
    other.nrow = 1;
    other.ncol = 0;
    return *this;
  }
  std::size_t size() const noexcept { return rank; }

  std::size_t dim(std::size_t k) const noexcept {
    ass<"Invalid dim (out of bounds)">(k < rank);
    if (k == 0) return nrow;
    if (k == 1) return ncol;
    return other_dims[k - 2];
  }

  void set_rank1(std::size_t n) {
    rank = 1;
    nrow = n;
    ncol = 0;
    other_dims.reset();
  }
  void set_rank2(std::size_t r, std::size_t c) {
    rank = 2;
    nrow = r;
    ncol = c;
    other_dims.reset();
  }
  void set_rankN(std::size_t rk, std::size_t r, std::size_t c, const std::size_t* tail) {
    rank = rk;
    nrow = r;
    ncol = c;
    if (rank > 2) {
      other_dims = std::make_unique<std::size_t[]>(rank - 2);
      std::copy_n(tail, rank - 2, other_dims.get());
    } else {
      other_dims.reset();
    }
  }
};

/*
--------------------------------------------------------------------------------------------------
Traits (third dispatch layer)
These are the traits used in the operator overloads in 
Calculations/UnaryCalculations.hpp and Calculations/BinaryCalculations.hpp
--------------------------------------------------------------------------------------------------
*/
struct ComparisonTrait { using value_type = bool; };
struct LBufferTrait {};
struct RBufferTrait {};
struct SubsetViewTrait {};
struct ConstSubsetViewTrait {};
struct BorrowTrait {};
struct BinaryTrait {};
struct UnaryTrait {};

struct PlusTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l + r;
  }
};
struct MinusTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l - r;
  }
};
struct TimesTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l * r;
  }
};
struct DivideTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l / r;
  }
};
struct PowTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return pow(l, r);
  }
};
struct ModTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l % r;
  }
};
struct EqualTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l == r;
  }
};
struct SmallerTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l < r;
  }
};
struct SmallerEqualTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l <= r;
  }
};
struct LargerTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l > r;
  }
};
struct LargerEqualTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l >= r;
  }
};
struct UnEqualTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l != r;
  }
};
struct AndTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l && r;
  }
};
struct OrTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l || r;
  }
};

struct SinusTrait {
  template <typename L> static inline auto f(L a) { return sin(a); }
};
struct ASinusTrait {
  template <typename L> static inline auto f(L a) { return asin(a); }
};
struct SinusHTrait {
  template <typename L> static inline auto f(L a) { return sinh(a); }
};
struct CosinusTrait {
  template <typename L> static inline auto f(L a) { return cos(a); }
};
struct ACosinusTrait {
  template <typename L> static inline auto f(L a) { return acos(a); }
};
struct CosinusHTrait {
  template <typename L> static inline auto f(L a) { return cosh(a); }
};
struct TangensTrait {
  template <typename L> static inline auto f(L a) { return tan(a); }
};
struct ATangensTrait {
  template <typename L> static inline auto f(L a) { return atan(a); }
};
struct TangensHTrait {
  template <typename L> static inline auto f(L a) { return tanh(a); }
};
struct ExpTrait {
  template <typename L> static inline auto f(L a) { return exp(a); }
};
struct LogTrait {
  template <typename L> static inline auto f(L a) { return log(a); }
};
struct SquareRootTrait {
  template <typename L> static inline auto f(L a) { return sqrt(a); }
};
struct MinusUnaryTrait {
  template <typename L> static inline auto f(L a) { return -a; }
};

/*
-----------------------------------------------------------------------------------------------------------
Inner data structs Buffer, Borrow, UnaryOperation, etc.
-----------------------------------------------------------------------------------------------------------
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
- value_type
Classes Buffer, Borrow, UnaryOperation, BinaryOperation, SubsetView
*/
template <typename T, typename Trait = LBufferTrait> struct Buffer;
template <typename T, typename Trait = BorrowTrait> struct Borrow;
template <typename L, typename Trait = UnaryTrait> struct UnaryOperation;
template <typename L, typename R, typename Trait = BinaryTrait> struct BinaryOperation;
template<typename O, std::size_t N, typename Trait = SubsetViewTrait> struct SubsetView;
template<typename O, std::size_t N, typename Trait = ConstSubsetViewTrait> struct ConstSubsetView;

/*
-----------------------------------------------------------------------------------------------------------
Outer data structs
Array which wraps one of the inner data structs
-----------------------------------------------------------------------------------------------------------
*/
template<typename T, typename R>
struct Array { static_assert(sizeof(R) == 0, "No generic Array<T,R> implementation"); };
template <typename T, typename Trait> struct Array<T, Buffer<T, Trait>>;
template <typename T, typename Trait> struct Array<T, Borrow<T, Trait>>;
template <typename T, typename I, typename Trait> struct Array<T, UnaryOperation<I, Trait>>;
template <typename T, typename L, typename R, typename Trait> struct Array<T, BinaryOperation<L, R, Trait>>;
template <typename T, typename O, std::size_t N, typename Trait> struct Array<T, SubsetView<O, N, Trait>>;
template <typename T, typename O, std::size_t N, typename Trait> struct Array<T, ConstSubsetView<O, N, Trait>>;

/*
-----------------------------------------------------------------------------------------------------------
Extract data type from outer data structs
-----------------------------------------------------------------------------------------------------------
*/
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

/*
-----------------------------------------------------------------------------------------------------------
Determine type for literal bools, ints or doubles
-----------------------------------------------------------------------------------------------------------
*/
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

/*
-----------------------------------------------------------------------------------------------------------
Concepts to check whether object is:
* unary operation
* binary operation
* comparison
* subset
* const subset
* L buffer
* R buffer
* Borrow
-----------------------------------------------------------------------------------------------------------
*/
template <typename T> concept IsUnary = requires {
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
template <typename T> concept IsBinary = requires {
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
template <typename T> concept IsComparison = requires {
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
// Mutable subset view
// -------------------------------------------------------------------
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
// Const subset view
// -------------------------------------------------------------------
template <typename T>
concept IsConstSubsetView = requires {
typename ReRef<T>::type::TypeTrait;
requires IS<typename ReRef<T>::type::TypeTrait, ConstSubsetViewTrait>;
};
// extract N from ConstSubsetView
template<typename T>
struct const_subsetview_traits;

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

/*
-----------------------------------------------------------------------------------------------------------
Concepts to check what type of array is object:
* is array
* is L Buffer array
* is R Buffer array
* is Borrow array
* is subset array
* is const subset array
* is unary operation array
* is binary operation array
* is comparison operation array
* is operation array
    * unary | binary | comparison | subset | const subset
* is r or calculation
    * is operation array or r buffer array
-----------------------------------------------------------------------------------------------------------
*/
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

template <typename T> struct is_array_const_s : std::false_type {};
template <typename T, typename O, std::size_t N, typename Trait>
struct is_array_const_s<Array<T, ConstSubsetView<O, N, Trait>>> : std::bool_constant<std::is_same_v<Trait, ConstSubsetViewTrait>> {};
template <typename T> inline constexpr bool is_array_const_s_v = is_array_const_s<T>::value;
template <typename T> concept IsConstSubsetArray = is_array_const_s_v<T>;

template <typename T> concept IsUnaryArray = IsArray<T> && IsUnary<typename T::DType>;
template <typename T> concept IsBinaryArray = IsArray<T> && IsBinary<typename T::DType>;
template <typename T> concept IsComparisonArray = IsArray<T> && IsComparison<typename T::DType>;

template <typename T>
concept IsOperationArray =
IsArray<T> && (
IsUnary<typename T::DType> ||
IsBinary<typename T::DType> ||
IsComparison<typename T::DType> ||
IsSubsetView<typename T::DType> ||
IsConstSubsetView<typename T::DType>
    );
template <typename T>
concept IsROrCalculationArray = requires(T t) {
typename T::DType;
requires IsOperationArray<T> || IsRArray<T>;
};

} // namespace etr

#endif
