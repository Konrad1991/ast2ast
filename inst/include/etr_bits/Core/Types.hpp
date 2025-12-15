#ifndef TYPES_ETR_H
#define TYPES_ETR_H

namespace etr {

// Scalar types (First dispatch layer)
// --------------------------------------------------------------------------------------------------
struct Logical;
struct Integer;
struct Double;
struct Dual;

struct Logical {
  bool val;
  Logical();
  Logical(bool v);
  Logical(Integer v);
  Logical(Double v);
  Logical(Dual v);
  Integer operator+(const Logical&) const;
  Integer operator-(const Logical&) const;
  Integer operator*(const Logical&) const;
  Double operator/(const Logical&) const;
  Double pow(const Logical&) const;
  Logical operator==(const Logical&) const;
  Logical operator<(const Logical&) const;
  Logical operator<=(const Logical&) const;
  Logical operator>(const Logical&) const;
  Logical operator>=(const Logical&) const;
  Logical operator!=(const Logical&) const;
  Logical operator&&(const Logical&) const;
  Logical operator||(const Logical&) const;
  Double sin() const;
  Double asin() const;
  Double sinh() const;
  Double cos() const;
  Double acos() const;
  Double cosh() const;
  Double tan() const;
  Double atan() const;
  Double tanh() const;
  Double exp() const;
  Double log() const;
  Double sqrt() const;
  Integer operator-() const;
  friend std::ostream& operator<<(std::ostream&, const Logical&);
};

struct Integer {
  int val;
  Integer();
  Integer(int v);
  Integer(Logical v);
  Integer(Double v);
  Integer(Dual v);
  Integer operator+(const Integer&) const;
  Integer operator-(const Integer&) const;
  Integer operator*(const Integer&) const;
  Double operator/(const Integer&) const;
  Double pow(const Integer&) const;
  Logical operator==(const Integer&) const;
  Logical operator<(const Integer&) const;
  Logical operator<=(const Integer&) const;
  Logical operator>(const Integer&) const;
  Logical operator>=(const Integer&) const;
  Logical operator!=(const Integer&) const;
  Logical operator&&(const Integer&) const;
  Logical operator||(const Integer&) const;
  Double sin() const;
  Double asin() const;
  Double sinh() const;
  Double cos() const;
  Double acos() const;
  Double cosh() const;
  Double tan() const;
  Double atan() const;
  Double tanh() const;
  Double exp() const;
  Double log() const;
  Double sqrt() const;
  Integer operator-() const;
  friend std::ostream& operator<<(std::ostream&, const Integer&);
};

struct Double {
  double val;
  Double();
  Double(double v);
  Double(Logical v);
  Double(Integer v);
  Double(Dual v);
  Double operator+(const Double&) const;
  Double operator-(const Double&) const;
  Double operator*(const Double&) const;
  Double operator/(const Double&) const;
  Double pow(const Double&) const;
  Logical operator==(const Double&) const;
  Logical operator<(const Double&) const;
  Logical operator<=(const Double&) const;
  Logical operator>(const Double&) const;
  Logical operator>=(const Double&) const;
  Logical operator!=(const Double&) const;
  Logical operator&&(const Double&) const;
  Logical operator||(const Double&) const;
  Double sin() const;
  Double asin() const;
  Double sinh() const;
  Double cos() const;
  Double acos() const;
  Double cosh() const;
  Double tan() const;
  Double atan() const;
  Double tanh() const;
  Double exp() const;
  Double log() const;
  Double sqrt() const;
  Double operator-() const;
  friend std::ostream& operator<<(std::ostream&, const Double&);
};

struct Dual {
  double val, dot;
  Dual();
  Dual(double v, double d);
  Dual(Logical v);
  Dual(Integer v);
  Dual(Double v);
  template<typename T> requires std::is_arithmetic_v<T>
  Dual(T v);
  Dual operator+(const Dual&) const;
  Dual operator-(const Dual&) const;
  Dual operator*(const Dual&) const;
  Dual operator/(const Dual&) const;
  Dual pow(const Dual&) const;
  Logical operator==(const Dual&) const;
  Logical operator<(const Dual&) const;
  Logical operator<=(const Dual&) const;
  Logical operator>(const Dual&) const;
  Logical operator>=(const Dual&) const;
  Logical operator!=(const Dual&) const;
  Logical operator&&(const Dual&) const;
  Logical operator||(const Dual&) const;
  Dual sin() const;
  Dual asin() const;
  Dual sinh() const;
  Dual cos() const;
  Dual acos() const;
  Dual cosh() const;
  Dual tan() const;
  Dual atan() const;
  Dual tanh() const;
  Dual exp() const;
  Dual log() const;
  Dual sqrt() const;
  Dual operator-() const;
  friend std::ostream& operator<<(std::ostream&, const Dual&);
};

Logical::Logical() : val(false) {}
Logical::Logical(bool v) : val(v) {}
Logical::Logical(Integer v) : val(static_cast<bool>(v.val)) {}
Logical::Logical(Double v)  : val(static_cast<bool>(v.val)) {}
Logical::Logical(Dual v)    : val(static_cast<bool>(v.val)) {}

Integer::Integer() : val(0) {}
Integer::Integer(int v) : val(v) {}
Integer::Integer(Logical v) : val(static_cast<int>(v.val)) {}
Integer::Integer(Double v)  : val(static_cast<int>(v.val)) {}
Integer::Integer(Dual v)    : val(static_cast<int>(v.val)) {}

Double::Double() : val(0.0) {}
Double::Double(double v) : val(v) {}
Double::Double(Logical v) : val(static_cast<double>(v.val)) {}
Double::Double(Integer v) : val(static_cast<double>(v.val)) {}
Double::Double(Dual v)    : val(v.val) {}

Dual::Dual() : val(0.0), dot(0.0) {}
Dual::Dual(double v, double d) : val(v), dot(d) {}
Dual::Dual(Logical v) : val(static_cast<double>(v.val)), dot(0.0) {}
Dual::Dual(Integer v) : val(static_cast<double>(v.val)), dot(0.0) {}
Dual::Dual(Double v)    : val(v.val), dot(0.0) {}
template<typename T> requires std::is_arithmetic_v<T>
Dual::Dual(T v)    : val(static_cast<double>(v)), dot(0.0) {}

std::ostream& operator<<(std::ostream& os, const Logical& x) { return os << x.val; }
std::ostream& operator<<(std::ostream& os, const Integer& x) { return os << x.val; }
std::ostream& operator<<(std::ostream& os, const Double& x) { return os << x.val; }
std::ostream& operator<<(std::ostream& os, const Dual& x) { return os << x.val << " " << x.dot; }

Integer Logical::operator+(const Logical& other) const { return Integer(val + other.val); }
Integer Integer::operator+(const Integer& other) const { return Integer(val + other.val); }
Double Double::operator+(const Double& other) const { return Double(val + other.val); }
Dual Dual::operator+(const Dual& other) const { return Dual(val + other.val, dot + other.dot); }

Integer Logical::operator-(const Logical& other) const { return Integer(val - other.val); }
Integer Integer::operator-(const Integer& other) const { return Integer(val - other.val); }
Double Double::operator-(const Double& other) const { return Double(val - other.val); }
Dual Dual::operator-(const Dual& other) const { return Dual(val - other.val, dot - other.dot); }

Integer Logical::operator*(const Logical& other) const { return Integer(val * other.val); }
Integer Integer::operator*(const Integer& other) const { return Integer(val * other.val); }
Double Double::operator*(const Double& other) const { return Double(val * other.val); }
Dual Dual::operator*(const Dual& other) const { return Dual(val * other.val, val*other.dot + dot*other.val); }

Double Logical::operator/(const Logical& other) const { return Double(static_cast<double>(val) / static_cast<double>(other.val)); }
Double Integer::operator/(const Integer& other) const { return Double(static_cast<double>(val) / static_cast<double>(other.val)); }
Double Double::operator/(const Double& other) const { return Double(val / other.val); }
Dual Dual::operator/(const Dual& other) const {
  const double inv = 1.0 / other.val;
  return Dual(val * inv, (dot * other.val - val * other.dot) * (inv*inv));
}

Double Logical::pow(const Logical& other) const { return Double(std::pow(static_cast<double>(val), static_cast<double>(other.val))); }
Double Integer::pow(const Integer& other) const { return Double(std::pow(static_cast<double>(val), static_cast<double>(other.val))); }
Double Double::pow(const Double& other) const { return Double(std::pow(val, other.val)); }
Dual Dual::pow(const Dual& other) const {
  double f = std::pow(val, other.val);
  double d = f * (other.dot * std::log(val) + other.val * dot / val);
  return Dual(f, d);
}

Logical Logical::operator==(const Logical& other) const { return Logical(val == other.val); }
Logical Integer::operator==(const Integer& other) const { return Logical(val == other.val); }
Logical Double::operator==(const Double& other) const { return Logical(val == other.val); }
Logical Dual::operator==(const Dual& other) const { return Logical(val == other.val); }

Logical Logical::operator<(const Logical& other) const { return Logical(val < other.val); }
Logical Integer::operator<(const Integer& other) const { return Logical(val < other.val); }
Logical Double::operator<(const Double& other) const { return Logical(val < other.val); }
Logical Dual::operator<(const Dual& other) const { return Logical(val < other.val); }

Logical Logical::operator<=(const Logical& other) const { return Logical(val <= other.val); }
Logical Integer::operator<=(const Integer& other) const { return Logical(val <= other.val); }
Logical Double::operator<=(const Double& other) const { return Logical(val <= other.val); }
Logical Dual::operator<=(const Dual& other) const { return Logical(val <= other.val); }

Logical Logical::operator>(const Logical& other) const { return Logical(val > other.val); }
Logical Integer::operator>(const Integer& other) const { return Logical(val > other.val); }
Logical Double::operator>(const Double& other) const { return Logical(val > other.val); }
Logical Dual::operator>(const Dual& other) const { return Logical(val > other.val); }

Logical Logical::operator>=(const Logical& other) const { return Logical(val >= other.val); }
Logical Integer::operator>=(const Integer& other) const { return Logical(val >= other.val); }
Logical Double::operator>=(const Double& other) const { return Logical(val >= other.val); }
Logical Dual::operator>=(const Dual& other) const { return Logical(val >= other.val); }

Logical Logical::operator!=(const Logical& other) const { return Logical(val != other.val); }
Logical Integer::operator!=(const Integer& other) const { return Logical(val != other.val); }
Logical Double::operator!=(const Double& other) const { return Logical(val != other.val); }
Logical Dual::operator!=(const Dual& other) const { return Logical(val != other.val); }

Logical Logical::operator&&(const Logical& other) const { return Logical(val && other.val); }
Logical Integer::operator&&(const Integer& other) const { return Logical(val && other.val); }
Logical Double::operator&&(const Double& other) const { return Logical(val && other.val); }
Logical Dual::operator&&(const Dual& other) const { return Logical(val && other.val); }

Logical Logical::operator||(const Logical& other) const { return Logical(val || other.val); }
Logical Integer::operator||(const Integer& other) const { return Logical(val || other.val); }
Logical Double::operator||(const Double& other) const { return Logical(val || other.val); }
Logical Dual::operator||(const Dual& other) const { return Logical(val || other.val); }

Double Logical::sin() const { return Double(std::sin(static_cast<double>(val))); }
Double Integer::sin() const { return Double(std::sin(static_cast<double>(val))); }
Double Double::sin() const { return Double(std::sin(val)); }
Dual Dual::sin() const { return Dual(std::sin(val), std::cos(val)); }

Double Logical::sinh() const { return Double(std::sinh(static_cast<double>(val))); }
Double Integer::sinh() const { return Double(std::sinh(static_cast<double>(val))); }
Double Double::sinh() const { return Double(std::sinh(val)); }
Dual Dual::sinh() const { return Dual(std::sinh(val), std::cosh(val)); }

Double Logical::asin() const { return Double(std::asin(static_cast<double>(val))); }
Double Integer::asin() const { return Double(std::asin(static_cast<double>(val))); }
Double Double::asin() const { return Double(std::asin(val)); }
Dual Dual::asin() const {
    return Dual(std::asin(val), 1.0 / std::sqrt(1.0 - val * val));
}

Double Logical::cos() const { return Double(std::cos(static_cast<double>(val))); }
Double Integer::cos() const { return Double(std::cos(static_cast<double>(val))); }
Double Double::cos() const { return Double(std::cos(val)); }
Dual Dual::cos() const { return Dual(std::cos(val), -std::sin(val)); }

Double Logical::cosh() const { return Double(std::cosh(static_cast<double>(val))); }
Double Integer::cosh() const { return Double(std::cosh(static_cast<double>(val))); }
Double Double::cosh() const { return Double(std::cosh(val)); }
Dual Dual::cosh() const { return Dual(std::cosh(val), std::sinh(val)); }

Double Logical::acos() const { return Double(std::acos(static_cast<double>(val))); }
Double Integer::acos() const { return Double(std::acos(static_cast<double>(val))); }
Double Double::acos() const { return Double(std::acos(val)); }
Dual Dual::acos() const {
    return Dual(std::acos(val), -1.0 / std::sqrt(1.0 - val * val));
}

Double Logical::tan() const { return Double(std::tan(static_cast<double>(val))); }
Double Integer::tan() const { return Double(std::tan(static_cast<double>(val))); }
Double Double::tan() const { return Double(std::tan(val)); }
Dual Dual::tan() const {
    return Dual(std::tan(val), 1.0 / (std::cos(val) * std::cos(val))); // sec^2(x)
}

Double Logical::tanh() const { return Double(std::tanh(static_cast<double>(val))); }
Double Integer::tanh() const { return Double(std::tanh(static_cast<double>(val))); }
Double Double::tanh() const { return Double(std::tanh(val)); }
Dual Dual::tanh() const {
    double t = std::tanh(val);
    return Dual(t, 1.0 - t * t);
}

Double Logical::atan() const { return Double(std::atan(static_cast<double>(val))); }
Double Integer::atan() const { return Double(std::atan(static_cast<double>(val))); }
Double Double::atan() const { return Double(std::atan(val)); }
Dual Dual::atan() const { return Dual(std::atan(val), 1.0 / (1.0 + val * val)); }

Double Logical::exp() const { return Double(std::exp(static_cast<double>(val))); }
Double Integer::exp() const { return Double(std::exp(static_cast<double>(val))); }
Double Double::exp() const { return Double(std::exp(val)); }
Dual Dual::exp() const { return Dual(std::exp(val), std::exp(val)); }

Double Logical::log() const { return Double(std::log(static_cast<double>(val))); }
Double Integer::log() const { return Double(std::log(static_cast<double>(val))); }
Double Double::log() const { return Double(std::log(val)); }
Dual Dual::log() const { return Dual(std::log(val), 1.0 / val); }

Double Logical::sqrt() const { return Double(std::sqrt(static_cast<double>(val))); }
Double Integer::sqrt() const { return Double(std::sqrt(static_cast<double>(val))); }
Double Double::sqrt() const { return Double(std::sqrt(val)); }
Dual Dual::sqrt() const { return Dual(std::sqrt(val), 0.5 / std::sqrt(val)); }

Integer Logical::operator-() const { return Integer(-static_cast<int>(val)); }
Integer Integer::operator-() const { return Integer(-val); }
Double Double::operator-() const { return Double(-val); }
Dual Dual::operator-() const { return Dual(-val, -dot); }
// Concepts for scalar
// --------------------------------------------------------------------------------------------------
template<typename T>
struct to_ast_scalar {
  using type = T;
};
template<> struct to_ast_scalar<double>  { using type = Double;  };
template<> struct to_ast_scalar<int>     { using type = Integer; };
template<> struct to_ast_scalar<bool>    { using type = Logical; };

template<typename T>
using to_ast_scalar_t = typename to_ast_scalar<T>::type;

// Common type of scalar types
// --------------------------------------------------------------------------------------------------
template<typename T>
using bare_t = std::remove_cv_t<std::remove_reference_t<T>>;

enum class ScalarRank { Logical, Integer, Double, Dual };

template<typename T> struct scalar_rank;

template<> struct scalar_rank<Logical> { static constexpr auto value = ScalarRank::Logical; };
template<> struct scalar_rank<Integer> { static constexpr auto value = ScalarRank::Integer; };
template<> struct scalar_rank<Double>  { static constexpr auto value = ScalarRank::Double; };
template<> struct scalar_rank<Dual>    { static constexpr auto value = ScalarRank::Dual; }; // highest

template<typename L, typename R>
using common_scalar = std::conditional_t<
    (static_cast<int>(scalar_rank<L>::value) >=
     static_cast<int>(scalar_rank<R>::value)),
    L, R
>;

template<typename L, typename R>
struct common_type {
  using type = common_scalar<
  bare_t<to_ast_scalar_t<L>>,
  bare_t<to_ast_scalar_t<R>>
  >;
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

// Traits (third dispatch layer)
// --------------------------------------------------------------------------------------------------
struct ComparisonTrait { using value_type = bool; };

struct LBufferTrait {};
struct RBufferTrait {};
struct SubsetViewTrait {};
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
- value_type

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
