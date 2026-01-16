#ifndef TYPES_ETR_H
#define TYPES_ETR_H

#include <cmath>
#include <limits>

#ifdef STANDALONE_ETR
#include "stddef.h"
#include <cxxabi.h>
#else
/*
 Actually Rcpp would be sufficent.
 But when I include Rcpp.h I get the error in paropt that compilation fails
*/
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#endif

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

// Handler which store reference to r for L values and otherwise copy the value to keep it alive
// -----------------------------------------------------------------------------------------------------------
template <class T>
class ConstHolder {
  std::shared_ptr<T> owned; // used only when constructed from rvalue
  const T* ptr = nullptr;

public:
  ConstHolder(const T& ref) : ptr(&ref) {}

  ConstHolder(T&& r)
    : owned(std::make_shared<T>(std::move(r))), ptr(owned.get()) {}

  ConstHolder(const ConstHolder&) = default;
  ConstHolder(ConstHolder&&) noexcept = default;
  ConstHolder& operator=(const ConstHolder&) = default;
  ConstHolder& operator=(ConstHolder&&) noexcept = default;

  const T& get() const { return *ptr; }
};


template <typename T>
class Holder {
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

// Helper
// -----------------------------------------------------------------------------------------------------------
template <typename T, typename U>
inline constexpr bool IS = std::is_same_v<T, U>;

template <typename T> using IsCppArith = std::is_arithmetic<T>;
template <typename T> constexpr bool IsCppArithV = std::is_arithmetic_v<T>;

template <typename T> using Decayed = std::decay_t<T>;

template <typename T> using ReRef = std::remove_reference<T>;
template <typename T> constexpr bool IsRvalueV = std::is_rvalue_reference_v<T>;

template <typename T> using IsClass = std::is_class<T>;
template <typename T> constexpr bool IsClassV = std::is_class_v<T>;

template <typename T> struct It {
  T *p;
  T &operator*() const { return *p; }
  bool operator!=(const It &rhs) const { return p != rhs.p; }
  bool operator==(const It& rhs) const { return p == rhs.p; }
  It& operator++() { ++p; return *this; }
};

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

inline std::size_t safe_modulo(std::size_t idx, std::size_t sz) {
  return sz ? (idx % sz) : 0;  // no UB if n==0
}
template <class F, class... Args> inline F forEachArg(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...); return f;
}
template<typename Dim>
auto&& dim_view(Dim& d) {
  if constexpr (requires { d.get(); }) {
    return d.get();
  } else {
    return d;
  }
}

inline std::size_t safe_index_from_double(double x) {
  ass<"invalid index argument">(std::isfinite(x));
  double t = std::trunc(x);
  ass<"Negative indices are not supported">(t >= 1);
  return static_cast<std::size_t>(t);
}

// Scalar types (First dispatch layer)
// --------------------------------------------------------------------------------------------------
struct Logical;
struct Integer;
struct Double;
struct Dual;
struct LogicalRef;
struct IntegerRef;
struct DoubleRef;
struct DualRef;
template<typename T> struct Variable;
template<typename T> struct Expr;
template<typename T> using ExprPtr = std::shared_ptr<Expr<T>>;
struct BooleanExpr;

struct Logical {
  bool val;
  bool is_na{false};
  inline operator bool() const { return val; }
  inline Logical();
  inline Logical(bool v);
  inline Logical(Integer v);
  inline Logical(Double v);
  inline Logical(Dual v);
  template<typename T> Logical(Variable<T> v);
  inline Integer operator+(const Logical&) const;
  inline Integer operator-(const Logical&) const;
  inline Integer operator*(const Logical&) const;
  inline Double operator/(const Logical&) const;
  inline Double pow(const Logical&) const;
  inline Logical operator==(const Logical&) const;
  inline Logical operator<(const Logical&) const;
  inline Logical operator<=(const Logical&) const;
  inline Logical operator>(const Logical&) const;
  inline Logical operator>=(const Logical&) const;
  inline Logical operator!=(const Logical&) const;
  inline Logical operator&&(const Logical&) const;
  inline Logical operator||(const Logical&) const;
  inline Double sin() const;
  inline Double asin() const;
  inline Double sinh() const;
  inline Double cos() const;
  inline Double acos() const;
  inline Double cosh() const;
  inline Double tan() const;
  inline Double atan() const;
  inline Double tanh() const;
  inline Double exp() const;
  inline Double log() const;
  inline Double sqrt() const;
  inline Integer operator-() const;
  inline friend std::ostream& operator<<(std::ostream&, const Logical&);
  inline static Logical NA() { Logical x; x.is_na = true; return x; }
  inline bool isNA() const noexcept {
    return is_na;
  }
};

struct Integer {
  int val;
  bool is_na{false};
  inline Integer();
  inline Integer(int v);
  inline Integer(Logical v);
  inline Integer(Double v);
  inline Integer(Dual v);
  template<typename T> Integer(Variable<T> v);
  inline Integer operator+(const Integer&) const;
  inline Integer operator-(const Integer&) const;
  inline Integer operator*(const Integer&) const;
  inline Double operator/(const Integer&) const;
  inline Double pow(const Integer&) const;
  inline Logical operator==(const Integer&) const;
  inline Logical operator<(const Integer&) const;
  inline Logical operator<=(const Integer&) const;
  inline Logical operator>(const Integer&) const;
  inline Logical operator>=(const Integer&) const;
  inline Logical operator!=(const Integer&) const;
  inline Logical operator&&(const Integer&) const;
  inline Logical operator||(const Integer&) const;
  inline Double sin() const;
  inline Double asin() const;
  inline Double sinh() const;
  inline Double cos() const;
  inline Double acos() const;
  inline Double cosh() const;
  inline Double tan() const;
  inline Double atan() const;
  inline Double tanh() const;
  inline Double exp() const;
  inline Double log() const;
  inline Double sqrt() const;
  inline Integer operator-() const;
  inline friend std::ostream& operator<<(std::ostream&, const Integer&);
  inline static Integer NA() { Integer x; x.is_na = true; return x; }
  inline bool isNA() const noexcept {
    return is_na;
  }
};

struct Double {
  double val;
  bool is_na;
  inline Double();
  inline Double(double v);
  inline Double(Logical v);
  inline Double(Integer v);
  inline Double(Dual v);
  inline Double operator+(const Double&) const;
  inline Double operator-(const Double&) const;
  inline Double operator*(const Double&) const;
  inline Double operator/(const Double&) const;
  inline Double pow(const Double&) const;
  inline Logical operator==(const Double&) const;
  inline Logical operator<(const Double&) const;
  inline Logical operator<=(const Double&) const;
  inline Logical operator>(const Double&) const;
  inline Logical operator>=(const Double&) const;
  inline Logical operator!=(const Double&) const;
  inline Logical operator&&(const Double&) const;
  inline Logical operator||(const Double&) const;
  inline Double sin() const;
  inline Double asin() const;
  inline Double sinh() const;
  inline Double cos() const;
  inline Double acos() const;
  inline Double cosh() const;
  inline Double tan() const;
  inline Double atan() const;
  inline Double tanh() const;
  inline Double exp() const;
  inline Double log() const;
  inline Double sqrt() const;
  inline Double operator-() const;
  inline friend std::ostream& operator<<(std::ostream&, const Double&);
  inline static Double NA() {
    Double x(std::numeric_limits<double>::quiet_NaN());
    x.is_na = true;
    return x;
  }
  inline static Double NaN() {
    return Double(std::numeric_limits<double>::quiet_NaN());
  }
  inline static Double Inf() {
    return Double(std::numeric_limits<double>::infinity());
  }
  inline bool isNA() const noexcept {
    return is_na;
  }
  inline bool isNaN() const noexcept {
    return !is_na && std::isnan(val);
  }
  inline bool isFinite() const noexcept {
    return !is_na && std::isfinite(val);
  }
  inline bool isInfinite() const noexcept {
    return !is_na && !isNaN() && !std::isfinite(val);
  }
};

struct Dual {
  double val, dot;
  bool is_na;
  bool is_na_dot;
  inline Dual();
  inline Dual(double v, double d);
  inline Dual(Logical v);
  inline Dual(Integer v);
  inline Dual(Double v);
  // Does not require a ctr for Variable. Forward and reverse mode AD cannot be mixed!
  template<typename T> requires std::is_arithmetic_v<T>
  Dual(T v);
  inline Dual operator+(const Dual&) const;
  inline Dual operator-(const Dual&) const;
  inline Dual operator*(const Dual&) const;
  inline Dual operator/(const Dual&) const;
  inline Dual pow(const Dual&) const;
  inline Logical operator==(const Dual&) const;
  inline Logical operator<(const Dual&) const;
  inline Logical operator<=(const Dual&) const;
  inline Logical operator>(const Dual&) const;
  inline Logical operator>=(const Dual&) const;
  inline Logical operator!=(const Dual&) const;
  inline Logical operator&&(const Dual&) const;
  inline Logical operator||(const Dual&) const;
  inline Dual sin() const;
  inline Dual asin() const;
  inline Dual sinh() const;
  inline Dual cos() const;
  inline Dual acos() const;
  inline Dual cosh() const;
  inline Dual tan() const;
  inline Dual atan() const;
  inline Dual tanh() const;
  inline Dual exp() const;
  inline Dual log() const;
  inline Dual sqrt() const;
  inline Dual operator-() const;
  inline friend std::ostream& operator<<(std::ostream&, const Dual&);
  inline static Dual NA() {
    Dual x(std::numeric_limits<double>::quiet_NaN(),
           std::numeric_limits<double>::quiet_NaN());
    x.is_na = true;
    x.is_na_dot = true;
    return x;
  }
  inline static Dual NaN() {
    return Dual (std::numeric_limits<double>::quiet_NaN(),
           std::numeric_limits<double>::quiet_NaN());
  }
  inline static Dual Inf() {
    return Dual(std::numeric_limits<double>::infinity(),
                std::numeric_limits<double>::infinity());
  }
  inline bool isNA() const noexcept {
    return is_na;
  }
  inline bool isNADot() const noexcept {
    return is_na_dot;
  }
  inline bool isNaN() const noexcept {
    return !is_na && std::isnan(val);
  }
  inline bool isNaNDot() const noexcept {
    return !is_na_dot && std::isnan(dot);
  }
  inline bool isFinite() const noexcept {
    return !is_na && std::isfinite(val);
  }
  inline bool isInfinite() const noexcept {
    return !is_na && !isNaN() && !std::isfinite(val);
  }
  inline bool isFiniteDot() const noexcept {
    return !is_na_dot && std::isfinite(dot);
  }
  inline bool isInfiniteDot() const noexcept {
    return !is_na_dot && !isNaNDot() && !std::isfinite(dot);
  }
};

inline Logical::Logical() : val(false), is_na(false) {}
inline Logical::Logical(bool v) : val(v), is_na(false) {}
inline Logical::Logical(Integer v) : val(static_cast<bool>(v.val)), is_na(v.is_na) {}
inline Logical::Logical(Double v)  : val(static_cast<bool>(v.val)), is_na(v.is_na) {}
inline Logical::Logical(Dual v)    : val(static_cast<bool>(v.val)), is_na(v.is_na) {}

inline Integer::Integer() : val(0), is_na(false) {}
inline Integer::Integer(int v) : val(v), is_na(false) {}
inline Integer::Integer(Logical v) : val(static_cast<int>(v.val)), is_na(v.is_na) {}
inline Integer::Integer(Double v)  : val(static_cast<int>(v.val)), is_na(v.is_na) {}
inline Integer::Integer(Dual v)    : val(static_cast<int>(v.val)), is_na(v.is_na) {}

inline Double::Double() : val(0.0), is_na(false) {}
inline Double::Double(double v) : val(v), is_na(false) {}
inline Double::Double(Logical v) : val(static_cast<double>(v.val)), is_na(v.is_na) {}
inline Double::Double(Integer v) : val(static_cast<double>(v.val)), is_na(v.is_na) {}
inline Double::Double(Dual v)    : val(v.val), is_na(v.is_na) {}

inline Dual::Dual() : val(0.0), dot(0.0), is_na(false), is_na_dot(false) {}
inline Dual::Dual(double v, double d) : val(v), dot(d), is_na(false), is_na_dot(false) {}
inline Dual::Dual(Logical v) : val(static_cast<double>(v.val)), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
inline Dual::Dual(Integer v) : val(static_cast<double>(v.val)), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
inline Dual::Dual(Double v)    : val(v.val), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
template<typename T> requires std::is_arithmetic_v<T>
inline Dual::Dual(T v)    : val(static_cast<double>(v)), dot(0.0), is_na(std::isnan(v)), is_na_dot(false) {}

inline std::ostream& operator<<(std::ostream& os, const Logical& x) {
  if (x.isNA()) return os << "NA";
  if (x.val) return os << "TRUE"; else return os << "FALSE";
}
inline std::ostream& operator<<(std::ostream& os, const Integer& x) {
  if (x.isNA()) return os << "NA";
  return os << x.val;
}
inline std::ostream& operator<<(std::ostream& os, const Double& x) {
  if (x.isNA()) return os << "NA";
  if (x.isNaN()) return os << "NaN";
  if (!x.isFinite())   return os << (x.val > 0 ? "Inf" : "-Inf");
  return os << x.val;
}
inline std::ostream& operator<<(std::ostream& os, const Dual& x) {
  auto print_scalar = [&](double v, bool is_na) -> std::ostream& {
    if (is_na) return os << "NA";
    if (std::isnan(v)) return os << "NaN";
    if (!std::isfinite(v)) return os << (v > 0 ? "Inf" : "-Inf");
    return os << v;
  };
  os << "(";
  print_scalar(x.val, x.isNA());
  os << ", ";
  print_scalar(x.dot, x.isNADot());
  os << ")";
  return os;
}

inline Integer Logical::operator+(const Logical& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val + other.val);
}
inline Integer Integer::operator+(const Integer& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val + other.val);
}
inline Double Double::operator+(const Double& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(val + other.val);
}
inline Dual Dual::operator+(const Dual& other) const {
  if (is_na || other.is_na) return Dual::NA();
  const double v = val + other.val;
  if (is_na_dot || other.is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, dot + other.dot);
}

inline Integer Logical::operator-(const Logical& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val - other.val);
}
inline Integer Integer::operator-(const Integer& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val - other.val);
}
inline Double Double::operator-(const Double& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(val - other.val);
}
inline Dual Dual::operator-(const Dual& other) const {
  if (is_na || other.is_na) return Dual::NA();
  const double v = val - other.val;
  if (is_na_dot || other.is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, dot - other.dot);
}

inline Integer Logical::operator*(const Logical& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val * other.val);
}
inline Integer Integer::operator*(const Integer& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val * other.val);
}
inline Double Double::operator*(const Double& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(val * other.val);
}
inline Dual Dual::operator*(const Dual& other) const {
  if (is_na || other.is_na) return Dual::NA();
  const double v = val * other.val;
  if (is_na_dot || other.is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, dot * other.val + val * other.dot);
}

inline Double Logical::operator/(const Logical& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(static_cast<double>(val) / static_cast<double>(other.val));
}
inline Double Integer::operator/(const Integer& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(static_cast<double>(val) / static_cast<double>(other.val));
}
inline Double Double::operator/(const Double& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(val / other.val);
}
inline Dual Dual::operator/(const Dual& other) const {
  if (is_na || other.is_na) return Dual::NA();
  const double v = val / other.val;
  if (is_na_dot || other.is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, (dot * other.val - val * other.dot) / (other.val * other.val));
}

inline Double Logical::pow(const Logical& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(std::pow(static_cast<double>(val), static_cast<double>(other.val)));
}
inline Double Integer::pow(const Integer& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(std::pow(static_cast<double>(val), static_cast<double>(other.val)));
}
inline Double Double::pow(const Double& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(std::pow(val, other.val));
}
inline Dual Dual::pow(const Dual& other) const {
  if (is_na || other.is_na) return Dual::NA();
  const double f = std::pow(val, other.val);
  if (!std::isfinite(f) || is_na_dot || other.is_na_dot) return Dual(f, std::numeric_limits<double>::quiet_NaN());
  const double d = f * (other.dot * std::log(val) + other.val * dot / val);
  return Dual(f, d);
}

inline Logical Logical::operator==(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val == other.val);
}
inline Logical Integer::operator==(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val == other.val);
}
inline Logical Double::operator==(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val == other.val);
}
inline Logical Dual::operator==(const Dual& other) const {
  if ( is_na || other.is_na) return Logical::NA();
  return Logical(val == other.val);
}

inline Logical Logical::operator<(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val < other.val);
}
inline Logical Integer::operator<(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val < other.val);
}
inline Logical Double::operator<(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val < other.val);
}
inline Logical Dual::operator<(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val < other.val);
}

inline Logical Logical::operator<=(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val <= other.val);
}
inline Logical Integer::operator<=(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val <= other.val);
}
inline Logical Double::operator<=(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val <= other.val);
}
inline Logical Dual::operator<=(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val <= other.val);
}

inline Logical Logical::operator>(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val > other.val);
}
inline Logical Integer::operator>(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val > other.val);
}
inline Logical Double::operator>(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val > other.val);
}
inline Logical Dual::operator>(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val > other.val);
}

inline Logical Logical::operator>=(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val >= other.val);
}
inline Logical Integer::operator>=(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val >= other.val);
}
inline Logical Double::operator>=(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val >= other.val);
}
inline Logical Dual::operator>=(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val >= other.val);
}

inline Logical Logical::operator!=(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val != other.val);
}
inline Logical Integer::operator!=(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val != other.val);
}
inline Logical Double::operator!=(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val != other.val);
}
inline Logical Dual::operator!=(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val != other.val);
}

inline Logical Logical::operator&&(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}
inline Logical Integer::operator&&(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}
inline Logical Double::operator&&(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}
inline Logical Dual::operator&&(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}

inline Logical Logical::operator||(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}
inline Logical Integer::operator||(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}
inline Logical Double::operator||(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}
inline Logical Dual::operator||(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}

inline Double Logical::sin() const {
  if (is_na) return Double::NA();
  return Double(std::sin(static_cast<double>(val)));
}
inline Double Integer::sin() const {
  if (is_na) return Double::NA();
  return Double(std::sin(static_cast<double>(val)));
}
inline Double Double::sin() const {
  if (is_na) return Double::NA();
  return Double(std::sin(val));
}
inline Dual Dual::sin() const {
  if (is_na) return Dual::NA();
  const double v = std::sin(val);
  if (is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, std::cos(val));
}

inline Double Logical::sinh() const {
  if (is_na) return Double::NA();
  return Double(std::sinh(static_cast<double>(val)));
}
inline Double Integer::sinh() const {
  if (is_na) return Double::NA();
  return Double(std::sinh(static_cast<double>(val)));
}
inline Double Double::sinh() const {
  if (is_na) return Double::NA();
  return Double(std::sinh(val));
}
inline Dual Dual::sinh() const {
  if (is_na) return Dual::NA();
  const double v = std::sinh(val);
  if (is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, std::cosh(val));
}

inline Double Logical::asin() const {
  if (is_na) return Double::NA();
  return Double(std::asin(static_cast<double>(val)));
}
inline Double Integer::asin() const {
  if (is_na) return Double::NA();
  return Double(std::asin(static_cast<double>(val)));
}
inline Double Double::asin() const {
  if (is_na) return Double::NA();
  return Double(std::asin(val));
}
inline Dual Dual::asin() const {
  if (is_na) return Dual::NA();
  const double v = std::asin(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 1.0 / std::sqrt(1.0 - val * val));
}

inline Double Logical::cos() const {
  if (is_na) return Double::NA();
  return Double(std::cos(static_cast<double>(val)));
}
inline Double Integer::cos() const {
  if (is_na) return Double::NA();
  return Double(std::cos(static_cast<double>(val)));
}
inline Double Double::cos() const {
  if (is_na) return Double::NA();
  return Double(std::cos(val));
}
inline Dual Dual::cos() const {
  if (is_na) return Dual::NA();
  const double v = std::cos(val);
  if (is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, -std::sin(val));
}

inline Double Logical::cosh() const {
  if (is_na) return Double::NA();
  return Double(std::cosh(static_cast<double>(val)));
}
inline Double Integer::cosh() const {
  if (is_na) return Double::NA();
  return Double(std::cosh(static_cast<double>(val)));
}
inline Double Double::cosh() const {
  if (is_na) return Double::NA();
  return Double(std::cosh(val));
}
inline Dual Dual::cosh() const {
  if (is_na) return Dual::NA();
  const double v = std::cosh(val);
  if (is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, std::sinh(val));
}

inline Double Logical::acos() const {
  if (is_na) return Double::NA();
  return Double(std::acos(static_cast<double>(val)));
}
inline Double Integer::acos() const {
  if (is_na) return Double::NA();
  return Double(std::acos(static_cast<double>(val)));
}
inline Double Double::acos() const {
  if (is_na) return Double::NA();
  return Double(std::acos(val));
}
inline Dual Dual::acos() const {
  if (is_na) return Dual::NA();
  const double v = std::acos(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, -1.0 / std::sqrt(1.0 - val * val));
}

inline Double Logical::tan() const {
  if (is_na) return Double::NA();
  return Double(std::tan(static_cast<double>(val)));
}
inline Double Integer::tan() const {
  if (is_na) return Double::NA();
  return Double(std::tan(static_cast<double>(val)));
}
inline Double Double::tan() const {
  if (is_na) return Double::NA();
  return Double(std::tan(val));
}
inline Dual Dual::tan() const {
  if (is_na) return Dual::NA();
  const double v = std::tan(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 1.0 / (std::cos(val) * std::cos(val)));
}

inline Double Logical::tanh() const {
  if (is_na) return Double::NA();
  return Double(std::tanh(static_cast<double>(val)));
}
inline Double Integer::tanh() const {
  if (is_na) return Double::NA();
  return Double(std::tanh(static_cast<double>(val)));
}
inline Double Double::tanh() const {
  if (is_na) return Double::NA();
  return Double(std::tanh(val));
}
inline Dual Dual::tanh() const {
  if (is_na) return Dual::NA();
  const double t = std::tanh(val);
  if (is_na_dot) return Dual(t, std::numeric_limits<double>::quiet_NaN());
  return Dual(t, 1.0 - t * t);
}

inline Double Logical::atan() const {
  if (is_na) return Double::NA();
  return Double(std::atan(static_cast<double>(val)));
}
inline Double Integer::atan() const {
  if (is_na) return Double::NA();
  return Double(std::atan(static_cast<double>(val)));
}
inline Double Double::atan() const {
  if (is_na) return Double::NA();
  return Double(std::atan(val));
}
inline Dual Dual::atan() const {
  if (is_na) return Dual::NA();
  const double v = std::atan(val);
  if (is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 1.0 / (1.0 + val * val));
}

inline Double Logical::exp() const {
  if (is_na) return Double::NA();
  return Double(std::exp(static_cast<double>(val)));
}
inline Double Integer::exp() const {
  if (is_na) return Double::NA();
  return Double(std::exp(static_cast<double>(val)));
}
inline Double Double::exp() const {
  if (is_na) return Double::NA();
  return Double(std::exp(val));
}
inline Dual Dual::exp() const {
  if (is_na) return Dual::NA();
  const double v = std::exp(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, v);
}

inline Double Logical::log() const {
  if (is_na) return Double::NA();
  return Double(std::log(static_cast<double>(val)));
}
inline Double Integer::log() const {
  if (is_na) return Double::NA();
  return Double(std::log(static_cast<double>(val)));
}
inline Double Double::log() const {
  if (is_na) return Double::NA();
  return Double(std::log(val));
}
inline Dual Dual::log() const {
  if (is_na) return Dual::NA();
  const double v = std::log(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 1.0 / val);
}

inline Double Logical::sqrt() const {
  if (is_na) return Double::NA();
  return Double(std::sqrt(static_cast<double>(val)));
}
inline Double Integer::sqrt() const {
  if (is_na) return Double::NA();
  return Double(std::sqrt(static_cast<double>(val)));
}
inline Double Double::sqrt() const {
  if (is_na) return Double::NA();
  return Double(std::sqrt(val));
}
inline Dual Dual::sqrt() const {
  if (is_na) return Dual::NA();
  const double v = std::sqrt(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 0.5 / v);
}

inline Integer Logical::operator-() const {
  if (is_na) return Integer::NA();
  return Integer(-static_cast<int>(val));
}
inline Integer Integer::operator-() const {
  if (is_na) return Integer::NA();
  return Integer(-val);
}
inline Double Double::operator-() const {
  if (is_na) return Double::NA();
  return Double(-val);
}
inline Dual Dual::operator-() const {
  if (is_na) return Dual::NA();
  if (is_na_dot) return Dual(-val, std::numeric_limits<double>::quiet_NaN());
  return Dual(-val, -dot);
}

// Concepts for scalar
// --------------------------------------------------------------------------------------------------
template<typename T>
struct from_ast_scalar {
  using type = T;
};
template<> struct from_ast_scalar<Double>  { using type = double;  };
template<> struct from_ast_scalar<Integer>     { using type = int; };
template<> struct from_ast_scalar<Logical>    { using type = bool; };
template<> struct from_ast_scalar<Variable<Double>> { using type = double; };
template<typename T>
using from_ast_scalar_t = typename from_ast_scalar<T>::type;

template<typename T>
struct to_ast_scalar {
  using type = T;
};
template<> struct to_ast_scalar<double>  { using type = Double;  };
template<> struct to_ast_scalar<int>     { using type = Integer; };
template<> struct to_ast_scalar<bool>    { using type = Logical; };

template<> struct to_ast_scalar<LogicalRef>  { using type = Logical; };
template<> struct to_ast_scalar<IntegerRef>  { using type = Integer; };
template<> struct to_ast_scalar<DoubleRef>   { using type = Double;  };
template<> struct to_ast_scalar<DualRef>     { using type = Dual;    };

template<typename T>
using to_ast_scalar_t = typename to_ast_scalar<T>::type;

// Common type of scalar types
// --------------------------------------------------------------------------------------------------
template<typename T>
using bare_t = std::remove_cv_t<std::remove_reference_t<T>>;

enum class ScalarRank { Logical, Integer, Double, Dual, Variable };

// template<typename T> struct scalar_rank;
template <class T>
struct scalar_rank {
  static constexpr int value = -1;
};

template<> struct scalar_rank<Logical> { static constexpr auto value = ScalarRank::Logical; };
template<> struct scalar_rank<Integer> { static constexpr auto value = ScalarRank::Integer; };
template<> struct scalar_rank<Double>  { static constexpr auto value = ScalarRank::Double; };
template<> struct scalar_rank<Dual>    { static constexpr auto value = ScalarRank::Dual; };
template<> struct scalar_rank<Variable<Double>>    { static constexpr auto value = ScalarRank::Variable; }; // highest

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
std::same_as<bare_t<T>, Double> ||
std::same_as<bare_t<T>, Dual>;
template <typename T> constexpr bool IsArithV = IsArith<T>;

// Concept to detect refs to scalars
template<typename T> concept IsArithRef =
std::same_as<bare_t<T>, LogicalRef> ||
std::same_as<bare_t<T>, IntegerRef> ||
std::same_as<bare_t<T>, DoubleRef> ||
std::same_as<bare_t<T>, DualRef>;
template <typename T> constexpr bool IsArithRefV = IsArithRef<T>;

template<typename T> concept IsDouble = std::same_as<T, Double>;
template<typename T> concept IsInteger = std::same_as<T, Integer>;
template<typename T> concept IsLogical = std::same_as<T, Logical>;
template<typename T> concept IsDual = std::same_as<T, Dual>;

template<typename T> concept IsDoubleRef = std::same_as<T, DoubleRef>;
template<typename T> concept IsIntegerRef = std::same_as<T, IntegerRef>;
template<typename T> concept IsLogicalRef = std::same_as<T, LogicalRef>;
template<typename T> concept IsDualRef = std::same_as<T, DualRef>;

// reverse ad
template <typename T> struct is_any_variable : std::false_type {};
template <typename T> struct is_any_variable<Variable<T>> : std::true_type {};
template <typename T> inline constexpr bool is_any_variable_v = is_any_variable<T>::value;
template <typename T> concept IsVariable = is_any_variable_v<T>;

template <typename T> struct is_any_expr : std::false_type {};
template <typename T> struct is_any_expr<ExprPtr<T>> : std::true_type {};
template <typename T> inline constexpr bool is_any_expr_v = is_any_expr<T>::value;
template <typename T> concept IsExpr = is_any_expr_v<T>;

template <typename T> concept IsADType = requires(T t) {
  typename T;
  requires IsVariable<T> || IsExpr<T> || IS<T, BooleanExpr>;
};

template <typename T> concept IsScalarLike = requires(T t) {
  typename T;
  requires IsArithV<T> || IsArithRefV<T> || IsADType<T>;
};
template <typename T> concept IsScalarOrScalarRef = requires(T t) {
  typename T;
  requires IsArithV<T> || IsArithRefV<T>;
};

// Traits (third dispatch layer)
// --------------------------------------------------------------------------------------------------
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
template<typename O, std::size_t N, typename Trait = ConstSubsetViewTrait> struct ConstSubsetView;

// Outer data structs
// -----------------------------------------------------------------------------------------------------------
template<typename T, typename R>
struct Array { static_assert(sizeof(R) == 0, "No generic Array<T,R> implementation"); };
template <typename T, typename Trait> struct Array<T, Buffer<T, Trait>>;
template <typename T, typename Trait> struct Array<T, Borrow<T, Trait>>;
template <typename T, typename I, typename Trait> struct Array<T, UnaryOperation<I, Trait>>;
template <typename T, typename L, typename R, typename Trait> struct Array<T, BinaryOperation<L, R, Trait>>;
template <typename T, typename O, std::size_t N, typename Trait> struct Array<T, SubsetView<O, N, Trait>>;
template <typename T, typename O, std::size_t N, typename Trait> struct Array<T, ConstSubsetView<O, N, Trait>>;

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

// Concepts
// -----------------------------------------------------------------------------------------------------------
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

template<typename O, std::size_t N, typename Trait>
struct const_subsetview_traits<ConstSubsetView<O, N, Trait>> {
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

// Second dispatch layer
// --------------------------------------------------------------------------------------------------
// unary - ================================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto operator-(const O& o) -> decltype(o.operator-()) {
  return o.operator-();
}
// sqrt ===================================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto sqrt(const O& o) -> decltype(o.sqrt()) {
  return o.sqrt();
}
// exp ===================================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto exp(const O& o) -> decltype(o.exp()) {
  return o.exp();
}
// log ===================================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto log(const O& o) -> decltype(o.log()) {
  return o.log();
}
// tan/atan/tanh =========================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto tan(const O& o) -> decltype(o.tan()) {
  return o.tan();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto atan(const O& o) -> decltype(o.atan()) {
  return o.atan();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto tanh(const O& o) -> decltype(o.tanh()) {
  return o.tanh();
}
// cos/acos/cosh =========================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto cos(const O& o) -> decltype(o.cos()) {
  return o.cos();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto acos(const O& o) -> decltype(o.acos()) {
  return o.acos();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto cosh(const O& o) -> decltype(o.cosh()) {
  return o.cosh();
}
// sin/asin/sinh =========================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto sin(const O& o) -> decltype(o.sin()) {
  return o.sin();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto asin(const O& o) -> decltype(o.asin()) {
  return o.asin();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto sinh(const O& o) -> decltype(o.sinh()) {
  return o.sinh();
}

// && ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator&&(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator&&( common_type_t<L,R>(r) ) ) {
  using CT = common_type_t<L, R>;
  return CT(l).operator&&( CT(r) );
}

// || ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator||(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator||( common_type_t<L,R>(r) ) ) {
  using CT = common_type_t<L, R>;
  return CT(l).operator||( CT(r) );
}

// != ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && (IsScalarOrScalarRef<L> || IsCppArithV<L>) && (IsScalarOrScalarRef<R> || IsCppArithV<R>) )
inline auto operator!=(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator!=( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator!=( CT(r) );
}
// >= ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator>=(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator>=( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator>=( CT(r) );
}
// > ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator>(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator>( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator>( CT(r) );
}
// <= ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator<=(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator<=( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator<=( CT(r) );
}
// < =====================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator<(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator<( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator<( CT(r) );
}
// == ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator==(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator==( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator==( CT(r) );
}
// pow ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto pow(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).pow( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).pow( CT(r) );
}
// / ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator/(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator/( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator/( CT(r) );
}
// + ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator+(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator+( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator+( CT(r) );
}
// - ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator-(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator-( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator-( CT(r) );
}
// * ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator*(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator*( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator*( CT(r) );
}

} // namespace etr

#endif
