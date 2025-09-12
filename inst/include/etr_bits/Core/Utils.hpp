#ifndef UTILS_ETR_H
#define UTILS_ETR_H

#include "Header.hpp"


namespace etr {

// Helper
// -----------------------------------------------------------------------------------------------------------
template <typename T, typename U>
inline constexpr bool IS = std::is_same_v<T, U>;

template <typename T> using IsArith = std::is_arithmetic<T>;
template <typename T> constexpr bool IsArithV = std::is_arithmetic_v<T>;
template <typename T> using Decayed = std::decay_t<T>;

template <typename T> using ReRef = std::remove_reference<T>;
template <typename T> constexpr bool IsRvalueV = std::is_rvalue_reference_v<T>;

template <typename T> using IsClass = std::is_class<T>;
template <typename T> constexpr bool IsClassV = std::is_class_v<T>;

template <typename T, typename U> constexpr bool is = IS<T, U>;

template <typename LT, typename LU, typename RT, typename RU>
constexpr bool isd = IS<LT, LU> && IS<RT, RU>;

template <typename T>
inline constexpr bool IsPair = false;
template <typename A, typename B>
inline constexpr bool IsPair<std::pair<A, B>> = true;

template <typename T1, typename T2>
typename std::common_type<T1, T2>::type CommonType(T1 arg1, T2 arg2) {
  return arg1 + arg2; // Fake function just for getting the common type
}

template <bool B> using BoolConstant = std::integral_constant<bool, B>;
using TRUE = BoolConstant<true>;
using T = BoolConstant<true>;
using FALSE = BoolConstant<false>;
using F = BoolConstant<false>;

// TODO: check how r values behave here
const double NA = std::numeric_limits<double>::quiet_NaN();
const double NaN = std::numeric_limits<double>::quiet_NaN();
const double Inf = std::numeric_limits<double>::infinity();

inline int d2i(double inp) { return static_cast<int>(inp); }
inline double i2d(int inp) { return static_cast<double>(inp); }
inline bool d2b(double inp) { return static_cast<bool>(inp); }
inline bool i2b(double inp) { return static_cast<bool>(inp); }
inline double b2d(bool inp) { return static_cast<double>(inp); }

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
  if (!inp)
    throw std::runtime_error(msg.value.data());
#else
  if (!inp)
    Rcpp::stop(msg.value.data());
#endif
}

inline void warn(bool inp, std::string message) {
#ifdef STANDALONE_ETR
  if (!inp)
    std::cerr << "Warning: " + message << std::endl;
#else
  if (!inp)
    Rcpp::warning("Warning: " + message);
#endif
}

template <string_literal msg> inline void warn(bool inp) {
#ifdef STANDALONE_ETR
  if (!inp)
    std::cerr << msg.value.data() << std::endl;
#else
  if (!inp)
    Rcpp::warning(msg.value.data());
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

} // namespace etr

#endif
