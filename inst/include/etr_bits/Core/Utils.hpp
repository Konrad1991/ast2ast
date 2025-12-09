#ifndef UTILS_ETR_H
#define UTILS_ETR_H

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

// FIX: need a better soultion. maybe just use a define.
// Logical l = TRUE does not work
// Logical l(TRUE) works. But l has type: etr::Logical (std::integral_constant<bool, true>)
// THerefore, IsScalar does not recognise it
// Logical l = true has type: etr::Logical --> here IsScalar works

// FIX: Moreover, the R code translates TRUE to true, and FALSE to false
// However, the R code does not handle T and F correctly.

// TODO: Furthermore, NaN has to be removed

// TODO: handle NA and Inf in each algorithm explicitly

template <bool B> using BoolConstant = std::integral_constant<bool, B>;
using TRUE = BoolConstant<true>;
using T = BoolConstant<true>;
using FALSE = BoolConstant<false>;
using F = BoolConstant<false>;
const double NA = std::numeric_limits<double>::quiet_NaN();
const double NaN = std::numeric_limits<double>::quiet_NaN();
const double Inf = std::numeric_limits<double>::infinity();

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

template <class F, class... Args> inline F forEachArg(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...); return f;
}

} // namespace etr

#endif
