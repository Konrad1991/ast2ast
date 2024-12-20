#ifndef UTILS_ETR_H
#define UTILS_ETR_H

#include "Header.hpp"

namespace etr {
template <typename T, typename U> constexpr bool is = IS<T, U>;
template <typename LT, typename LU, typename RT, typename RU>
constexpr bool isd = IS<LT, LU> && IS<RT, RU>;

template <typename T1, typename T2>
typename std::common_type<T1, T2>::type CommonType(T1 arg1, T2 arg2) {
  return arg1 + arg2; // Fake function just for getting the common type
}

template <typename T> inline bool isDoubleInt(const T d) {
  int i = d;
  if ((d - i) > 0) {
    return false;
  }
  return true;
}

// TODO: check is this needed anymore
typedef double BaseType;

template <bool B> using BoolConstant = std::integral_constant<bool, B>;
using TRUE = BoolConstant<true>;
using T = BoolConstant<true>;
using FALSE = BoolConstant<false>;
using F = BoolConstant<false>;

// TODO: check how r values behave here
const double NA = std::numeric_limits<double>::quiet_NaN();
const double Inf = std::numeric_limits<double>::infinity();

inline int d2i(double inp) { return static_cast<int>(inp); }

inline double i2d(int inp) { return static_cast<double>(inp); }

inline bool d2b(double inp) { return static_cast<bool>(inp); }

inline bool i2b(double inp) { return static_cast<bool>(inp); }

inline double b2d(bool inp) { return static_cast<double>(inp); }

template <typename T> struct It {
  T *p;
  T &operator*() { return *p; }
  bool operator!=(const It &rhs) { return p != rhs.p; }
  void operator++() { ++p; }
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
  std::size_t sz = 0;
  SI(const std::size_t sz_) : sz(sz_) {}
  SI(const int sz_) : sz(static_cast<std::size_t>(sz_)) {}
  SI(const long long sz_) : sz(static_cast<std::size_t>(sz_)) {}
};

} // namespace etr

#endif
