#ifndef UTILS_ETR_H
#define UTILS_ETR_H

#include "Header.hpp"

namespace etr {
template <typename T, typename U> constexpr bool is = std::is_same_v<T, U>;
template <typename LT, typename LU, typename RT, typename RU>
constexpr bool isd = std::is_same_v<LT, LU> && std::is_same_v<RT, RU>;

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

inline void ass(bool inp, std::string message) {
#ifdef STANDALONE_ETR
  if (!inp)
    throw std::runtime_error(message);
#else
  if (!inp)
    Rcpp::stop(message);
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

struct SI {
  std::size_t sz = 0;
  SI(const std::size_t sz) : sz(sz) {}
  SI(const int sz) : sz(static_cast<std::size_t>(sz)) {}
  SI(const long long sz) : sz(static_cast<std::size_t>(sz)) {}
};

} // namespace etr

#endif
