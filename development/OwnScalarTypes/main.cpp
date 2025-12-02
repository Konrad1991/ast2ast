#include <iostream>
#include <type_traits>

#include "stddef.h"
#include <cxxabi.h>

inline std::string demangle(const char *mangledName) {
  int status;
  char *demangled = abi::__cxa_demangle(mangledName, nullptr, nullptr, &status);
  std::string result(demangled);
  std::free(demangled);
  return result;
}

template <typename T> void inline printT() {
  std:: cout << demangle(typeid(T).name()) << std::endl;
}

// Scalar types
// --------------------------------------------------------------------------------------------------
struct Bool {
  bool val;
  Bool(bool v) : val(v) {}
};
struct Int {
  int val;
  Int(int v) : val(v) {}
};
struct Double {
  double val;
  Double(double v) : val(v) {}
};

// Common type of scalar types
// --------------------------------------------------------------------------------------------------
template<typename T>
using bare_t = std::remove_cv_t<std::remove_reference_t<T>>;

// Primary template
template<typename L, typename R>
struct common_type_internal {
  static_assert(!std::is_same_v<L,L>,
                "No common_type_internal< L, R > specialization provided.");
};

// 1. Same type
template<typename T>
struct common_type_internal<T, T> { using type = T; };

// 2. Bool with anything --> the other type
template<typename R>
struct common_type_internal<Bool, R> { using type = R; };

template<typename L>
struct common_type_internal<L, Bool> { using type = L; };

// 3. Int & Double --> Double
template<>
struct common_type_internal<Int, Double> { using type = Double; };

template<>
struct common_type_internal<Double, Int> { using type = Double; };

// Convenience alias
template<typename L, typename R>
struct common_type {
  using type = typename common_type_internal<bare_t<L>, bare_t<R>>::type;
};

template<typename L, typename R>
using common_type_t = typename common_type<L, R>::type;

// Concept to detect scalars
// --------------------------------------------------------------------------------------------------
template<class T>
concept ScalarType =
    std::same_as<T, Bool> ||
    std::same_as<T, Int> ||
    std::same_as<T, Double>;

int main () {
  Bool b(true);
  Int i(19);
  common_type_t<decltype(b), decltype(i)> c;
  printT<decltype(c)>();
}
