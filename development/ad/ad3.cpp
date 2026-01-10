#include <iostream>
#include <vector>
#include <memory>
#include <array>
#include <utility>
#include <cmath>
#include <optional>
#define STANDALONE_ETR
#include "../../inst/include/etr_bits/Core/Types.hpp"
using namespace etr;

namespace etr {
struct VariableAD {
  double value;
  double grad = 0.0;
  VariableAD(double val) : value(val) {}
  void backward(double grad_output = 1.0) {
    grad += grad_output;
  }
};
template<typename L, typename R>
struct BinaryAD {
  double value;
  double grad = 0.0;
  Holder<L> l;
  const double l_val;
  Holder<R> r;
  const double r_val;
  // L & L
  BinaryAD(double val, L& l_, R& r_, const double l_val_, const double r_val_)
  : value(val), l(l_), r(r_), l_val(l_val_), r_val(r_val_) {}
  // L & R
  BinaryAD(double val, L& l_, R&& r_, const double l_val_, const double r_val_)
  : value(val), l(l_), r(std::move(r_)), l_val(l_val_), r_val(r_val_) {}
  // R & L
  BinaryAD(double val, L&& l_, R& r_, const double l_val_, const double r_val_)
  : value(val), l(std::move(l_)), r(r_), l_val(l_val_), r_val(r_val_) {}
  // R & R
  BinaryAD(double val, L&& l_, R&& r_, const double l_val_, const double r_val_)
  : value(val), l(std::move(l_)), r(std::move(r_)), l_val(l_val_), r_val(r_val_) {}

  void backward(double grad_output = 1.0) {
    grad += grad_output;
    l.get().backward(grad_output * l_val);
    r.get().backward(grad_output * r_val);
  }
};

template<typename L, typename R>
inline auto operator+(L&&a, R&& b) {
  using l_dec = std::decay_t<L>;
  using r_dec = std::decay_t<R>;
  constexpr bool is_a_r = std::is_rvalue_reference_v<L&&>;
  constexpr bool is_b_r = std::is_rvalue_reference_v<R&&>;
  if constexpr (!is_a_r && !is_b_r) {
    return BinaryAD<l_dec, r_dec>(a.value + b.value, a, b, 1.0, 1.0);
  } else if constexpr(!is_a_r && is_b_r) {
    return BinaryAD<l_dec, R>(a.value + b.value, a, std::move(b), 1.0, 1.0);
  } else if constexpr(is_a_r && !is_b_r) {
    return BinaryAD<L, r_dec>(a.value + b.value, std::move(a), b, 1.0, 1.0);
  } else if constexpr (is_a_r && is_b_r) {
    return BinaryAD<L, R>(a.value + b.value, std::move(a), std::move(b), 1.0, 1.0);
  }
}
template<typename L, typename R>
inline auto operator*(L&&a, R&& b) {
  using l_dec = std::decay_t<L>;
  using r_dec = std::decay_t<R>;
  constexpr bool is_a_r = std::is_rvalue_reference_v<L&&>;
  constexpr bool is_b_r = std::is_rvalue_reference_v<R&&>;
  if constexpr (!is_a_r && !is_b_r) {
    return BinaryAD<l_dec, r_dec>(a.value * b.value, a, b, b.value, a.value);
  } else if constexpr(!is_a_r && is_b_r) {
    return BinaryAD<l_dec, R>(a.value * b.value, a, std::move(b), b.value, a.value);
  } else if constexpr(is_a_r && !is_b_r) {
    return BinaryAD<L, r_dec>(a.value * b.value, std::move(a), b, b.value, a.value);
  } else if constexpr (is_a_r && is_b_r) {
    return BinaryAD<L, R>(a.value * b.value, std::move(a), std::move(b), b.value, a.value);
  }
}
}

int main() {
  VariableAD x = 2.0;
  VariableAD y = 3.0;
  auto res1 = y*y;
  res1.backward();
  std::cout << "df/dx = " << x.grad << "\n"; // 0
  std::cout << "df/dy = " << y.grad << "\n"; // 6
  auto res2 = x*x;
  res2.backward();
  std::cout << "df/dx = " << x.grad << "\n"; // 4
  std::cout << "df/dy = " << y.grad << "\n"; // 6
  auto res3 = x + y + x;
  res3.backward();
  std::cout << "df/dx = " << x.grad << "\n"; // 6
  std::cout << "df/dy = " << y.grad << "\n"; // 7
  auto res4 = x * VariableAD(5.0);
  res4.backward();
  std::cout << "df/dx = " << x.grad << "\n"; // 11
  std::cout << "df/dy = " << y.grad << "\n"; // 7
}
