#include <iostream>
#include <tuple>
#include <memory>
#include <type_traits>
#define STANDALONE_ETR
#include "../../inst/include/etr_bits/Core/Types.hpp"
#include "../../inst/include/etr_bits/Core/Reflection.hpp"
#include "../../inst/include/etr_bits/Core/ReverseAutomaticDifferentiation.hpp"
#include "../../inst/include/etr_bits/Core/Buffer.hpp"
#include "../../inst/include/etr_bits/Core/Borrow.hpp"
#include "../../inst/include/etr_bits/Core/ArrayClass.hpp"

using namespace etr;

var f(var x, var y, var z) {
  return x + y + z*x + x* var(etr::Integer(3));
}

int main() {

  var x = etr::Double(1.1);
  var y = etr::Double(2.2);
  var z = etr::Double(3.3);
  var u = f(x, y, z);
  std::cout << u << std::endl;

  auto res = derivatives(u, wrt(x, y, z));
  for (auto i: res) std::cout << i << std::endl;

  var test = asin(x);
  std::cout << test << std::endl;
  auto d_test = derivativesx(test, wrt(x, y));
  for (auto i: d_test) std::cout << i << std::endl;
  std::cout << x.expr -> val << std::endl;

  if (x == y) {
    std::cout << 1 << std::endl;
  } else if (x == x) {
    std::cout << 2 << std::endl;
  }
  var res2 = x * var(etr::Integer(3));
  auto d_res2 = derivatives(res2, wrt(x));
  std::cout << res2 << std::endl;
  std::cout << d_res2[0] << std::endl;

  std::cout << get_val(x == var(etr::Double(1.1))) << std::endl;
  std::cout << get_val(x) << std::endl;
  std::cout << get_val(x + y) << std::endl;

  auto bla = x + etr::Integer(3);
  auto bla2 = etr::Integer(3) + etr::Logical(true);
  std::cout << bla << std::endl;
  std::cout << bla2 << std::endl;
  etr::derivatives(var(bla), wrt(x));

  x == etr::Integer(3);

  {
    var a(Double(3.0));
    var b(Double(4.0));
    auto test = a || b;
    // auto test = Variable<Double>(Double(3.14)) || Variable<Double>(Double(3.14));
    printTAST<decltype(std::declval<Variable<Double>>() || std::declval<Variable<Double>>())>();
    // static_assert(IS<decltype(std::declval<Variable<Double>>() || std::declval<Variable<Double>>()), Logical>, "std::declval<Variable<Double>>() || std::declval<Variable<Double>>() -> Logical");
  }

}

