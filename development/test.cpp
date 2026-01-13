#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"
using namespace etr;

int main() {
  std::vector<double> owner{1, 2, 3, 4, 5};
  std::vector<double> owner_dot(5, 0.0);
  Array<Double, Buffer<Double>> a = colon(Double(1), Double(5));
  Array<Dual, Borrow<Dual>> b(owner.data(), owner_dot.data(), owner.size(), std::vector<std::size_t>{5});

  if (at(a, Integer(1)) > at(a, Integer(2))) {

  }
  // if (at(b, Integer(1)) > at(a, Integer(2))) {
  //
  // }
  at(b, Integer(1)) + at(b, Integer(2));
}
