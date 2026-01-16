#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"
using namespace etr;

int main() {
  std::vector<double> owner{1, 2, 3, 4, 5};
  Array<Double, Borrow<Double, BorrowTrait>> a(owner.data(), owner.size(), std::vector<std::size_t>{owner.size()});
  at(a, Integer(1)) + at(a, Integer(1));
  at(a, Integer(1)) + Variable<Double>(3.14);
  get_val(at(a, Integer(1)));
  at(a, Integer(1)) = at(a, Integer(1)) + at(a, Integer(2));
  at(a, Integer(1)) = Double(1);
  at(a, Integer(1)) = Integer(1);

  Array<Double, Buffer<Double>> b = c(Double(1), Double(2), Double(3));
  b = at(a, Integer(1)) + Double(0);
  print(subset(a, at(a, Integer(1))));
  b  + at(a, Integer(1));
}
