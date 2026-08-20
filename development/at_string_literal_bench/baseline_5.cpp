#define STANDALONE_ETR
#include "../../inst/include/etr_bits/Core.hpp"
#include "../../inst/include/etr_bits/Subsetting.hpp"
using namespace etr;
int main() {
  Array<Double, Buffer<Double>> a(std::size_t(5 + 1));
  for (std::size_t i = 0; i <= (std::size_t)5; i++) a.d.set(i, Double((double)i));
  Double acc(0.0);
  acc = acc + at(a, Integer(1));
  acc = acc + at(a, Integer(2));
  acc = acc + at(a, Integer(3));
  acc = acc + at(a, Integer(4));
  acc = acc + at(a, Integer(5));
  std::cout << get_val(acc) << std::endl;
}
