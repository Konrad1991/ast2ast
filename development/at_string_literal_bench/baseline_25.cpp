#define STANDALONE_ETR
#include "../../inst/include/etr_bits/Core.hpp"
#include "../../inst/include/etr_bits/Subsetting.hpp"
using namespace etr;
int main() {
  Array<Double, Buffer<Double>> a(std::size_t(25 + 1));
  for (std::size_t i = 0; i <= (std::size_t)25; i++) a.d.set(i, Double((double)i));
  Double acc(0.0);
  acc = acc + at(a, Integer(1));
  acc = acc + at(a, Integer(2));
  acc = acc + at(a, Integer(3));
  acc = acc + at(a, Integer(4));
  acc = acc + at(a, Integer(5));
  acc = acc + at(a, Integer(6));
  acc = acc + at(a, Integer(7));
  acc = acc + at(a, Integer(8));
  acc = acc + at(a, Integer(9));
  acc = acc + at(a, Integer(10));
  acc = acc + at(a, Integer(11));
  acc = acc + at(a, Integer(12));
  acc = acc + at(a, Integer(13));
  acc = acc + at(a, Integer(14));
  acc = acc + at(a, Integer(15));
  acc = acc + at(a, Integer(16));
  acc = acc + at(a, Integer(17));
  acc = acc + at(a, Integer(18));
  acc = acc + at(a, Integer(19));
  acc = acc + at(a, Integer(20));
  acc = acc + at(a, Integer(21));
  acc = acc + at(a, Integer(22));
  acc = acc + at(a, Integer(23));
  acc = acc + at(a, Integer(24));
  acc = acc + at(a, Integer(25));
  std::cout << get_val(acc) << std::endl;
}
