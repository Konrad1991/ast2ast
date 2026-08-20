#define STANDALONE_ETR
#include "../../inst/include/etr_bits/Core.hpp"
#include "../../inst/include/etr_bits/Subsetting.hpp"
#include "at_tagged.hpp"
using namespace etr;
int main() {
  Array<Double, Buffer<Double>> a(std::size_t(5 + 1));
  for (std::size_t i = 0; i <= (std::size_t)5; i++) a.d.set(i, Double((double)i));
  Double acc(0.0);
  acc = acc + at_tagged<"a[1]">(a, Integer(1));
  acc = acc + at_tagged<"a[2]">(a, Integer(2));
  acc = acc + at_tagged<"a[3]">(a, Integer(3));
  acc = acc + at_tagged<"a[4]">(a, Integer(4));
  acc = acc + at_tagged<"a[5]">(a, Integer(5));
  std::cout << get_val(acc) << std::endl;
}
