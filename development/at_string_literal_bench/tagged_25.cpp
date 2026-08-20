#define STANDALONE_ETR
#include "../../inst/include/etr_bits/Core.hpp"
#include "../../inst/include/etr_bits/Subsetting.hpp"
#include "at_tagged.hpp"
using namespace etr;
int main() {
  Array<Double, Buffer<Double>> a(std::size_t(25 + 1));
  for (std::size_t i = 0; i <= (std::size_t)25; i++) a.d.set(i, Double((double)i));
  Double acc(0.0);
  acc = acc + at_tagged<"a[1]">(a, Integer(1));
  acc = acc + at_tagged<"a[2]">(a, Integer(2));
  acc = acc + at_tagged<"a[3]">(a, Integer(3));
  acc = acc + at_tagged<"a[4]">(a, Integer(4));
  acc = acc + at_tagged<"a[5]">(a, Integer(5));
  acc = acc + at_tagged<"a[6]">(a, Integer(6));
  acc = acc + at_tagged<"a[7]">(a, Integer(7));
  acc = acc + at_tagged<"a[8]">(a, Integer(8));
  acc = acc + at_tagged<"a[9]">(a, Integer(9));
  acc = acc + at_tagged<"a[10]">(a, Integer(10));
  acc = acc + at_tagged<"a[11]">(a, Integer(11));
  acc = acc + at_tagged<"a[12]">(a, Integer(12));
  acc = acc + at_tagged<"a[13]">(a, Integer(13));
  acc = acc + at_tagged<"a[14]">(a, Integer(14));
  acc = acc + at_tagged<"a[15]">(a, Integer(15));
  acc = acc + at_tagged<"a[16]">(a, Integer(16));
  acc = acc + at_tagged<"a[17]">(a, Integer(17));
  acc = acc + at_tagged<"a[18]">(a, Integer(18));
  acc = acc + at_tagged<"a[19]">(a, Integer(19));
  acc = acc + at_tagged<"a[20]">(a, Integer(20));
  acc = acc + at_tagged<"a[21]">(a, Integer(21));
  acc = acc + at_tagged<"a[22]">(a, Integer(22));
  acc = acc + at_tagged<"a[23]">(a, Integer(23));
  acc = acc + at_tagged<"a[24]">(a, Integer(24));
  acc = acc + at_tagged<"a[25]">(a, Integer(25));
  std::cout << get_val(acc) << std::endl;
}
