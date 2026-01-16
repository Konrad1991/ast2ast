#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"
using namespace etr;

int main() {
  Array<Double, Buffer<Double>> a(SI{3});
  a.set(0, 1.1);
  a.set(1, 2.2);
  a.set(2, 3.3);
  Integer i1 = 1;
  subset(a, i1) = Double(3.14);
  auto sub = subset(a, i1);
  print(sub);
  // ass<"Subset modify Array">(compare(get_val(sub), 3.14));
}
