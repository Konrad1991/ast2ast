#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/DataStructures/ArrayClass.hpp"
#include "../inst/include/etr_bits/Calculations.hpp"
#include "../inst/include/etr_bits/Subsetting/PreservingSubsetting.hpp"
#include "../inst/include/etr_bits/Utilities/Printing.hpp"
using namespace etr;

int main() {
  Double d = 3.14;
  Logical l = true;
  Integer i = 40;

  Double res = d + d;

  Array<Double, Buffer<Double>> v1(SI{3});
  v1[0] = 3.14;
  v1[1] = 1.2;
  v1[2] = 2.3;
  Array<Integer, Buffer<Integer>> v2(SI{4});
  v2[0] = 3;
  v2[1] = 1;
  v2[2] = 2;
  v2[3] = 1;

  print(v1);
  auto bla = subset(v1, v2);
  print(bla);

  auto bla2 = subset(v1, true);
  print(bla2);

  auto bla3 = subset(v1, 3);
  print(bla3);
}
