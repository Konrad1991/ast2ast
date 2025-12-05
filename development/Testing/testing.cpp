#define STANDALONE_ETR
#include "../../inst/include/etr_bits/Core.hpp"
#include "../../inst/include/etr_bits/DataStructures/ArrayClass.hpp"
#include "../../inst/include/etr_bits/Calculations/UnaryCalculations.hpp"
#include "../../inst/include/etr_bits/Calculations/BinaryCalculations.hpp"
#include "../../inst/include/etr_bits/Subsetting/PreservingSubsetting.hpp"
#include "../../inst/include/etr_bits/Utilities/Printing.hpp"
#include "../../inst/include/etr_bits/Allocation/Coca.hpp"

using namespace etr;

int main() {
  using Ad = Array<double, Buffer<double, LBufferTrait>>;
  using Ai = Array<int, Buffer<int, LBufferTrait>>;

  const std::size_t n = 24;
  Ad a;
  a = c(1.0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24);
  a.dim.resize(3);
  a.dim[0] = 2;
  a.dim[1] = 3;
  a.dim[2] = 4;
  print(a);

  Ai cols(SI{2}); cols[0] = 2; cols[1] = 1;
  Ai d3(SI{3}); d3[0] = 2; d3[1] = 1; d3[2] = 4;

  print(
    subset(a, true, cols, d3)
  );

  subset(a, true, cols, d3) = 3.14;
  print(a);

  Double d = 3.14;
  auto ds = sinus(d);
  Logical l = true;
  // sinus(l);
  auto bla = sinus(3.14);
  printT<decltype(bla)>();
}
