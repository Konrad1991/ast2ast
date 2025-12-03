#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/DataStructures/ArrayClass.hpp"
#include "../inst/include/etr_bits/Calculations/UnaryCalculations.hpp"
#include "../inst/include/etr_bits/Calculations/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/Subsetting/PreservingSubsetting.hpp"
#include "../inst/include/etr_bits/Utilities/Printing.hpp"

using namespace etr;

int main() {
  using Ad = Array<double, Buffer<double, LBufferTrait>>;
  using Ai = Array<int, Buffer<int, LBufferTrait>>;

  const std::size_t n = 24;
  Ad a(SI{n});
  for (std::size_t i = 0; i < n; i++) {
    a[i] = static_cast<double>(i);
  }
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

}
