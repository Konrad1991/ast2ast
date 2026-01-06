#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/Calculations/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/Calculations/UnaryCalculations.hpp"
#include "../inst/include/etr_bits/Subsetting.hpp"
#include "../inst/include/etr_bits/Allocation.hpp"
#include "../inst/include/etr_bits/Utilities/Printing.hpp"
using namespace etr;

int main() {
  Array<Double, Buffer<Double>> a;

  a = c(Double(1.1), Double(2.2), Double(3.3));
  print(a);
}
