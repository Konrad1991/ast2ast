#define STANDALONE_ETR
#include "../inst/include/etr_bits/Allocation.hpp"
#include "../inst/include/etr_bits/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/BufferVector.hpp"
#include "../inst/include/etr_bits/Coca.hpp"
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/Printing.hpp"
#include "../inst/include/etr_bits/Subsetting/LazySubsetting.hpp"
#include "../inst/include/etr_bits/UnaryCalculations.hpp"
using namespace etr;

int main() {
  Vec<double> a;
  a = coca(1, 2, 3, 4);
  std::cout << "Start" << std::endl;
  // print(a);
  // sub[0] = 3.14;
  // print(a);
  // subset_test(a, 2) = 40;
  // print(a);
  // subset_test(a, true) = 3.1;
  // print(a);
  subset_test(a, coca(1, 3)) = coca(10, 20);
  // print(a);
}
