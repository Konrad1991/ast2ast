
#include <optional>
#include <stdexcept>
#include <type_traits>
#define STANDALONE_ETR
#include "../inst/include/etr_bits/DataStructures.hpp"
#include "../inst/include/etr_bits/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/Printing.hpp"
#include "../inst/include/etr_bits/Subsetting.hpp"
#include "../inst/include/etr_bits/Coca.hpp"
#include "../inst/include/etr_bits/Allocation/AllocationUtils.hpp"
#include "../inst/include/etr_bits/Allocation/Colon.hpp"
using namespace etr;

int main() {

  Vec<double> v1;
  v1 = coca(1, 2, 3, 4);

  print(subset(v1, coca(1.1, 2.2)));
  int i = 1;
  subset(v1, 1) = 3.14;
  print(subset(v1, i));
  print(subset(coca(1, 2, 3), 3));
  subset(coca(1, 2, 3), 2) = 4.5;
  subset(coca(1, 2, 3), i) = 4.5;

  subset(v1, 2.2) = 3.14;
  print(subset(v1, 2.3));

  Vec<double> v2;
  v2 = coca(1, 2);
  subset(v1, v2 + v2) = 700;
  print(v1);

  subset(v1, coca(1, 2, 1.2, 2)) = coca(100, 200, 300, 400);
  print(v1);

  subset(v1, true) = 500;
  print(v1);
  subset(v1, false) = 500;
  print(v1);


  subset(v1, coca(true, false, true, true)) = 900;
  print(v1);


  Mat<double> m(SI{2}, SI{3});
  print(m);
  print(subset(m, coca(1, 2), coca(2)));
  print(subset(m, coca(2), coca(1, 2)));

  std::cout << std::endl;
  print(subset(m, true, 2));
  print(subset(m, coca(1, 2), 2));
  print(subset(m, true, coca(1, 2)));


  std::cout << std::endl;
  print(v1);
  print(at(v1, 1));
  print(at(v1, coca(1)));

  print(at(m, 1, 2));


  Vec<double> test;
  test = colon(coca(1), coca(9));
  print(test);
  test = colon(coca(true), coca(9));

}
