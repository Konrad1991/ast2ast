#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/DataStructures/ArrayClass.hpp"
#include "../inst/include/etr_bits/Calculations/UnaryCalculations.hpp"
#include "../inst/include/etr_bits/Calculations/BinaryCalculations.hpp"

using namespace etr;

int main() {
  using A = Array<double, Buffer<double, LBufferTrait>>;
  A a(SI{3});
  a[0] = 3.14;
  a[1] = 6.14;
  a[2] = 3.14/2.0;
  A a_sin;
  a_sin = sinus(a);

  std::cout << a_sin.size() << std::endl;
  std::cout << a_sin[0] << std::endl;
  std::cout << a_sin[1] << std::endl;
  std::cout << a_sin[2] << std::endl;

  A a_plus;
  A a_non_conformable;
  a_plus = a_sin + a_sin;
  std::cout << a_plus.size() << std::endl;
  std::cout << a_plus[0] << std::endl;
  std::cout << a_plus[1] << std::endl;
  std::cout << a_plus[2] << std::endl;

}
