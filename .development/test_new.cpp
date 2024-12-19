#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
using namespace etr;

int main() {
  Buffer<double> v(10);
  std::cout << v << std::endl;

  double* ptr = new double[5];
  for (int i = 0; i < 5; i++) ptr[i] = 0.0;
  Borrow<double> b(ptr, 5);
  std::cout << b << std::endl;

  delete[] ptr;
}
