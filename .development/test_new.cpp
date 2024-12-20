/*
g++ test_new.cpp -std=c++20 -g -Wall -Wpedantic -Wextra -Wconversion
-Wconversion -Wunitialized -Wshadow -Wmissing-declarations
-Wimplicit-fallthrough
-Wduplicated-cond
-Wsign-compare
-Wformat
-Wnull-dereference
-Wdeprecated
-Wstrict-aliasing
*/
#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/BufferVector.hpp"
#include "../inst/include/etr_bits/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/UnaryCalculations.hpp"
using namespace etr;

int main() {
  Buffer<double> v(10);
  std::cout << v << std::endl;

  double* ptr = new double[5];
  for (int i = 0; i < 5; i++) ptr[i] = 0.0;
  Borrow<double> b(ptr, 5);
  std::cout << b << std::endl;

  Vec<double> vec(SI{10});
  std::cout << vec.size() << std::endl;
  for (auto& i: vec) i = 3.14;
  vec = -(vec + vec);
  std::cout << vec << std::endl;


  vec[0] = 10;

  delete[] ptr;
}
