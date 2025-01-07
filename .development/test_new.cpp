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
#include "../inst/include/etr_bits/Coca.hpp"
#include "../inst/include/etr_bits/Printing.hpp"
using namespace etr;


int main() {
  Vec<double> a;
  Vec<double> b;
  Vec<double> c;
  Vec<double> d;

  a = coca(1.1,2,3);
  b = coca(1.1, 5, 6);
  c = a + b + a + 3.14;
  d = -c;
  print(c);
  print(d);

  for (auto i: d.d) std::cout << i << std::endl;
}
