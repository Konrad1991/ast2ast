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
#include "../inst/include/etr_bits/Coca.hpp"
#include "../inst/include/etr_bits/Printing.hpp"
using namespace etr;


int main() {
  Vec<double> a;
  Vec<double> b;
  Vec<double> c;
  Vec<bool> d;

  a = coca(1,2,3);
  b = coca(1, 5, 6);
  c = a + b;
  d = a == b;
  auto e = a == b;
  auto f = a + b;
  for (auto i: f.d) {
    std::cout << "val: " << i << std::endl;
  }

  print(c);
  print(d);
  printTAST<decltype(e)>();
  printTAST<decltype(f)>();
  printTAST<decltype(f.d)>();

  auto test = f.d;

  print(a + c);

}
