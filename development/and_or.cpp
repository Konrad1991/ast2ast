#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"
using namespace etr;

int main() {
  Array<Double, Buffer<Double>> a = c(Double(0), Double(2));
  Double d = 3.14;
  auto test1 = a && a;
  auto test2 = a & a;
  auto test3 = a && d;
  auto test4 = d && d;
  auto test5 = d & d;
  printTAST<decltype(test1)>();
  printTAST<decltype(test2)>();
  printTAST<decltype(test3)>();
  printTAST<decltype(test4)>();
  printTAST<decltype(test5)>();

  if (at(a, Integer(1)) && at(a, Integer(2))) {
    print("Test");
  } else if (d || d) {
    print("bla");
  }
}
