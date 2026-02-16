#define STANDALONE_ETR
#include <string>
#include <iostream>
#include <vector>
#include "stddef.h"
#include <cxxabi.h>
#include "../inst/include/etr_bits/Core/Reflection.hpp"
#include "../inst/include/etr.hpp"
using namespace etr;


int main() {
  std::vector<double> owner{1, 2, 3};
  Array<Double, Borrow<Double>> b(owner.data(), owner.size(), std::vector<std::size_t>{3});
  b = b + at(b, Integer(1));
  print(b);
  auto test = seq_len(Integer(1));
  print(test);
  for (Integer i = 1; i < length(b); i = i + Integer(1)) {
    print(i);
  }
}
