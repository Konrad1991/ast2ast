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
  etr::Array<etr::Double, etr::Buffer<etr::Double>> a =
    etr::c(etr::Double(1), etr::Double(100), etr::Double(0.2), etr::Double(10));
  print(a);
  double* val = a.d.data();
  const std::size_t n = a.d.size();
  std::sort(val, val + n);
  print(a);
}

