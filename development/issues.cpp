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
  Array<Double, Buffer<Double, LBufferTrait>> a = c(Double(3.4));
  auto b = c(a, Integer(3));
  print(b);

}
