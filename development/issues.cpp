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
  etr::TAPE_INTERN.clear();
  Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> A;
  Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> C;
  A = matrix(
    c(ReverseDouble(1), ReverseDouble(2), ReverseDouble(3), ReverseDouble(4), ReverseDouble(5), ReverseDouble(6)),
    Integer(2), Integer(3)
  );
  {
    C = mat_mul(
      A,
      matrix(
        c(Double(1), Double(2), Double(3), Double(4), Double(5), Double(6)),
        Integer(3), Integer(2)
      ) + Double(0.0)
    );
  }
  print(C);
}
