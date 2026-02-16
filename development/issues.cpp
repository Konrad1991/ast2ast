#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"
using namespace etr;


int main() {
  Array<Double, Buffer<Double, RBufferTrait>> a = c(Double(3.4));
  auto b = c(Double(3.12), a, Integer(3));
  print(b);

}
