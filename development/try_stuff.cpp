#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"

using namespace etr;

int main() {
  etr::TAPE_INTERN.clear();
  etr::Array<etr::ReverseDouble, etr::Buffer<etr::ReverseDouble>> a;
  a = etr::diag<etr::ReverseDouble>(etr::c(etr::ReverseDouble(1.0), etr::ReverseDouble(2.0)), etr::Integer(2), etr::Integer(3));
  print(a);
}
