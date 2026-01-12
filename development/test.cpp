#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"

auto f1() {
  etr::Array<etr::Double, etr::Buffer<etr::Double>> a = etr::c(
    etr::Double(3), etr::Double(4), etr::Double(1)
  );
  etr::Integer size;
  etr::Array<etr::Double, etr::Buffer<etr::Double>> temp;
  size = etr::length(a);
  for(const auto& i : etr::colon(etr::Integer(1), size)) {
    for(const auto& j : etr::colon(etr::Integer(1), (size - etr::Integer(1)))) {
      if (etr::subset(a, j) > etr::subset(a, j + etr::Integer(1))) {
        temp = etr::subset(a, j);
        etr::subset(a, j) = etr::subset(a, j + etr::Integer(1));
        etr::subset(a, j + etr::Integer(1)) = temp;
      }
    }
  }
  return(a);
}

int main() {
  const auto res = f1();
  etr::print(res);
}
