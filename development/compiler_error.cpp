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
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };
  auto compare_dims = [](const std::vector<std::size_t>& l, const std::vector<std::size_t>& r) {
    if (l.size() != r.size()) return false;
    for (std::size_t i = 0; i < l.size(); i++) {
      if (l[i] != r[i]) return false;
    }
    return true;
  };
  using RealType = ReverseDouble;
  using A = Array<RealType, Buffer<RealType, LBufferTrait>>;
  A a(SI{3});
  a.set(0, 1.1);
  a.set(1, 2.2);
  a.set(2, 3.3);
  Array<Integer, Buffer<Integer, LBufferTrait>> indices(SI{3});
  indices.dim = std::vector<std::size_t>{3};
  indices.set(0, 1);
  indices.set(1, 3);
  indices.set(2, 2);
  auto sub = subset(a, indices);
  printTAST<decltype(sub.get(0))>();
  ass<"Get element 1">(compare(get_val(sub.get(0)), 1.1));
}

