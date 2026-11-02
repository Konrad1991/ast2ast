// CRAN r-oldrel-macos-x86_64: bus error in Dim::release(), test_range_type.R:55
//
// g++     -std=c++20 -O1 -g -fno-omit-frame-pointer -fsanitize=address,undefined development/repro_range_bus_error.cpp -o /tmp/repro && /tmp/repro
// clang++ -std=c++20 -O2 -g -fno-omit-frame-pointer -fsanitize=address,undefined development/repro_range_bus_error.cpp -o /tmp/repro && /tmp/repro
#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include <cstdio>

int f(int n_) {
  etr::Integer n(n_);
  etr::IntRange i;
  etr::Integer s;
  i = etr::colon(etr::Integer(1), n);
  s = etr::Integer(0);
  for (const auto r : etr::colon(etr::Integer(1), etr::Integer(3))) {
    for (const auto v : i) {
      s = s + v;
    }
    i = i + etr::Integer(1);
  }
  return etr::get_val(s);
}

int main() {
  const int res = f(4);
  std::printf("result: %d (expected 42)\n", res);
  return res == 42 ? 0 : 1;
}
