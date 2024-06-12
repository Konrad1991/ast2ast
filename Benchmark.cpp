
#define STANDALONE_ETR
#include "./inst/include/etr.hpp"

using namespace etr;

auto leibniz(int rounds) {
  double x = 1.0;
  double pi = 1.0;
  Vec<double> v = vector_numeric(rounds + 1);
  double start = 2.0;
  for (auto &i : v) {
    i = start;
    start = start + 1.0;
  }
  for (auto &i : v) {
    x = x * -1;
    pi = pi + x / (etr::i2d(2) * i - 1);
  }
  pi = pi * 4;
  return pi;
}

int main(int argc, char *argv[]) {
  double ret = leibniz(10000000);
  std::cout << ret << std::endl;
  return 0;
}
