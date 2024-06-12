
#define STANDALONE_ETR
#include "./inst/include/etr.hpp"

using namespace etr;

auto leibnizNr1(int rounds) {
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

auto leibnizNr2(int rounds) {
  double x = 1.0;
  double pi = 1.0;

  for (auto &i : colon(2, rounds + 1)) {
    x = x * -1;
    pi = pi + x / (etr::i2d(2) * i - 1);
  }
  pi = pi * 4;
  return pi;
}

int main(int argc, char *argv[]) {
  double ret = leibnizNr1(100000000);
  std::cout << ret << std::endl;

  return 0;
}
