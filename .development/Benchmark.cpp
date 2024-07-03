#include <ctime>
#define STANDALONE_ETR
#include "./inst/include/etr.hpp"
#include <time.h>
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
auto leibnizNr3(int rounds) {
  double pi = 1.0;

  for (auto &i : colon(2, rounds + 1)) {
    if ((i % 2) == 0) {
      pi = pi + (-1.0) / (etr::i2d(2) * i - 1);
    } else {
      pi = pi + 1.0 / (etr::i2d(2) * i - 1);
    }
  }
  pi = pi * 4;
  return pi;
}

auto leibnizNr4(int rounds) {
  double pi = 1.0;
  int i = 2;
  while (i < (rounds + 1)) {
    if ((i % 2) == 0) {
      pi = pi + (-1.0) / (etr::i2d(2) * i - 1);
    } else {
      pi = pi + 1.0 / (etr::i2d(2) * i - 1);
    }
    i++;
  }
  pi = pi * 4;
  return pi;
}

auto leibnizNr5(int rounds) {
  double pi = 1.0;
  double factor = 2.0;
  bool isNegative = false;

  for (int i = 2; i <= rounds + 1; ++i) {
    if (isNegative) {
      pi = pi + (1.0) / (factor * i - 1);
    } else {
      pi = pi + (-1.0) / (factor * i - 1);
    }
    isNegative = !isNegative;
  }

  return pi * 4;
}

double leibnizNr6(int rounds) {
  double pi = 1.0;
  double denominator = 3.0;
  double sign = -1.0;

  for (int i = 2; i <= rounds; ++i) {
    pi += sign / denominator;
    denominator += 2.0;
    sign = -sign;
  }

  return pi * 4.0;
}

int main(int argc, char *argv[]) {
  int rounds = 1000000;
  clock_t t;
  t = clock();
  double ret = leibnizNr1(rounds);
  t = clock() - t;
  std::cout << static_cast<float>(t) / CLOCKS_PER_SEC << " sec" << std::endl;
  std::cout << ret << std::endl;

  t = clock();
  ret = leibnizNr2(rounds);
  t = clock() - t;
  std::cout << static_cast<float>(t) / CLOCKS_PER_SEC << " sec" << std::endl;
  std::cout << ret << std::endl;

  t = clock();
  ret = leibnizNr3(rounds);
  t = clock() - t;
  std::cout << static_cast<float>(t) / CLOCKS_PER_SEC << " sec" << std::endl;
  std::cout << ret << std::endl;

  t = clock();
  ret = leibnizNr4(rounds);
  t = clock() - t;
  std::cout << static_cast<float>(t) / CLOCKS_PER_SEC << " sec" << std::endl;
  std::cout << ret << std::endl;

  t = clock();
  ret = leibnizNr5(rounds);
  t = clock() - t;
  std::cout << static_cast<float>(t) / CLOCKS_PER_SEC << " sec" << std::endl;
  std::cout << ret << std::endl;

  t = clock();
  ret = leibnizNr6(rounds);
  t = clock() - t;
  std::cout << static_cast<float>(t) / CLOCKS_PER_SEC << " sec" << std::endl;
  std::cout << ret << std::endl;

  return 0;
}
