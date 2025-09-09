#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

const double pi = 3.1515;

void test_unary_scalar() {
  sinus(pi);
  int a = -1;
  Vec<double> t = -sinus(pi);
  print(t);
  try {
    sinus(1);
  } catch (const std::exception &e) {
    std::cout << e.what() << std::endl;
  }
}

int main() {
 test_unary_scalar();
}
