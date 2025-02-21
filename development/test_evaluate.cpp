#define STANDALONE_ETR
// #define DERIV_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

int main() {
  Vec<double> v = coca(1, 2, 3, 4);
  Evaluate(v);
  Evaluate(coca(1, 2, 3));
  auto res = Evaluate(v + v);
  print(res);
}
