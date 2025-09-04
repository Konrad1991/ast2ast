#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

int main() {
  Vec<double> v = colon(1, 16);
  print(subset(v, c(1, 2, 3)));
}
