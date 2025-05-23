#include <optional>
#include <stdexcept>
#include <type_traits>
#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

template<typename T>
requires IsSubsetVec<Decayed<T>>
void test(const T& inp) {

}

int main() {

  Vec<double> v1;
  Vec<double> v2;

  v1 = c(1, 2, 3, 4);
  v2 = rep(v1 + 1, 2);
  print(v2);

  v1 = numeric(2);
  print(v1);

  Mat<int> m;
  m = matrix(colon(1, 10), 2, 5);
  print(m);

  test(subset(v1, 1));
}
