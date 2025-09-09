#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

int main() {
  Vec<double> v = c(1.1, 2.2, 3.3);
  double buf[3] = {1, 2, 3};
  Vec<double, Borrow<double>> b(buf, 3);

  // Print buffer
  for (const auto &i: v) {
    print(i);
  }
  // Print borrow
  for (const auto &i: b) {
    print(i);
  }
  // Print R Buffer
  for (const auto& i: c(10.1, 10.2, 10.3)) {
    print(i);
  }
  // Print unary calculation on l value
  const double pi = 3.141592;
  Vec<double> temp = c(0, pi/2, pi, 2*pi);
  for (const auto&i: sinus(temp)) {
    print(i);
  }
  // Print unary calculation on r value
  for (const auto&i: sinus(c(0, pi/2, pi, 2*pi))) {
    print(i);
  }

  v = v + c(1.1, 2.2, 3.3);
  print(v);
}
