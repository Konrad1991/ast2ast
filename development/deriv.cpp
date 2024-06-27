#define STANDALONE_ETR
#define DERIV_ETR
#include "etr.hpp"

using namespace etr;

int main() {
  Vec<double> y;
  y = coca(1, 2, 3);
  Vec<double> x;
  x = coca(1, 2, 3);
  set_indep(x);

  // dy/dx = get_deriv(x) * x + x * get_deriv(x) = 2, 4, 6
  assign_deriv(y, get_deriv(x) * x + x * get_deriv(x));
  // y = x*x = 1, 4, 9
  y = x * x;

  print(get_deriv(x));
  print(get_deriv(y));
  print(y);
  print(x);
  print();

  // dy/dx = get_deriv(y) * x + y * get_deriv(x) =
  //         6  *  3  +  9  *  1 = 27
  assign_deriv(subset(y, 2),
               get_deriv(subset(y, 3)) * x(3) + y(3) * get_deriv(subset(x, 3)));
  // y(2) = y(3) * x(3) + 1.1 = 28.1
  y(2) = x(3) * y(3) + 1.1;

  print(get_deriv(x));
  print(get_deriv(y));
  print(y);
  print(x);
  print();
}
