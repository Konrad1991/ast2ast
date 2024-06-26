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
  assign_deriv(y, 
               get_deriv(x) * x + x * get_deriv(x));
  // y = x*x = 1, 4, 9
  y = x*x;
 
  print(get_deriv(x));
  print(get_deriv(y));
  print(y);
  print(x);
  print();

  assign_deriv(subset_deriv(y, 2),
               get_deriv(subset_deriv(x, 3)) * x(3) +
               get_deriv(subset_deriv(x, 3)) * x(3));
  y(2) = x(3) * x(3); 
  // y = x*x = 1, 9, 9
  // dy/dx = get_deriv(x) * x + x * get_deriv(x) = 1, 4, 9
  print(get_deriv(x));
  print(get_deriv(y));
  print(y);
  print(x);
  print();
}

