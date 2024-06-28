#define DERIV_ETR
#define STANDALONE_ETR
#include "etr.hpp"
using namespace etr;

int main() {
  Vec<double> a;
  Vec<double> b;
  Vec<double> c;
  Vec<double> d;
  Vec<double> dydotdy;
  Vec<double> y;
  Vec<double> ydot;
  Vec<double> parameter;

  set_indep(y);

  y = coca(1.0, 2.0, 3.0, 4.0);
  parameter = coca(1.0, 2.0, 3.0, 4.0);
  ydot = vector_numeric(4);
  etr::assign_deriv(a, etr::get_deriv(etr::at(parameter, 1)));
  a = at(parameter, 1);
  b = at(parameter, 2);
  c = at(parameter, 3);
  d = at(parameter, 4);

  assign_deriv(ydot(1), get_deriv(y(1)) * a + y(1) * get_deriv(a));
  ydot(1) = y(1) * a;

  // assign_deriv(ydot, get_deriv(y) * parameter + y * get_deriv(parameter));
  // ydot = y * parameter;
  dydotdy = get_deriv(ydot);
  print(dydotdy);
}
