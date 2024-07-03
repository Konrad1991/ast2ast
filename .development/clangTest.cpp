#define STANDALONE_ETR
#define DERIV_ETR
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

  parameter = coca(1, 2, 3, 4);
  ydot = vector_numeric(4);
  y = coca(1, 2, 3, 4);

  set_indep(y);
  print(at(parameter, 1));
  assign_deriv(a, get_deriv(at(parameter, 1)));
  a = at(parameter, 1);
  assign_deriv(b, get_deriv(at(parameter, 2)));
  b = at(parameter, 2);
  assign_deriv(c, get_deriv(at(parameter, 3)));
  c = at(parameter, 3);
  assign_deriv(d, get_deriv(at(parameter, 4)));
  d = at(parameter, 4);
  assign_deriv(at(ydot, 1), get_deriv(at(y, 1)) * a + at(y, 1) * get_deriv(a));
  subset(ydot, 1) = at(y, 1) * a;
  assign_deriv(at(ydot, 2), get_deriv(at(y, 2)) * b + at(y, 2) * get_deriv(b));
  subset(ydot, 2) = at(y, 2) * b;
  assign_deriv(at(ydot, 3), get_deriv(at(y, 3)) * c + at(y, 3) * get_deriv(c));
  subset(ydot, 3) = at(y, 3) * c;
  assign_deriv(at(ydot, 4), get_deriv(at(y, 4)) * d + at(y, 4) * get_deriv(d));
  subset(ydot, 4) = at(y, 4) * d;
  dydotdy = get_deriv(ydot);
}
