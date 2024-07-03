#define DERIV_ETR
#define STANDALONE_ETR
#include "etr.hpp"
using namespace etr;

int main() {
  Vec<double> jac;
  Vec<double> y;
  Vec<double> x;
  y = vector_numeric(2);
  x = coca(1, 2);
  jac = matrix(i2d(0), length(y), length(x));
  for(auto i: colon(1, length(x))) {
    set_indep(subset(x, i));
    assign(at(y, 1),

           at(x, 1) ^ 2 * at(x, 2),

           i2d(2) * at(x, 1) * get_deriv(at(x, 1)) * at(x, 2) + 
           at(x, 1) ^2 * get_deriv(at(x, 2)));

    assign(at(y, 2), 

           i2d(5) * at(x, 1) + sinus(at(x, 2)),

           i2d(5) * get_deriv(at(x, 1)) +
           get_deriv(at(x, 2)) * cosinus(at(x, 2)));

    unset_indep(subset(x, i));
    subset(jac, true, i) = get_deriv(y);
  }
  print(jac);
}
