#define STANDALONE_ETR
#define DERIV_ETR
#include "etr.hpp"

using namespace etr;

int main() {

  Vec<double> y;
  Vec<double> y1;
  Vec<double> x;
  Vec<double> jac;
  x = coca(2, 5);
  y = coca(1, 2);

  assign(y, vector_numeric(2), 0);  
  assign(jac, matrix(i2d(0), length(y), length(x)), 0);   
  for (auto&i:  colon(i2d(1), length(x))) {    
      set_indep(subset(x, i));      
     assign(at(y, 1),
            power(at(x, 1), 2) + sinus(4),
            i2d(2) * at(x, 1) * get_deriv(at(x, 1)));   
    assign(at(y, 2), 
            at(x, 2) * 7,
            get_deriv(at(x, 2)) * 7);    
     unset_indep(subset(x, i));    
     subset(jac, true, i) = get_deriv(y);  
  }   
  print(jac);
}
