
#include "all.hpp"

int main() {
  vec a(range(1., 8.));

  print(a);
  print();
  print(subset(a, 0, 4));
  print();
  print(subset(a, 1, 5));
  print();
  a.subset(2, 6) = subset(a, 0,4) + subset(a, 1, 5);
  print(a);

}
