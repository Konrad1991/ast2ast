#define STANDALONE_ETR


#include "etr.hpp"

using namespace etr;

int main() {
   Vec<double> a = coca(1, 2, 3);
   Vec<int> b = coca(1, 2, 3);
   print(a);
}
