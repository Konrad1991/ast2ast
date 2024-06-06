#include <stdexcept>
#include <type_traits>
#define STANDALONE_ETR
#include "../include/etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_arithmetic_unary() {
  // NOTE: test unary minus trigo etc.
  {
    std::string s = "ArithmeticTests: ";
    Vec<double> a = coca(1, 2, 3);
    Vec<double> b = coca(4, 5, 6);
    Vec<double> c;
    ass(sinus(a) == coca(0.8414710, 0.9092974, 0.1411200), s + "sinus");
    ass(asinus(a) == coca(1.570796, NA, NA), s + "asinus");
    ass(sinush(a) == coca(1.175201, 3.626860, 10.017875), s + "sinush");
    ass(cosinus(a) == coca(0.5403023, -0.4161468, -0.9899925), s + "cosinus");
    ass(acosinus(a) == coca(0, NA, NA), s + "acosinus");
    ass(cosinush(a) == coca(1.543081, 3.762196, 10.067662), s + "cosinush");
    ass(tangens(a) == coca(1.5574077, -2.1850399, -0.1425465), s + "tangens");
    ass(atangens(a) == coca(0.7853982, 1.1071487, 1.2490458), s + "atangens");
    ass(tangensh(a) == coca(0.7615942, 0.9640276, 0.9950548), s + "tangensh");
    ass(ln(a) == coca(0.0000000, 0.6931472, 1.0986123), s + "ln");
    ass(sqroot(a) == coca(1.000000, 1.414214, 1.732051), s + "sqrt");
    ass(exp(a) == coca(2.718282, 7.389056, 20.085537), s + "exp");
  }
}
