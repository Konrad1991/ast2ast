#include "etr.hpp"
#include <stdexcept>
#include <type_traits>
using namespace etr;

// [[Rcpp::export]]
void test_arithmetic_unary() {
  // NOTE: test unary minus trigo etc.
  {
    std::string s = "ArithmeticTests: ";
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> b; b = c(4, 5, 6);
    Vec<double> c;
    ass(sinus(a) == etr::c(0.8414710, 0.9092974, 0.1411200), std::string(s) + "sinus");
    ass(asinus(a) == etr::c(1.570796, NA, NA), std::string(s) + "asinus");
    ass(sinush(a) == etr::c(1.175201, 3.626860, 10.017875), std::string(s) + "sinush");
    ass(cosinus(a) == etr::c(0.5403023, -0.4161468, -0.9899925), std::string(s) + "cosinus");
    ass(acosinus(a) == etr::c(0, NA, NA), std::string(s) + "acosinus");
    ass(cosinush(a) == etr::c(1.543081, 3.762196, 10.067662), std::string(s) + "cosinush");
    ass(tangens(a) == etr::c(1.5574077, -2.1850399, -0.1425465), std::string(s) + "tangens");
    ass(atangens(a) == etr::c(0.7853982, 1.1071487, 1.2490458), std::string(s) + "atangens");
    ass(tangensh(a) == etr::c(0.7615942, 0.9640276, 0.9950548), std::string(s) + "tangensh");
    ass(ln(a) == etr::c(0.0000000, 0.6931472, 1.0986123), std::string(s) + "ln");
    ass(sqroot(a) == etr::c(1.000000, 1.414214, 1.732051), std::string(s) + "sqrt");
    ass(exp(a) == etr::c(2.718282, 7.389056, 20.085537), std::string(s) + "exp");
  }
}
