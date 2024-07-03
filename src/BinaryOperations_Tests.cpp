#include "etr.hpp"
#include <stdexcept>
#include <type_traits>
using namespace etr;

// [[Rcpp::export]]
void test_arithmetic() {
  // NOTE: test arithmetic +, -, * and /
  {
    std::string s = "ArithmeticTests: ";
    Vec<double> a = coca(1, 2, 3);
    Vec<double> b = coca(4, 5, 6);
    Vec<double> c;
    c = a + b;
    print(c);
    print(a - b);
    print(a * b);
    print(a / b);
    print(power(a, b));
  }
}

// [[Rcpp::export]]
void test_comparison() {
  // NOTE: test comparisons
  {
    std::string s = "ComparisonTests: ";
    Vec<double> a = coca(1, 2, 3);
    Vec<double> b = coca(4, 5, 6);
    print(a == b); // TODO: printing is wrong result is handled as double
    print(a != b);
    print(a >= b);
    print(a <= b);
    print(a < b);
    print(a > b);
    print(coca(1, 2, 3, 9) > 5);
  }
}
