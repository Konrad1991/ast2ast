#include "etr.hpp"
#include <stdexcept>
#include <type_traits>
using namespace etr;

// [[Rcpp::export]]
void test_arithmetic() {
  // NOTE: test arithmetic +, -, * and /
  {
    std::string s; s = "ArithmeticTests: ";
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> b; b = c(4, 5, 6);
    Vec<double> c;
    c = a + b;
    ass<"5">(c[0] == 5);
    ass<"7">(c[1] == 7);
    ass<"9">(c[2] == 9);
    c = a - b;
    ass<"-3">(c[0] == -3);
    ass<"-3">(c[1] == -3);
    ass<"-3">(c[2] == -3);
    c = a * b;
    ass<"4">(c[0] == 4);
    ass<"10">(c[1] == 10);
    ass<"18">(c[2] == 18);
    c = a / b;
    ass<"1/4">(c[0] == 0.25);
    ass<"2/5">(c[1] == 0.4);
    ass<"1/2">(c[2] == 0.5);
    c = power(a, b);
    ass<"1">(c[0] == 1);
    ass<"2^5">(c[1] == 32);
    ass<"3^6">(c[2] == 729);
  }
}

// [[Rcpp::export]]
void test_comparison() {
  // NOTE: test comparisons
  {
    std::string s = "ComparisonTests: ";
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> b; b = c(1, 5, 2);
    Vec<bool> c; c = a == b;
    ass<"1 == 1">(c[0]);
    ass<"2 == 5">(!c[1]);
    ass<"3 == 2">(!c[2]);
    c = a != b;
    ass<"1 != 1">(!c[0]);
    ass<"2 != 5">(c[1]);
    ass<"3 != 2">(c[2]);
    c = a >= b;
    ass<"1 >= 1">(c[0]);
    ass<"2 >= 5">(!c[1]);
    ass<"3 >= 2">(c[2]);
    c = a <= b;
    ass<"1 <= 1">(c[0]);
    ass<"2 <= 5">(c[1]);
    ass<"3 <= 2">(!c[2]);
    c = a < b;
    ass<"1 < 1">(!c[0]);
    ass<"2 < 5">(c[1]);
    ass<"3 < 2">(!c[2]);
    c = a > b;
    ass<"1 > 1">(!c[0]);
    ass<"2 > 5">(!c[1]);
    ass<"3 > 2">(c[2]);

    c = etr::c(1, 2, 3, 8) > 5;
    ass<"1 > 5">(!c[0]);
    ass<"2 > 5">(!c[1]);
    ass<"3 > 5">(!c[2]);
    ass<"8 > 5">(c[3]);
  }
}
