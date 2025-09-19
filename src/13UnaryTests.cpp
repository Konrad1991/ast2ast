#include "etr.hpp"
#include <stdexcept>
#include <type_traits>
#include <cmath>
using namespace etr;

static inline bool approx(double a, double b, double tol = 1e-12) {
  return std::fabs(a - b) <= tol;
}

// [[Rcpp::export]]
void test_arithmetic_unary() {
  // NOTE: test unary minus trigo etc. l values
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
    ass(etr::expo(a) == etr::c(2.718282, 7.389056, 20.085537), std::string(s) + "exp");
  }
  // NOTE: test unary minus trigo etc. r values
  {
    std::string s = "ArithmeticTests: ";
    ass(etr::sinus(etr::c(1.0, 2.0, 3.0)) == etr::c(0.8414710, 0.9092974, 0.1411200), std::string(s) + "sinus");
    ass(etr::asinus(etr::c(1.0, 2.0, 3.0)) == etr::c(1.570796, NA, NA), std::string(s) + "asinus");
    ass(etr::sinush(etr::c(1.0, 2.0, 3.0)) == etr::c(1.175201, 3.626860, 10.017875), std::string(s) + "sinush");
    ass(etr::cosinus(etr::c(1.0, 2.0, 3.0)) == etr::c(0.5403023, -0.4161468, -0.9899925), std::string(s) + "cosinus");
    ass(etr::acosinus(etr::c(1.0, 2.0, 3.0)) == etr::c(0, NA, NA), std::string(s) + "acosinus");
    ass(etr::cosinush(etr::c(1.0, 2.0, 3.0)) == etr::c(1.543081, 3.762196, 10.067662), std::string(s) + "cosinush");
    ass(etr::tangens(etr::c(1.0, 2.0, 3.0)) == etr::c(1.5574077, -2.1850399, -0.1425465), std::string(s) + "tangens");
    ass(etr::atangens(etr::c(1.0, 2.0, 3.0)) == etr::c(0.7853982, 1.1071487, 1.2490458), std::string(s) + "atangens");
    ass(etr::tangensh(etr::c(1.0, 2.0, 3.0)) == etr::c(0.7615942, 0.9640276, 0.9950548), std::string(s) + "tangensh");
    ass(etr::ln(etr::c(1.0, 2.0, 3.0)) == etr::c(0.0000000, 0.6931472, 1.0986123), std::string(s) + "ln");
    ass(etr::sqroot(etr::c(1.0, 2.0, 3.0)) == etr::c(1.000000, 1.414214, 1.732051), std::string(s) + "sqrt");
    ass(etr::expo(etr::c(1.0, 2.0, 3.0)) == etr::c(2.718282, 7.389056, 20.085537), std::string(s) + "exp");
  }
  // NOTE: Iterations
  {
    Vec<double> a = c(0.0, M_PI/2.0, M_PI);
    double sum = 0.0;
    for (const auto& i: sinus(a)) {
       sum += i;
    }
    ass(approx(sum, 1.0), "Loop over l value");
    sum = 0.0;
    for (const auto& i: cosinus(c(0.0, M_PI /2.0, M_PI) + c(0.0))) {
       sum += i;
    }
    ass(approx(sum, 0.0), "Loop over r value");
  }
  {
    Vec<double> a; a = c(1.0, 2.0, 3.0);
    auto m = -a;
    ass(m == c(-1.0, -2.0, -3.0), "unary minus on lvalue");
    // rvalue chain lifetime through holder + iterator
    double s = 0.0;
    for (auto x : -sinus(c(0.0, M_PI/2.0, M_PI))) s += x;
    ass(approx(s, -1.0), "unary minus over rvalue chain");
  }
  {
    Vec<double> a; a = c(0.0, M_PI/2.0, M_PI);
    const auto u = sinus(a);     // forces begin() const
    double s = 0.0;
    for (auto x : u) s += x;     // uses It<const T>
    ass(approx(s, 1.0), "const iteration over unary adaptor");
  }
  {
    Vec<double> e;               // default empty
    auto u = sinus(e);
    ass(u.size() == 0, "unary over empty has size 0");
    std::size_t n = 0;
    for (auto x : u) { (void)x; ++n; }   // should not deref / segfault
    ass(n == 0, "iteration over empty unary range");
  }
  {
    Vec<int> ai; ai = c(1, 2, 3);
    try {
      auto s = sinus(ai);
    }
    catch (const Rcpp::exception& e) {
      std::string expected = "Error: sin on int vector values";
      ass(e.what() == std::string(expected), "Sinus on int");
    }
  }
  {
    Vec<double> a; a = c(0.25, 0.5, 0.75, 1.0);
    auto u = cosinus(a);
    double s1 = 0.0, s2 = 0.0;
    for (auto x : u) s1 += x;
    for (std::size_t i = 0; i < u.size(); ++i) s2 += u[i];
    ass(approx(s1, s2), "iteration sum equals index sum");
  }
}


