#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// uniroot()'s C++ signature always took Integer maxit; the R-level type
// check used to require a double literal (100.0), forcing an unnecessary
// implicit Double -> Integer conversion. It now accepts both natively.

static bool close(double l, double r) { return std::abs(l - r) < 1e-9; }

// [[Rcpp::export]]
void test_uniroot_maxiter() {
  Array<Double, Buffer<Double>> interval;
  interval = c(Double(0.0), Double(10.0));
  std::function<Double(Double)> g = [](Double x) { return x * x - Double(4.0); };

  // ---- maxiter passed as Integer (matches the native C++ signature) ------
  {
    auto res = uniroot(g, interval, Double(1e-10), Integer(100));
    ass<"uniroot int maxiter root">(close(get_val(res.root), 2.0));
  }
  // ---- maxiter passed as Double (implicit conversion to Integer) ---------
  {
    auto res = uniroot(g, interval, Double(1e-10), Double(100.0));
    ass<"uniroot double maxiter root">(close(get_val(res.root), 2.0));
  }
}
