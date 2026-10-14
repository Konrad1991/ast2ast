#include <Rcpp.h>
#include "../inst/include/etr.hpp"
#include <functional>
#include <string>
using namespace etr;

// every task warns with its own index; the R side checks that all n warnings
// arrive in task order (= serial map order), although the workers run unordered
// [[Rcpp::export]]
void test_pmap_warnings(int n) {
  Array<Double, Buffer<Double>> x(SI{static_cast<std::size_t>(n)});
  for (int i = 0; i < n; i++) x.set(i, Double(static_cast<double>(i)));
  std::function<Double(Double)> f = [](Double v) {
    warn(false, "task " + std::to_string(static_cast<int>(get_val(v))));
    return v * Double(2.0);
  };
  auto res = pmap(f, Double(4.0), x);
  ass<"pmap warning test: wrong length">(res.size() == static_cast<std::size_t>(n));
  for (int i = 0; i < n; i++) {
    ass<"pmap warning test: wrong value">(get_val(res.get(i)) == 2.0 * i);
  }
}
