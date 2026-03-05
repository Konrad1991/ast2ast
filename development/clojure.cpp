#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

int main() {
  std::vector<double> owner{1, 2, 3};
  Array<Double, Borrow<Double>> b(owner.data(), owner.size(), std::vector<std::size_t>{3});
  Array<Double, Buffer<Double>> a = c(Double(1), Double(2), Double(3));

  auto l = [](auto& a, auto& b) -> void {
    print(a + b);
  };

  l(a, b);
  l(b, a);

}
