#define STANDALONE_ETR

#include "../inst/include/etr.hpp"

int main() {
  std::vector<double> owner = {1, 2, 3};
  etr::Array<etr::Double, etr::Borrow<etr::Double>> a(
    owner.data(), owner.size(), std::vector<std::size_t>{owner.size()}
  );
  std::function<void(etr::Array<etr::Double, etr::Borrow<etr::Double>> g)>foo;
  foo = [&](etr::Array<etr::Double, etr::Borrow<etr::Double>> g) -> void {
    g = etr::c(etr::Double(1.1), etr::Double(2.2), etr::Double(3.3));
    return(etr::Evaluate());
  };
  foo(a);
  etr::print(a);
}
