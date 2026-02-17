#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

/*
f:
--> f1 = x1 * x2
--> f2 = x1 + x2*x2

Jacobian:
df1/dx1 = x2    df2/dx2 = x1
df2/dx1 = 1     df2/dx2 = 2*x2

with x1 = 2 and x2 = 3
f1 = 6
f2 = 11

df1/dfx = 3     df2/dx2 = 2
df2/dx1 = 1     df2/dx2 = 6
*/
void f(Array<Dual, Borrow<Dual, BorrowTrait>>& y, const Array<Dual, Borrow<Dual, BorrowTrait>>& x, const double whatever) {
  Dual x1 = at(x, Integer(1));
  Dual x2 = at(x, Integer(2));
  Dual y1 = at(y, Integer(1));
  Dual y2 = at(y, Integer(2));
  y1 = x1*x2;
  y2 = x1 + x2*x2;

  subset(y, Integer(1)) = y1;
  subset(y, Integer(2)) = y2;
}

using RealTypeETR = Variable<Double>;
auto f_rev_ad(Array<RealTypeETR, Buffer<RealTypeETR>>& y, const Array<RealTypeETR, Buffer<RealTypeETR>>& x, const double whatever) {
  RealTypeETR x1 = at(x, Integer(1));
  RealTypeETR x2 = at(x, Integer(2));
  RealTypeETR y1 = at(y, Integer(1));
  RealTypeETR y2 = at(y, Integer(2));
  y1 = x1*x2;
  y2 = x1 + x2*x2;

  subset(y, Integer(1)) = y1;
  subset(y, Integer(2)) = y2;
  return y;
}

// [[Rcpp::export]]
void test_jacobian() {
  auto compare = [](double l, double r) {
    double TOL = 1e-5;
    return std::abs(l - r) < TOL;
  };
  double whatever = 0.0;

  // Forward mode
  {
    std::vector<double> owner_y{0, 0};
    std::vector<double> owner_y_dot{0, 0};
    std::vector<double> owner_x{2, 3};
    std::vector<double> owner_x_dot{0, 0};
    Array<Dual, Borrow<Dual, BorrowTrait>> y(owner_y.data(), owner_y_dot.data(), owner_y.size(), std::vector<std::size_t>{2});
    Array<Dual, Borrow<Dual, BorrowTrait>> x(owner_x.data(), owner_x_dot.data(), owner_x.size(), std::vector<std::size_t>{2});
    Array<Double, Buffer<Double>> jac;

    jacobian_forward<0, 1>(jac, &f, std::ref(y), std::ref(x), whatever);
    ass<"result of f[0]">(compare(get_val(y.get(0)), 6.0));
    ass<"result of f[1]">(compare(get_val(y.get(1)), 11.0));
    ass<"Jac calculated by dual[0]">(compare(get_val(jac.get(0)), 3.0));
    ass<"Jac calculated by dual[1]">(compare(get_val(jac.get(1)), 1.0));
    ass<"Jac calculated by dual[2]">(compare(get_val(jac.get(2)), 2.0));
    ass<"Jac calculated by dual[3]">(compare(get_val(jac.get(3)), 6.0));

  }
  // Reverse mode
  {
    Array<RealTypeETR, Buffer<RealTypeETR>> y = c(RealTypeETR(0), RealTypeETR(0));
    Array<RealTypeETR, Buffer<RealTypeETR>> x = c(RealTypeETR(2), RealTypeETR(3));
    Array<Double, Buffer<Double>> jac;
    jacobian_backward<0, 1>(jac, f_rev_ad, std::ref(y), std::ref(x), whatever);
    ass<"result of f_rev_ad[0]">(compare(get_val(y.get(0)), 6.0));
    ass<"result of f_rev_ad[1]">(compare(get_val(y.get(1)), 11.0));
    ass<"Jac calculated by rev AD[0]">(compare(get_val(jac.get(0)), 3.0));
    ass<"Jac calculated by rev AD[1]">(compare(get_val(jac.get(1)), 1.0));
    ass<"Jac calculated by rev AD[2]">(compare(get_val(jac.get(2)), 2.0));
    ass<"Jac calculated by rev AD[3]">(compare(get_val(jac.get(3)), 6.0));
  }

}
