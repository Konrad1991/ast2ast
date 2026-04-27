#define STANDALONE_ETR
#include <string>
#include <iostream>
#include <vector>
#include "stddef.h"
#include <cxxabi.h>
#include "../inst/include/etr_bits/Core/Reflection.hpp"
#include "../inst/include/etr.hpp"
using namespace etr;

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

using RealTypeETR = ReverseDouble;
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
    TAPE_INTERN.clear();
    Array<RealTypeETR, Buffer<RealTypeETR>> y = c(RealTypeETR(0), RealTypeETR(0));
    Array<RealTypeETR, Buffer<RealTypeETR>> x = c(RealTypeETR::Var(2), RealTypeETR::Var(3));
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

// [[Rcpp::export]]
void test_deriv_scalar() {
  // Smoke test of deriv() on the scalar/scalar branch, using only the
  // ReverseDouble surface (no jacobian_backward, no Borrow plumbing).
  auto compare = [](double l, double r) { return std::abs(l - r) < 1e-5; };

  // f(x, y) = sin(x) * y + x^2,  at x=1.5, y=2.0
  // df/dx = cos(x) * y + 2*x   = cos(1.5)*2 + 3      ≈ 0.14147 + 3 = 3.14147
  // df/dy = sin(x)             = sin(1.5)            ≈ 0.99749
  TAPE_INTERN.clear();
  ReverseDouble x = ReverseDouble::Var(1.5);
  ReverseDouble y = ReverseDouble::Var(2.0);
  ReverseDouble f = x.sin() * y + x * x;
  ass<"f value">(compare(get_val(f), std::sin(1.5) * 2.0 + 1.5 * 1.5));

  auto df_dx = deriv(f, x);
  auto df_dy = deriv(f, y);
  ass<"df/dx">(compare(get_val(df_dx.get(0)), std::cos(1.5) * 2.0 + 2.0 * 1.5));
  ass<"df/dy">(compare(get_val(df_dy.get(0)), std::sin(1.5)));
}

// [[Rcpp::export]]
void test_tape_reuse() {
  // Calling the translated function twice on the same thread must give the
  // same answer (exercises the clear()/repush cycle).
  auto compare = [](double l, double r) { return std::abs(l - r) < 1e-5; };
  double whatever = 0.0;
  auto run_once = [&](double x1v, double x2v) {
    TAPE_INTERN.clear();
    Array<RealTypeETR, Buffer<RealTypeETR>> y = c(RealTypeETR(0), RealTypeETR(0));
    Array<RealTypeETR, Buffer<RealTypeETR>> x =
      c(RealTypeETR::Var(x1v), RealTypeETR::Var(x2v));
    Array<Double, Buffer<Double>> jac;
    jacobian_backward<0, 1>(jac, f_rev_ad, std::ref(y), std::ref(x), whatever);
    return jac;
  };

  auto jac_a = run_once(2.0, 3.0);
  auto jac_b = run_once(2.0, 3.0);
  ass<"reuse jac[0]">(compare(get_val(jac_a.get(0)), get_val(jac_b.get(0))));
  ass<"reuse jac[1]">(compare(get_val(jac_a.get(1)), get_val(jac_b.get(1))));
  ass<"reuse jac[2]">(compare(get_val(jac_a.get(2)), get_val(jac_b.get(2))));
  ass<"reuse jac[3]">(compare(get_val(jac_a.get(3)), get_val(jac_b.get(3))));

  auto jac_c = run_once(1.0, 4.0);
  // df1/dx1 = x2 = 4, df1/dx2 = x1 = 1, df2/dx1 = 1, df2/dx2 = 2*x2 = 8.
  ass<"reuse jac_c[0]">(compare(get_val(jac_c.get(0)), 4.0));
  ass<"reuse jac_c[1]">(compare(get_val(jac_c.get(1)), 1.0));
  ass<"reuse jac_c[2]">(compare(get_val(jac_c.get(2)), 1.0));
  ass<"reuse jac_c[3]">(compare(get_val(jac_c.get(3)), 8.0));
}

int main() {
  test_jacobian();
  test_deriv_scalar();
  test_tape_reuse();
}

