#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// Direct C++ tests of etr::lbfgsb (the R-level function_node wiring is separate).
// Loss is a std::function<S(A)> exactly as codegen emits it for a scalar-valued
// fn() under derivative = "forward" (Dual) / "reverse" (ReverseDouble).

using FwdArr = Array<Dual, Buffer<Dual>>;
using RevArr = Array<ReverseDouble, Buffer<ReverseDouble>>;

static bool lclose(double l, double r, double tol = 1e-4) {
  return std::abs(l - r) < tol;
}

static const double INF = std::numeric_limits<double>::infinity();

static FwdArr fwd_vec(double a, double b) {
  FwdArr v(SI{2});
  v.set(0, Dual(a, 0.0));
  v.set(1, Dual(b, 0.0));
  return v;
}
static FwdArr fwd_scalar(double a) {
  FwdArr v(SI{1});
  v.set(0, Dual(a, 0.0));
  return v;
}

// [[Rcpp::export]]
void test_lbfgsb_quadratic_forward() {
  // f(x) = (x1 - 1)^2 + (x2 + 2)^2  -> argmin (1, -2), min 0
  std::function<Dual(FwdArr)> loss = [](FwdArr v) -> Dual {
    Dual a = at(v, Integer(1)) - Dual(1.0);
    Dual b = at(v, Integer(2)) + Dual(2.0);
    return a * a + b * b;
  };

  auto r = lbfgsb(loss, fwd_vec(0.0, 0.0), fwd_scalar(-INF), fwd_scalar(INF),
                  Integer(100), Double(1e7), Double(0.0), Integer(5));

  ass<"quad fwd: convergence == 0">(get_val(r.convergence) == 0);
  ass<"quad fwd: par1">(lclose(get_val(r.par.get(0)), 1.0));
  ass<"quad fwd: par2">(lclose(get_val(r.par.get(1)), -2.0));
  ass<"quad fwd: value ~ 0">(lclose(get_val(r.value), 0.0));
  ass<"quad fwd: fncount > 0">(get_val(r.counts.get(0)) > 0);
  ass<"quad fwd: grcount > 0">(get_val(r.counts.get(1)) > 0);
}

// [[Rcpp::export]]
void test_lbfgsb_quadratic_reverse() {
  TAPE_INTERN.clear();
  // same objective, reverse-mode loss
  std::function<ReverseDouble(RevArr)> loss = [](RevArr v) -> ReverseDouble {
    ReverseDouble a = at(v, Integer(1)) - ReverseDouble(1.0);
    ReverseDouble b = at(v, Integer(2)) + ReverseDouble(2.0);
    return a * a + b * b;
  };

  RevArr x0(SI{2});
  x0.set(0, ReverseDouble::Var(0.0));
  x0.set(1, ReverseDouble::Var(0.0));

  auto r = lbfgsb(loss, x0, fwd_scalar(-INF), fwd_scalar(INF),
                  Integer(100), Double(1e7), Double(0.0), Integer(5));

  ass<"quad rev: convergence == 0">(get_val(r.convergence) == 0);
  ass<"quad rev: par1">(lclose(get_val(r.par.get(0)), 1.0));
  ass<"quad rev: par2">(lclose(get_val(r.par.get(1)), -2.0));
  ass<"quad rev: value ~ 0">(lclose(get_val(r.value), 0.0));
}

// [[Rcpp::export]]
void test_lbfgsb_bounds() {
  // f(x) = (x1 - 3)^2 + (x2 - 3)^2 with upper = 1 -> argmin (1, 1), min 8
  std::function<Dual(FwdArr)> loss = [](FwdArr v) -> Dual {
    Dual a = at(v, Integer(1)) - Dual(3.0);
    Dual b = at(v, Integer(2)) - Dual(3.0);
    return a * a + b * b;
  };

  auto r = lbfgsb(loss, fwd_vec(0.0, 0.0), fwd_scalar(-INF), fwd_scalar(1.0),
                  Integer(100), Double(1e7), Double(0.0), Integer(5));

  ass<"bounds: convergence == 0">(get_val(r.convergence) == 0);
  ass<"bounds: par1 at upper">(lclose(get_val(r.par.get(0)), 1.0));
  ass<"bounds: par2 at upper">(lclose(get_val(r.par.get(1)), 1.0));
  ass<"bounds: value ~ 8">(lclose(get_val(r.value), 8.0));
}

// [[Rcpp::export]]
void test_lbfgsb_rosenbrock() {
  // 100*(x2 - x1^2)^2 + (1 - x1)^2  -> argmin (1, 1), min 0
  std::function<Dual(FwdArr)> loss = [](FwdArr v) -> Dual {
    Dual x1 = at(v, Integer(1));
    Dual x2 = at(v, Integer(2));
    Dual t1 = x2 - x1 * x1;
    Dual t2 = Dual(1.0) - x1;
    return Dual(100.0) * t1 * t1 + t2 * t2;
  };

  auto r = lbfgsb(loss, fwd_vec(-1.2, 1.0), fwd_scalar(-INF), fwd_scalar(INF),
                  Integer(300), Double(1e7), Double(0.0), Integer(10));

  ass<"rosenbrock: convergence == 0">(get_val(r.convergence) == 0);
  ass<"rosenbrock: par1">(lclose(get_val(r.par.get(0)), 1.0, 1e-3));
  ass<"rosenbrock: par2">(lclose(get_val(r.par.get(1)), 1.0, 1e-3));
  ass<"rosenbrock: value ~ 0">(lclose(get_val(r.value), 0.0, 1e-4));
}

// [[Rcpp::export]]
void test_lbfgsb_bound_length_error() {
  std::function<Dual(FwdArr)> loss = [](FwdArr v) -> Dual {
    return at(v, Integer(1)) * at(v, Integer(1)) + at(v, Integer(2)) * at(v, Integer(2));
  };
  // lower has length 3, x has length 2 -> must be rejected
  FwdArr bad_lower(SI{3});
  bad_lower.set(0, Dual(-INF, 0.0));
  bad_lower.set(1, Dual(-INF, 0.0));
  bad_lower.set(2, Dual(-INF, 0.0));

  bool threw = false;
  try {
    lbfgsb(loss, fwd_vec(1.0, 1.0), bad_lower, fwd_scalar(INF),
           Integer(100), Double(1e7), Double(0.0), Integer(5));
  } catch (const std::exception&) {
    threw = true;
  }
  ass<"lbfgsb rejects wrong-length bound">(threw);
}
