#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

// backsolve/forwardsolve have no factorization step (dtrsm takes no info
// parameter, unlike dgetrf/dpotrf), so a zero diagonal entry has to be
// checked explicitly before the substitution runs -- see check_tri_diag()
// in TriSolve.hpp (plain/forward modes) and push_trisolve() in Scalars.hpp
// (reverse mode). ass<> routes through Rcpp::stop() in a package build, so
// the failure surfaces as Rcpp::exception, not std::runtime_error.

static bool close(double l, double r) { return std::abs(l - r) < 1e-9; }

// [[Rcpp::export]]
void test_trisolve_zero_diag() {
  auto make_mat = [](double v0, double v1, double v2, double v3, std::size_t n) {
    Array<Double, Buffer<Double>> M;
    M = c(Double(v0), Double(v1), Double(v2), Double(v3));
    M.dim = std::vector<std::size_t>{n, n};
    return M;
  };
  auto make_vec = [](double a, double b) {
    Array<Double, Buffer<Double>> v;
    v = c(Double(a), Double(b));
    return v;
  };

  // ---- plain: zero on the upper-triangular diagonal throws ---------------
  {
    auto R = make_mat(0.0, 0.0, 1.0, 2.0, 2); // col-major: [[0,1],[0,2]]
    auto b = make_vec(1.0, 1.0);
    try {
      auto x = backsolve(R, b);
      ass<"backsolve zero diagonal must throw">(false);
    } catch (const Rcpp::exception& e) {
      const std::string expected = "backsolve: matrix is exactly singular (zero on the diagonal)";
      ass<"backsolve zero diagonal message">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // ---- plain: zero on the lower-triangular diagonal throws ---------------
  {
    auto L = make_mat(0.0, 1.0, 0.0, 2.0, 2); // col-major: [[0,0],[1,2]]
    auto b = make_vec(1.0, 1.0);
    try {
      auto x = forwardsolve(L, b);
      ass<"forwardsolve zero diagonal must throw">(false);
    } catch (const Rcpp::exception& e) {
      const std::string expected = "forwardsolve: matrix is exactly singular (zero on the diagonal)";
      ass<"forwardsolve zero diagonal message">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // ---- plain: non-singular still works, matches direct substitution ------
  {
    auto R = make_mat(2.0, 0.0, 1.0, 3.0, 2); // col-major: [[2,1],[0,3]]
    auto b = make_vec(1.0, 1.0);
    auto x = backsolve(R, b);
    ass<"backsolve normal x[1]">(close(get_val(x.get(1)), 1.0 / 3.0));
    ass<"backsolve normal x[0]">(close(get_val(x.get(0)), (1.0 - 1.0 / 3.0) / 2.0));
  }
  // ---- forward mode (Dual): zero diagonal throws --------------------------
  {
    Array<Dual, Buffer<Dual>> R;
    R = c(Dual(0.0, 0.0), Dual(0.0, 0.0), Dual(1.0, 0.0), Dual(2.0, 0.0));
    R.dim = std::vector<std::size_t>{2, 2};
    auto b = make_vec(1.0, 1.0);
    try {
      auto x = backsolve(R, b);
      ass<"backsolve dual zero diagonal must throw">(false);
    } catch (const Rcpp::exception& e) {
      const std::string expected = "backsolve: matrix is exactly singular (zero on the diagonal)";
      ass<"backsolve dual zero diagonal message">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // ---- reverse mode (ReverseDouble): zero diagonal throws ------------------
  {
    TAPE_INTERN.clear();
    Array<ReverseDouble, Buffer<ReverseDouble>> R;
    R = c(ReverseDouble::Var(0.0), ReverseDouble::Var(0.0),
          ReverseDouble::Var(1.0), ReverseDouble::Var(2.0));
    R.dim = std::vector<std::size_t>{2, 2};
    auto b = make_vec(1.0, 1.0);
    try {
      auto x = backsolve(R, b);
      ass<"backsolve reverse zero diagonal must throw">(false);
    } catch (const Rcpp::exception& e) {
      const std::string expected = "backsolve: matrix is exactly singular (zero on the diagonal)";
      ass<"backsolve reverse zero diagonal message">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
}
