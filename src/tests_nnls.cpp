#include <Rcpp.h>
#include "../inst/include/etr.hpp"
#include <vector>
#include <cmath>
using namespace etr;

// Reference: the actual Lawson-Hanson Fortran routine (src/nnls.f), same
// algorithm the CRAN `nnls` package wraps. Compiled into ast2ast's own
// test build only -- not something consuming packages link against, see
// the discussion around etr_bits/Core/nnls.hpp for why that distinction
// matters.
extern "C" {
  void F77_NAME(nnls)(double* a, int* mda, int* m, int* n, double* b,
                       double* x, double* rnorm, double* w, double* zz,
                       int* index, int* mode);
}

static void run_fortran_nnls(std::vector<double> a, int m, int n, std::vector<double> b,
                              std::vector<double>& x, double& rnorm, int& mode) {
  x.assign(n, 0.0);
  std::vector<double> w(n, 0.0), zz(m, 0.0);
  std::vector<int> index(n, 0);
  int mda = m;
  F77_CALL(nnls)(a.data(), &mda, &m, &n, b.data(), x.data(), &rnorm, w.data(), zz.data(), index.data(), &mode);
}

static void run_etr_nnls(const std::vector<double>& a, int m, int n, const std::vector<double>& b,
                          std::vector<double>& x, double& rnorm, int& mode) {
  std::vector<Double> A(a.size());
  for (std::size_t i = 0; i < a.size(); ++i) A[i] = Double(a[i]);
  std::vector<Double> B(b.size());
  for (std::size_t i = 0; i < b.size(); ++i) B[i] = Double(b[i]);
  std::vector<Double> xd, wd;
  Double rn;
  nnls_core(A, m, n, B, xd, wd, rn, mode);
  x.resize(xd.size());
  for (std::size_t i = 0; i < xd.size(); ++i) x[i] = xd[i].val;
  rnorm = rn.val;
}

static void compare_case(const char* label, const std::vector<double>& a, int m, int n,
                          const std::vector<double>& b) {
  std::vector<double> x_f, x_e;
  double rnorm_f = 0.0, rnorm_e = 0.0;
  int mode_f = 0, mode_e = 0;
  run_fortran_nnls(a, m, n, b, x_f, rnorm_f, mode_f);
  run_etr_nnls(a, m, n, b, x_e, rnorm_e, mode_e);

  ass<"nnls vs nnls.f: mode mismatch">(mode_f == mode_e);
  for (int i = 0; i < n; ++i) {
    if (std::abs(x_f[i] - x_e[i]) >= 1e-8) {
      Rcpp::Rcout << label << ": x[" << i << "] fortran=" << x_f[i]
                  << " etr=" << x_e[i] << "\n";
    }
    ass<"nnls vs nnls.f: x mismatch">(std::abs(x_f[i] - x_e[i]) < 1e-8);
  }
  ass<"nnls vs nnls.f: rnorm mismatch">(std::abs(rnorm_f - rnorm_e) < 1e-8);
}

// [[Rcpp::export]]
void test_nnls() {
  // ---- case 1: unconstrained solution already feasible, no active bound --
  // A = [[1,0],[1,1],[0,1]] (col-major), b = [2,1,1].
  // Normal equations: A^T A = [[2,1],[1,2]], A^T b = [3,2] -> x = [4/3, 1/3].
  {
    std::vector<double> a = {1.0, 1.0, 0.0,  0.0, 1.0, 1.0}; // col1={1,1,0}, col2={0,1,1}
    std::vector<double> b = {2.0, 1.0, 1.0};
    compare_case("case1", a, 3, 2, b);

    std::vector<double> x, r; double rnorm; int mode;
    run_etr_nnls(a, 3, 2, b, x, rnorm, mode);
    ass<"nnls case1 x[0]">(std::abs(x[0] - 4.0 / 3.0) < 1e-8);
    ass<"nnls case1 x[1]">(std::abs(x[1] - 1.0 / 3.0) < 1e-8);
  }

  // ---- case 2: unconstrained solution negative -> clamped to 0 -----------
  // A = [[1],[1]], b = [-1,-1]. Unconstrained x = -1 -> NNLS gives x=0,
  // residual = b itself, rnorm = ||b|| = sqrt(2).
  {
    std::vector<double> a = {1.0, 1.0};
    std::vector<double> b = {-1.0, -1.0};
    compare_case("case2", a, 2, 1, b);

    std::vector<double> x, r; double rnorm; int mode;
    run_etr_nnls(a, 2, 1, b, x, rnorm, mode);
    ass<"nnls case2 x[0]==0">(std::abs(x[0]) < 1e-8);
    ass<"nnls case2 rnorm==sqrt(2)">(std::abs(rnorm - std::sqrt(2.0)) < 1e-8);
  }

  // ---- case 3: mixed -- one variable clamped, one free -------------------
  // A = [[1,0],[0,1],[0,1]] (col-major), b = [1,-1,-1].
  // Column 2 alone can't reach negative targets (x>=0), so x2 -> 0;
  // x1 solves the remaining 1D problem against column 1: x1 = 1.
  {
    std::vector<double> a = {1.0, 0.0, 0.0,  0.0, 1.0, 1.0};
    std::vector<double> b = {1.0, -1.0, -1.0};
    compare_case("case3", a, 3, 2, b);
  }
}
