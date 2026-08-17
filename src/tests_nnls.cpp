#include <Rcpp.h>
#include "../inst/include/etr.hpp"
#include <vector>
#include <cmath>
using namespace etr;

// Sanity test for etr::nnls_core against hand-derived expected values --
// no longer compared against the Lawson-Hanson Fortran routine (nnls.f is
// kept under development/ as reference material only, not compiled into
// the package; see etr_bits/Core/nnls.hpp for the C++ port). The
// R-side test (inst/tinytest/test_nnls.R) covers broader comparison
// against the CRAN `nnls` package instead.

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

// [[Rcpp::export]]
void test_nnls() {
  // ---- case 1: unconstrained solution already feasible, no active bound --
  // A = [[1,0],[1,1],[0,1]] (col-major), b = [2,1,1].
  // Normal equations: A^T A = [[2,1],[1,2]], A^T b = [3,2] -> x = [4/3, 1/3].
  {
    std::vector<double> a = {1.0, 1.0, 0.0,  0.0, 1.0, 1.0}; // col1={1,1,0}, col2={0,1,1}
    std::vector<double> b = {2.0, 1.0, 1.0};
    std::vector<double> x; double rnorm; int mode;
    run_etr_nnls(a, 3, 2, b, x, rnorm, mode);
    ass<"nnls case1 mode">(mode == 1);
    ass<"nnls case1 x[0]">(std::abs(x[0] - 4.0 / 3.0) < 1e-8);
    ass<"nnls case1 x[1]">(std::abs(x[1] - 1.0 / 3.0) < 1e-8);
  }

  // ---- case 2: unconstrained solution negative -> clamped to 0 -----------
  // A = [[1],[1]], b = [-1,-1]. Unconstrained x = -1 -> NNLS gives x=0,
  // residual = b itself, rnorm = ||b|| = sqrt(2).
  {
    std::vector<double> a = {1.0, 1.0};
    std::vector<double> b = {-1.0, -1.0};
    std::vector<double> x; double rnorm; int mode;
    run_etr_nnls(a, 2, 1, b, x, rnorm, mode);
    ass<"nnls case2 mode">(mode == 1);
    ass<"nnls case2 x[0]==0">(std::abs(x[0]) < 1e-8);
    ass<"nnls case2 rnorm==sqrt(2)">(std::abs(rnorm - std::sqrt(2.0)) < 1e-8);
  }

  // ---- case 3: mixed -- one variable clamped, one free -------------------
  // A = [[1,0],[0,1],[0,1]] (col-major), b = [1,-1,-1].
  // Column 2 alone can't reach negative targets (x>=0), so x2 -> 0;
  // x1 solves the remaining 1D problem against column 1: x1 = 1.
  // residual = (0,-1,-1) -> rnorm = sqrt(2).
  {
    std::vector<double> a = {1.0, 0.0, 0.0,  0.0, 1.0, 1.0};
    std::vector<double> b = {1.0, -1.0, -1.0};
    std::vector<double> x; double rnorm; int mode;
    run_etr_nnls(a, 3, 2, b, x, rnorm, mode);
    ass<"nnls case3 mode">(mode == 1);
    ass<"nnls case3 x[0]">(std::abs(x[0] - 1.0) < 1e-8);
    ass<"nnls case3 x[1]==0">(std::abs(x[1]) < 1e-8);
    ass<"nnls case3 rnorm==sqrt(2)">(std::abs(rnorm - std::sqrt(2.0)) < 1e-8);
  }
}
