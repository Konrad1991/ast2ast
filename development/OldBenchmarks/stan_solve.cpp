// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(StanHeaders)]]
#include <stan/math/mix.hpp> // stuff from mix/ must come first
#include <stan/math.hpp>     // finally pull in everything from rev/ and prim/
#include <Rcpp.h>
#include <RcppEigen.h>       // do this AFTER including stuff from stan/math

// [[Rcpp::plugins(cpp17)]]
using namespace Eigen;
using stan::math::var;

// Reverse-mode gradient of  s = sum(A^-1 b)  w.r.t. A.
//
// Block-aware path: stan::math::mdivide_left(A, b) records ONE node for the
// linear solve with an analytical backward. This is the apples-to-apples
// comparison to ast2ast's BlockOp::Solve (LU once, dgetrs for the adjoint).
// [[Rcpp::export]]
Eigen::MatrixXd f_rev_stan_solve(const Eigen::MatrixXd& A_in,
                                 const Eigen::VectorXd& b_in) {
  const int n = A_in.rows();

  Matrix<var, Dynamic, Dynamic> A(n, n);
  Matrix<var, Dynamic, 1>       b(n);
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      A(i, j) = A_in(i, j);
  for (int i = 0; i < n; i++) b(i) = b_in(i);

  // forward pass: x = A^-1 b
  Matrix<var, Dynamic, 1> x = stan::math::mdivide_left(A, b);
  var s = stan::math::sum(x);

  // reverse pass
  s.grad();

  MatrixXd dA(n, n);
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      dA(i, j) = A(i, j).adj();

  // Without this, repeated calls grow Stan's autodiff arena monotonically.
  stan::math::recover_memory();
  return dA;
}

// Eager path: form the explicit inverse, then multiply. Records the inverse's
// backward plus a matvec -- what a naive user writes when they need A^-1 b.
// [[Rcpp::export]]
Eigen::MatrixXd f_rev_stan_solve_inv(const Eigen::MatrixXd& A_in,
                                     const Eigen::VectorXd& b_in) {
  const int n = A_in.rows();

  Matrix<var, Dynamic, Dynamic> A(n, n);
  Matrix<var, Dynamic, 1>       b(n);
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      A(i, j) = A_in(i, j);
  for (int i = 0; i < n; i++) b(i) = b_in(i);

  Matrix<var, Dynamic, 1> x = stan::math::inverse(A) * b;
  var s = stan::math::sum(x);
  s.grad();

  MatrixXd dA(n, n);
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      dA(i, j) = A(i, j).adj();

  stan::math::recover_memory();
  return dA;
}
