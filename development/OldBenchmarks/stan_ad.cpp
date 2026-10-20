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

// Eager Eigen operator*: each scalar fmadd becomes a var on the autodiff
// stack. ~O(M*K*N) stack entries. Mirrors what a naive user writes.
// [[Rcpp::export]]
Eigen::MatrixXd f_rev_stan(const Eigen::MatrixXd& A_in,
                           const Eigen::MatrixXd& B_in) {

  const int M = A_in.rows();
  const int K = A_in.cols();   // == B_in.rows() by conformability
  const int N = B_in.cols();

  // convert to AD variables
  Matrix<var, Dynamic, Dynamic> A(M, K);
  Matrix<var, Dynamic, Dynamic> B(K, N);

  for (int i = 0; i < M; i++)
    for (int j = 0; j < K; j++)
      A(i,j) = A_in(i,j);

  for (int i = 0; i < K; i++)
    for (int j = 0; j < N; j++)
      B(i,j) = B_in(i,j);

  // forward pass
  Matrix<var, Dynamic, Dynamic> C = A * B;

  var s = C.sum();

  // reverse pass
  s.grad();

  // extract gradients
  MatrixXd dA(M, K);

  for (int i = 0; i < M; i++)
    for (int j = 0; j < K; j++)
      dA(i,j) = A(i,j).adj();

  // Without this, repeated calls grow Stan's autodiff arena monotonically.
  stan::math::recover_memory();

  return dA;
}

// Block-aware path: stan::math::multiply records ONE node for the matmul
// with an analytical backward. Closest apples-to-apples comparison to
// ast2ast's BlockOp::MatMul.
// [[Rcpp::export]]
Eigen::MatrixXd f_rev_stan_mul(const Eigen::MatrixXd& A_in,
                               const Eigen::MatrixXd& B_in) {

  const int M = A_in.rows();
  const int K = A_in.cols();
  const int N = B_in.cols();

  Matrix<var, Dynamic, Dynamic> A(M, K);
  Matrix<var, Dynamic, Dynamic> B(K, N);

  for (int i = 0; i < M; i++)
    for (int j = 0; j < K; j++)
      A(i,j) = A_in(i,j);

  for (int i = 0; i < K; i++)
    for (int j = 0; j < N; j++)
      B(i,j) = B_in(i,j);

  Matrix<var, Dynamic, Dynamic> C = stan::math::multiply(A, B);
  var s = stan::math::sum(C);
  s.grad();

  MatrixXd dA(M, K);
  for (int i = 0; i < M; i++)
    for (int j = 0; j < K; j++)
      dA(i,j) = A(i,j).adj();

  stan::math::recover_memory();
  return dA;
}
