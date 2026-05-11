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

// [[Rcpp::export]]
Eigen::MatrixXd f_rev_stan(const Eigen::MatrixXd& A_in,
                           const Eigen::MatrixXd& B_in) {

  int n = A_in.rows();
  int m = B_in.cols();

  // convert to AD variables
  Matrix<var, Dynamic, Dynamic> A(n, m);
  Matrix<var, Dynamic, Dynamic> B(m, n);

  for (int i = 0; i < n; i++)
    for (int j = 0; j < m; j++)
      A(i,j) = A_in(i,j);

  for (int i = 0; i < m; i++)
    for (int j = 0; j < n; j++)
      B(i,j) = B_in(i,j);

  // forward pass
  Matrix<var, Dynamic, Dynamic> C = A * B;

  var s = C.sum();

  // reverse pass
  s.grad();

  // extract gradients
  MatrixXd dA(n, m);

  for (int i = 0; i < n; i++)
    for (int j = 0; j < m; j++)
      dA(i,j) = A(i,j).adj();

  return dA;
}
