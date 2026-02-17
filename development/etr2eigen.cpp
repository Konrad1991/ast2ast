// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
// [[Rcpp::depends(ast2ast)]]
// [[Rcpp::plugins(cpp2a)]]
#include "etr.hpp"
#include <chrono>

using namespace etr;

// [[Rcpp::export]]
void test() {
  auto pr = [](const std::string& message, auto tstart, auto tend) {
    std::cout << message << ((tend - tstart).count())/1000000.0 << std::endl;
  };
  using clock = std::chrono::steady_clock;
  // ast2ast version
  {
    Array<Double, Buffer<Double>> A = matrix(colon(Double(1), Double(12)), Integer(4), Integer(3));
    auto res = mat_mul(t(A), A);
    print(res);
  }
  {
    Array<Double, Buffer<Double>> A_etr = matrix(colon(Double(1), Double(1240)), Integer(40), Integer(31));
    auto A = map_matrix(A_etr);
    const Eigen::Index p = A.cols();
    Array<Double, Buffer<Double>> G_etr = matrix(Double(0.0), Integer(static_cast<int>(p)), Integer(static_cast<int>(p)));
    auto G = map_matrix(G_etr);
    G.setZero();
    // --- compute Gram matrix ---
    const auto t0 = clock::now();
    G.noalias() = A.transpose() * A;
    const auto t1 = clock::now();
    pr("Standard version Double: ", t0, t1);
  }
  // Calculating derivs of linear algebra (reverse mode)
  {
    Array<Variable<Double>, Buffer<Variable<Double>>> A_etr = matrix(colon(Variable<Double>(1), Variable<Double>(1240)), Integer(40), Integer(31)); // 53 allocations
    auto A = map_matrix(A_etr); // 0 allocations
    const Eigen::Index p = A.cols(); // 0 allocations
    Array<Variable<Double>, Buffer<Variable<Double>>> G_etr = matrix(Variable<Double>(0.0), Integer(static_cast<int>(p)), Integer(static_cast<int>(p))); // 25 allocations
    auto G = map_matrix(G_etr); // 0 allocations
    // --- compute Gram matrix ---
    const auto t0 = clock::now();
    G.noalias() = A.transpose() * A; // 276 allocations
    auto d = deriv(G_etr, A_etr); // 4 allocations
    const auto t1 = clock::now();
    pr("Reverse: ", t0, t1);
  }
  // Calculating derivs of linear algebra (forward mode)
  {
    Array<Dual, Buffer<Dual>> A_etr = matrix(colon(Dual(1), Dual(1240)), Integer(40), Integer(31));
    auto A = map_matrix(A_etr);
    const Eigen::Index p = A.cols();
    Array<Dual, Buffer<Dual>> G_etr = matrix(Dual(0.0), Integer(static_cast<int>(p)), Integer(static_cast<int>(p)));
    auto G = map_matrix(G_etr);
    G.setZero();
    // --- compute Gram matrix ---
    const auto t0 = clock::now();
    for (std::size_t i = 1; i <= 1240; i++) {
      seed(A_etr, Integer(i));
      G.noalias() = A.transpose() * A;
      unseed(A_etr, Integer(i));
      auto d = get_dot(G_etr);
    }
    const auto t1 = clock::now();
    pr("Forward: ", t0, t1);
  }
}
