#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"

#include <Eigen/Core>
#include <Eigen/Dense>
#include <iostream>
#include <chrono>

using namespace etr;

namespace etr_eigen_detail {
template<class S>
struct NumTraitsBase : Eigen::NumTraits<double> {
  using Real = double;
  using NonInteger = S;
  using Nested = S;
  enum {
    IsComplex = 0,
    IsInteger = 0,
    IsSigned = 1,
    RequireInitialization = 1,
    ReadCost = 1,
    AddCost = 3,
    MulCost = 3
  };
  static inline Real epsilon() { return std::numeric_limits<double>::epsilon(); }
  static inline Real dummy_precision() { return 1e-12; }
  static inline int digits10() { return std::numeric_limits<double>::digits10; }
};
}

namespace Eigen {
template<> struct NumTraits<etr::Double> : etr_eigen_detail::NumTraitsBase<etr::Double> {
  static inline etr::Double highest() { return etr::Double(std::numeric_limits<double>::max()); }
  static inline etr::Double lowest()  { return etr::Double(std::numeric_limits<double>::lowest()); }
};

template<> struct NumTraits<etr::Dual> : etr_eigen_detail::NumTraitsBase<etr::Dual> {
  static inline etr::Dual highest() { return etr::Dual(std::numeric_limits<double>::max(), 0.0); }
  static inline etr::Dual lowest()  { return etr::Dual(std::numeric_limits<double>::lowest(), 0.0); }
};

template<> struct NumTraits<etr::Variable<etr::Double>> : etr_eigen_detail::NumTraitsBase<etr::Variable<etr::Double>> {
  static inline etr::Variable<etr::Double> highest() { return etr::Variable<etr::Double>(std::numeric_limits<double>::max()); }
  static inline etr::Variable<etr::Double> lowest()  { return etr::Variable<etr::Double>(std::numeric_limits<double>::lowest()); }
};

template<typename BinOp>
struct ScalarBinaryOpTraits<etr::Variable<etr::Double>, double, BinOp> {
  using ReturnType = etr::Variable<etr::Double>;
};

template<typename BinOp>
struct ScalarBinaryOpTraits<double, etr::Variable<etr::Double>, BinOp> {
  using ReturnType = etr::Variable<etr::Double>;
};

// If you also want mixing with etr::Double:
template<typename BinOp>
struct ScalarBinaryOpTraits<etr::Variable<etr::Double>, etr::Double, BinOp> {
  using ReturnType = etr::Variable<etr::Double>;
};

template<typename BinOp>
struct ScalarBinaryOpTraits<etr::Double, etr::Variable<etr::Double>, BinOp> {
  using ReturnType = etr::Variable<etr::Double>;
};

} // namespace Eigen

// ---- ETR <-> Eigen bridge -----------------------------------------
constexpr int ETRLayout = Eigen::ColMajor;

template<class Scalar>
using EMat = Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic, ETRLayout>;

template<class Scalar>
using EVec = Eigen::Matrix<Scalar, Eigen::Dynamic, 1>;

// Borrow an etr::Array as Eigen matrix
template<class Scalar, class BufferT>
auto map_matrix(Array<Scalar, BufferT>& a) {
  ass<"Expected matrix object">(a.get_dim().size() == 2);
  using MapT = Eigen::Map<EMat<Scalar>, Eigen::Unaligned>;
  const Eigen::Index nr = static_cast<Eigen::Index>(a.get_dim()[0]);
  const Eigen::Index nc = static_cast<Eigen::Index>(a.get_dim()[1]);
  return MapT(a.d.p, nr, nc);
}

int main() {
  auto pr = [](const std::string& message, auto tstart, auto tend) {
    std::cout << message << ((tend - tstart).count())/1000000.0 << std::endl;
  };
  using clock = std::chrono::steady_clock;
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
