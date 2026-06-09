#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_diag() {
  auto compare = [](const auto& got, const auto& expected) {
    const double TOL = 1e-6;
    for (std::size_t i = 0; i < expected.size(); i++) {
      ass<"Equal elements in diag">(std::abs(expected[i] - got[i]) < TOL);
    }
  };
  auto check_dim = [](const auto& got, const std::vector<std::size_t>& dim) {
    std::size_t n = 1;
    for (std::size_t d : dim) n *= d;
    ass<"diag size">(got.size() == n);
    ass<"diag rank">(got.get_dim().size() == dim.size());
    for (std::size_t i = 0; i < dim.size(); i++) {
      ass<"diag dim">(got.get_dim()[i] == dim[i]);
    }
  };

  // scalar x, square
  {
    Array<Double, Buffer<Double>> a;
    const double pi = 3.1415;
    std::vector<double> expected = {
      pi, 0.0, 0.0,
      0.0, pi, 0.0,
      0.0, 0.0, pi
    };
    a = diag<Double>(Double(pi), Integer(3), Integer(3));
    check_dim(a, {3, 3});
    double* p = a.d.data();
    compare(p, expected);
  }
  // scalar x, nrow < ncol -> diagonal of length nrow
  {
    Array<Double, Buffer<Double>> a;
    // column-major 2x3, diag entries at (0,0) and (1,1)
    std::vector<double> expected = {2.0, 0.0, 0.0, 2.0, 0.0, 0.0};
    a = diag<Double>(Double(2.0), Integer(2), Integer(3));
    check_dim(a, {2, 3});
    double* p = a.d.data();
    compare(p, expected);
  }
  // scalar x, nrow > ncol -> diagonal of length ncol
  {
    Array<Double, Buffer<Double>> a;
    // column-major 3x2, diag entries at (0,0) and (1,1)
    std::vector<double> expected = {5.0, 0.0, 0.0, 0.0, 5.0, 0.0};
    a = diag<Double>(Double(5.0), Integer(3), Integer(2));
    check_dim(a, {3, 2});
    double* p = a.d.data();
    compare(p, expected);
  }
  // array x, length matches diagonal
  {
    Array<Double, Buffer<Double>> a;
    std::vector<double> expected = {
      1.0, 0.0, 0.0,
      0.0, 2.0, 0.0,
      0.0, 0.0, 3.0
    };
    a = diag<Double>(c(Double(1.0), Double(2.0), Double(3.0)), Integer(3), Integer(3));
    check_dim(a, {3, 3});
    double* p = a.d.data();
    compare(p, expected);
  }
  // array x shorter than diagonal -> recycled
  {
    Array<Double, Buffer<Double>> a;
    // x = {1, 2}; diagonal = x[0], x[1], x[0]
    std::vector<double> expected = {
      1.0, 0.0, 0.0,
      0.0, 2.0, 0.0,
      0.0, 0.0, 1.0
    };
    a = diag<Double>(c(Double(1.0), Double(2.0)), Integer(3), Integer(3));
    check_dim(a, {3, 3});
    double* p = a.d.data();
    compare(p, expected);
  }
  // array x longer than diagonal -> truncated
  {
    Array<Double, Buffer<Double>> a;
    // x = {1, 2, 3, 4}; diagonal = x[0], x[1]
    std::vector<double> expected = {1.0, 0.0, 0.0, 2.0};
    a = diag<Double>(c(Double(1.0), Double(2.0), Double(3.0), Double(4.0)), Integer(2), Integer(2));
    check_dim(a, {2, 2});
    double* p = a.d.data();
    compare(p, expected);
  }
  // nrow / ncol given as length-1 vectors
  {
    Array<Double, Buffer<Double>> a;
    std::vector<double> expected = {
      7.0, 0.0, 0.0, 0.0,
      0.0, 7.0, 0.0, 0.0,
      0.0, 0.0, 7.0, 0.0,
      0.0, 0.0, 0.0, 7.0
    };
    a = diag<Double>(Double(7.0), c(Integer(4)), c(Integer(4)));
    check_dim(a, {4, 4});
    double* p = a.d.data();
    compare(p, expected);
  }
  // NA dimension -> throw
  {
    bool catched = false;
    try {
      Array<Double, Buffer<Double>> a = diag<Double>(Double(1.0), Integer::NA(), Integer(3));
    }
    catch (const std::exception& e) {
      catched = true;
      const std::string expected_message = "Found NA value in diag";
      ass<"diag NA message">(std::strcmp(e.what(), expected_message.c_str()) == 0);
    }
    ass<"Did not throw">(catched);
  }
}
