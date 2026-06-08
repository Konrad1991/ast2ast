#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_transpose() {
  auto compare = [](const auto& got, const auto& expected) {
    const double TOL = 1e-6;
    for (std::size_t i = 0; i < expected.size(); i++) {
      ass<"Equal elements in transpose">(std::abs(expected[i] - got[i]) < TOL);
    }
  };
  auto check_dim = [](const auto& got, const std::vector<std::size_t>& dim) {
    std::size_t n = 1;
    for (std::size_t d : dim) n *= d;
    ass<"transpose size">(got.size() == n);
    ass<"transpose rank">(got.get_dim().size() == dim.size());
    for (std::size_t i = 0; i < dim.size(); i++) {
      ass<"transpose dim">(got.get_dim()[i] == dim[i]);
    }
  };

  // vector -> 1 x n row matrix (R: t(c(1,2,3)) -> 1x3)
  {
    Array<Double, Buffer<Double>> a;
    std::vector<double> expected = {1.0, 2.0, 3.0};
    a = transpose(c(Double(1.0), Double(2.0), Double(3.0)));
    check_dim(a, {1, 3});
    double* p = a.d.data();
    compare(p, expected);
  }
  // non-square matrix: 2x3 -> 3x2
  {
    Array<Double, Buffer<Double>> a;
    // column-major 2x3: [1 3 5 / 2 4 6]; transpose 3x2: [1 2 / 3 4 / 5 6] -> {1,3,5,2,4,6}
    Array<Double, Buffer<Double>> m = matrix(colon(Double(1.0), Double(6.0)), Integer(2), Integer(3));
    std::vector<double> expected = {1.0, 3.0, 5.0, 2.0, 4.0, 6.0};
    a = transpose(m);
    check_dim(a, {3, 2});
    double* p = a.d.data();
    compare(p, expected);
  }
  // square matrix: 3x3
  {
    Array<Double, Buffer<Double>> a;
    // column-major 3x3: [1 4 7 / 2 5 8 / 3 6 9]; transpose -> {1,4,7,2,5,8,3,6,9}
    Array<Double, Buffer<Double>> m = matrix(colon(Double(1.0), Double(9.0)), Integer(3), Integer(3));
    std::vector<double> expected = {1.0, 4.0, 7.0, 2.0, 5.0, 8.0, 3.0, 6.0, 9.0};
    a = transpose(m);
    check_dim(a, {3, 3});
    double* p = a.d.data();
    compare(p, expected);
  }
  // double transpose returns the original (values and dimension)
  {
    Array<Double, Buffer<Double>> a;
    Array<Double, Buffer<Double>> m = matrix(colon(Double(1.0), Double(6.0)), Integer(2), Integer(3));
    std::vector<double> expected = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
    a = transpose(transpose(m));
    check_dim(a, {2, 3});
    double* p = a.d.data();
    compare(p, expected);
  }
  // scalar cannot be transposed
  {
    bool catched = false;
    try {
      Array<Double, Buffer<Double>> a = transpose(Double(3.0));
    }
    catch (const std::exception& e) {
      catched = true;
      const std::string expected_message = "You cannot transpose a scalar value";
      ass<"transpose scalar message">(std::strcmp(e.what(), expected_message.c_str()) == 0);
    }
    ass<"Did not throw">(catched);
  }
}
