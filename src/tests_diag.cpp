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

  // Scalar
  {
    Array<Double, Buffer<Double>> a;
    std::vector<double> expected = {1.0, 0.0, 0.0, 1.0};
    a = diag(Integer(2));
    check_dim(a, {2, 2});
    double* p = a.d.data();
    compare(p, expected);
  }
  // Vector of length 1
  {
    Array<Double, Buffer<Double>> a;
    std::vector<double> expected = {1.0, 0.0, 0.0, 1.0};
    a = diag(c(Integer(2)));
    check_dim(a, {2, 2});
    double* p = a.d.data();
    compare(p, expected);
  }
  // Vector length > 1
  {
    Array<Double, Buffer<Double>> a;
    std::vector<double> expected = {2.0, 0.0, 0.0, 3.0};
    a = diag(c(Integer(2), Integer(3)));
    check_dim(a, {2, 2});
    double* p = a.d.data();
    compare(p, expected);
  }
  // two scalars
  {
    Array<Double, Buffer<Double>> a;
    const double pi = 3.1415;
    std::vector<double> expected = {
      pi, 0.0, 0.0,
      0.0 , pi, 0.0,
      0.0 , 0.0, pi
    };
    a = diag(Double(pi), Integer(3));
    check_dim(a, {3, 3});
    double* p = a.d.data();
    compare(p, expected);
  }
  // array & scalar
  {
    Array<Double, Buffer<Double>> a;
    Array<Double, Buffer<Double>> r = colon(Double(1.0), Double(12.0));
    const std::size_t ncol = 15;
    std::vector<double> expected(225);
    std::vector<double> diagonal = {
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3
    };
    std::size_t counter = 0;
    for (std::size_t c = 0; c < ncol; c++) {
      for (std::size_t r = 0; r < ncol; r++) {
        if (c == r) {
          expected[c * ncol + r] = diagonal[counter];
          counter++;
        }
      }
    }
    a = diag(r, Integer(15));
    check_dim(a, {15, 15});
    double* p = a.d.data();
    compare(p, expected);
  }
  // scalar & array
  {
    Array<Double, Buffer<Double>> a;
    const double pi = 3.1415;
    std::vector<double> expected = {
      pi, 0.0, 0.0, 0.0,
      0.0 , pi, 0.0, 0.0,
      0.0 , 0.0, pi, 0.0,
      0.0 , 0.0, 0.0, pi
    };
    a = diag(Double(pi), c(Integer(4)));
    check_dim(a, {4, 4});
    double* p = a.d.data();
    compare(p, expected);

    // array size > 1
    bool catched = false;
    try {
      a = diag(Double(pi), c(Integer(2), Integer(4)));
    }
    catch (const std::exception& e) {
      catched = true;
      const std::string expected_message = "found invalid dimension argument to diag. Expected a vector with length 1L";
      ass<"allocating diag with array size > 1">(std::strcmp(e.what(), expected_message.c_str()) == 0);
    }
    ass<"Did not throw">(catched);
  }
  // array & array
  {
    Array<Double, Buffer<Double>> a;
    const double pi = 3.1415;
    std::vector<double> expected = {
      pi, 0.0, 0.0, 0.0,
      0.0 , pi, 0.0, 0.0,
      0.0 , 0.0, pi, 0.0,
      0.0 , 0.0, 0.0, pi
    };
    a = diag(rep(Double(pi), Integer(4)), c(Integer(4)));
    check_dim(a, {4, 4});
    double* p = a.d.data();
    compare(p, expected);

    // array size > 1
    bool catched = false;
    try {
      a = diag(c(Double(pi)), c(Integer(2), Integer(4)));
    }
    catch (const std::exception& e) {
      catched = true;
      const std::string expected_message = "found invalid dimension argument to diag. Expected a vector with length 1L";
      ass<"allocating diag with array size > 1">(std::strcmp(e.what(), expected_message.c_str()) == 0);
    }
    ass<"Did not throw">(catched);
  }
  // matrix -> extract diagonal (square)
  {
    Array<Double, Buffer<Double>> a;
    // column-major 3x3: [1 4 7 / 2 5 8 / 3 6 9], diagonal = 1, 5, 9
    Array<Double, Buffer<Double>> m = matrix(colon(Double(1.0), Double(9.0)), Integer(3), Integer(3));
    std::vector<double> expected = {1.0, 5.0, 9.0};
    a = diag(m);
    check_dim(a, {3});
    double* p = a.d.data();
    compare(p, expected);
  }
  // matrix -> extract diagonal (non-square, length = min(nrow, ncol))
  {
    Array<Double, Buffer<Double>> a;
    // column-major 2x3: [1 3 5 / 2 4 6], diagonal = 1, 4
    Array<Double, Buffer<Double>> m = matrix(colon(Double(1.0), Double(6.0)), Integer(2), Integer(3));
    std::vector<double> expected = {1.0, 4.0};
    a = diag(m);
    check_dim(a, {2});
    double* p = a.d.data();
    compare(p, expected);
  }
  // matrix -> extract diagonal (1x1: a matrix, so extract -> length-1 vector, not identity)
  {
    Array<Double, Buffer<Double>> a;
    Array<Double, Buffer<Double>> m = matrix(Double(3.1415), Integer(1), Integer(1));
    std::vector<double> expected = {3.1415};
    a = diag(m);
    check_dim(a, {1});
    double* p = a.d.data();
    compare(p, expected);
  }
}
