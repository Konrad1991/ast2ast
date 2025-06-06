#include <stdexcept>

// TODO: write tests!!!
#include "etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_isNA() {
  // NOTE: testing isNA
  {
    std::string s = "isNaTests: ";
    Vec<double> v;
    v = numeric(3);
    v[0] = NA;
    v[1] = Inf;
    ass(isNA(v[0]), std::string(s) + "NA");
    ass(!isNA(v[1]), std::string(s) + "Inf");
    ass(!isNA(v[2]), std::string(s) + "0.0");
    ass(isNA(NA), std::string(s) + "NA");
    ass(!isNA(3.14), std::string(s) + "3.14");
    ass(!isNA(14), std::string(s) + "14");
    ass(!isNA(Inf), std::string(s) + "Inf");
    ass(!isNA(-Inf), std::string(s) + "-Inf");
  }
}

// [[Rcpp::export]]
void test_isFinite() {
  // NOTE: testing isFinite
  {
    std::string s = "isFiniteTests: ";
    Vec<double> v;
    v = numeric(3);
    v[0] = NA;
    v[1] = Inf;
    ass(!isFinite(v[0]), std::string(s) + "NA");
    ass(!isFinite(v[1]), std::string(s) + "Inf");
    ass(isFinite(v[2]), std::string(s) + "0.0");
    ass(!isFinite(NA), std::string(s) + "NA");
    ass(isFinite(3.14), std::string(s) + "3.14");
    ass(isFinite(14), std::string(s) + "14");
    ass(!isFinite(Inf), std::string(s) + "Inf");
    ass(!isFinite(-Inf), std::string(s) + "-Inf");
  }
}
// [[Rcpp::export]]
void test_isInfinite() {
  // NOTE: testing isInfinite
  {
    std::string s = "isInfiniteTests: ";
    Vec<double> v;
    v = numeric(3);
    v[0] = NA;
    v[1] = Inf;
    ass(!isInfinite(v[0]), std::string(s) + "NA");
    ass(isInfinite(v[1]), std::string(s) + "Inf");
    ass(!isInfinite(v[2]), std::string(s) + "0.0");
    ass(!isInfinite(NA), std::string(s) + "NA");
    ass(!isInfinite(3.14), std::string(s) + "3.14");
    ass(!isInfinite(14), std::string(s) + "14");
    ass(isInfinite(Inf), std::string(s) + "Inf");
    ass(isInfinite(-Inf), std::string(s) + "-Inf");
  }
}

// [[Rcpp::export]]
void test_length() {
  // NOTE: test lengthTests
  {
    std::string s = "lengthTests: ";
    Vec<double> v;
    v = numeric(40);
    ass(v.size() == 40, std::string(s) + "numeric(40)");
    ass(numeric(40).size() == 40, std::string(s) + "numeric(40)");
    ass(numeric(400000).size() == 400000, std::string(s) + "numeric(400000)");
    ass(numeric(1).size() == 1, std::string(s) + "numeric(1)");
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> b; b = c(4, 5);
    ass((a + b).size() == 3, std::string(s) + "a + b");
    ass((a * b).size() == 3, std::string(s) + "a * b");
    ass((a * 3).size() == 3, std::string(s) + "a * 3");
    ass(length(1) == 1, std::string(s) + "arithmetic");
    ass(length(true) == 1, std::string(s) + "arithmetic");
    ass(length(NA) == 1, std::string(s) + "arithmetic");
    ass(length(Inf) == 1, std::string(s) + "arithmetic");
    ass(length(-Inf) == 1, std::string(s) + "arithmetic");
  }
}

// [[Rcpp::export]]
void test_dim() {
  // NOTE: test dim tests
  {
    std::string s = "dimTests: ";
    Mat<double> m;
    m = matrix(3, 4, 4);
    Vec<int> expected(SI{2});
    expected[0] = 4;
    expected[1] = 4;
    ass(dim(m)[0] == expected[0], std::string(s) + "matrix");
    ass(dim(m)[1] == expected[1], std::string(s) + "matrix");

    m = matrix(3, 2, 4);
    expected[0] = 2;
    expected[1] = 4;
    ass(dim(m)[0] == expected[0], std::string(s) + "matrix");
    ass(dim(m)[1] == expected[1], std::string(s) + "matrix");

    ass(dim(matrix(3, 4, 2))[0] == 4, std::string(s) + "r matrix");
    ass(dim(matrix(3, 4, 2))[1] == 2, std::string(s) + "r matrix");
  }
}
