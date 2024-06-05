#include <stdexcept>
#define STANDALONE_ETR

// TODO: write tests!!!
#include "../include/etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void isNa_tests() {
  // NOTE: testing isNA
  {
    std::string s = "isNaTests: ";
    Vec<double> v = vector_numeric(3);
    v[0] = NA;
    v[1] = Inf;
    ass(isNA(v[0]), s + "true");
    ass(!isNA(v[1]), s + "false");
    ass(!isNA(v[2]), s + "false");
    ass(isNA(NA), s + "NA");
    ass(!isNA(3.14), s + "double");
    ass(!isNA(14), s + "double");
    ass(!isNA(Inf), s + "double");
    ass(!isNA(-Inf), s + "double");
  }
}

// [[Rcpp::export]]
void isInf_tests() {
  // NOTE: testing isInf
  {
    std::string s = "isInfTests: ";
    Vec<double> v = vector_numeric(3);
    v[0] = NA;
    v[1] = Inf;
    ass(!isInf(v[0]), s + "false");
    ass(isInf(v[1]), s + "true");
    ass(!isInf(v[2]), s + "true");
    ass(!isInf(NA), s + "NA");
    ass(!isInf(3.14), s + "double");
    ass(!isInf(14), s + "double");
    ass(isInf(Inf), s + "double");
    ass(isInf(-Inf), s + "double");
  }
}

// [[Rcpp::export]]
void length_tests() {
  // NOTE: test lengthTests
  {
    std::string s = "lengthTests: ";
    Vec<double> v = vector_numeric(40);
    ass(v.size() == 40, s + "vector_numeric(40)");
    ass(vector_numeric(40).size() == 40, s + "vector_numeric(40)");
    ass(vector_numeric(400000).size() == 400000, s + "vector_numeric(400000)");
    ass(vector_numeric(1).size() == 1, s + "vector_numeric(1)");
    Vec<double> a = coca(1, 2, 3);
    Vec<double> b = coca(4, 5);
    ass((a + b).size() == 3, s + "a + b");
    ass((a * b).size() == 3, s + "a * b");
    ass((a * 3).size() == 3, s + "a * 3");
    ass(length(1) == 1, s + "arithmetic");
    ass(length(true) == 1, s + "arithmetic");
    ass(length(NA) == 1, s + "arithmetic");
    ass(length(Inf) == 1, s + "arithmetic");
    ass(length(-Inf) == 1, s + "arithmetic");
  }
}

// [[Rcpp::export]]
void dim_tests() {
  // NOTE: test dim tests
  {
    std::string s = "dimTests: ";
    Vec<double> m = matrix(3, 4, 4);
    Vec<int> expected(SI{2});
    expected[0] = 4;
    expected[1] = 4;
    ass(dim(m)[0] == expected[0], s + "matrix");
    ass(dim(m)[1] == expected[1], s + "matrix");

    m = matrix(3, 2, 4);
    expected[0] = 2;
    expected[1] = 4;
    ass(dim(m)[0] == expected[0], s + "matrix");
    ass(dim(m)[1] == expected[1], s + "matrix");

    try {
      dim(1);
    } catch (std::runtime_error &e) {
      std::string expected = "dim can only be called with matrix";
      ass(e.what() == expected, s + "arithmetic");
    }

    try {
      dim(coca(1, 2, 3));
    } catch (std::runtime_error &e) {
      std::string expected = "dim can only be called with matrix";
      ass(e.what() == expected, s + "r variable");
    }

    ass(dim(matrix(3, 4, 2))[0] == 4, s + "r matrix");
    ass(dim(matrix(3, 4, 2))[1] == 2, s + "r matrix");
  }
}
