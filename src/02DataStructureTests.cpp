#include "etr.hpp"
#include <cstddef>
#include <stdexcept>
#include <type_traits>
using namespace etr;

void test_vector_constructors() {
  // SI
  {
    SI s(1);
    Vec<double> v1(s);
    Vec<double> v2(SI{2});
    ass<"Vec size of 1">(v1.size() == 1);
    ass<"Vec size of 2">(v2.size() == 2);

    bool throws = false;
    try {
      const long long int size = 999999999999999999;
      Vec<double> v3(SI{size});
    } catch(...) {
      throws = true;
    }
    ass<"bad alloc large number">(throws);

    try {
      const long long int size = 0;
      Vec<double> v3(SI{size});
    } catch (const Rcpp::exception& e) {
      std::string expected = "Size has to be larger than 0!";
      ass<"Cannot allocate 0">(e.what() == std::string(expected));
    }

    try {
      const long long int size = -2;
      Vec<double> v3(SI{size});
    } catch (const Rcpp::exception& e) {
      std::string expected = "Size has to be larger than 0!";
      ass<"Neg size input">(e.what() == std::string(expected));
    }
  }

}

// [[Rcpp::export]]
void test_vector_class() {
  test_vector_constructors();
}

void test_matrix_constructors() {
  // SI
  {
    SI r(2);
    SI c(3);
    Mat<double> m1(r, c);
    ass<"Mat dim 2, 3">( (m1.size() == 6) && (m1.nr() == 2) && (m1.nc() == 3));

    bool throws = false;
    try {
      const long long int size = 999999999999999999;
      Mat<double> m2(SI{size}, SI{size});
    } catch(...) {
      throws = true;
    }
    ass<"bad alloc large number">(throws);

    try {
      const long long int size = 0;
      Mat<double> m3(SI{size}, SI{size});
    } catch (const Rcpp::exception& e) {
      std::string expected = "Size has to be larger than 0!";
      ass<"Cannot allocate 0">(e.what() == std::string(expected));
    }

    try {
      const long long int size = -2;
      Mat<double> m4(SI{size}, SI{size});
    } catch (const Rcpp::exception& e) {
      std::string expected = "Size has to be larger than 0!";
      ass<"Neg size input">(e.what() == std::string(expected));
    }
  }

}

// [[Rcpp::export]]
void test_matrix_class() {
  test_matrix_constructors();
}
