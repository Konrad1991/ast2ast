#include "etr.hpp"
#include <stdexcept>
using namespace etr;
#include <cstdint>
#include <limits>
#include <sstream>

// [[Rcpp::export]]
void test_numeric() {
  {
    Vec<double> vec1; vec1 = numeric(5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    int s = 5;
    Vec<double> vec2; vec2 = numeric(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
    Vec<double> vec3; vec3 = numeric(50000);
    ass(vec3.size() == 50000, "Vector should be of size 50000");
    Vec<double> vec4; vec4 = numeric(500000);
    ass(vec4.size() == 500000, "Vector should be of size 500000");
    Vec<double> vec5; vec5 = numeric(5000000);
    ass(vec5.size() == 5000000, "Vector should be of size 5000000");
  }
  {
    Vec<double> vec1; vec1 = numeric(5.0);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.0;
    Vec<double> vec2; vec2 = numeric(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    Vec<double> vec1; vec1 = numeric(5.5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.5;
    Vec<double> vec2; vec2 = numeric(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    try {
      Vec<double> vec1; vec1 = numeric(0);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1; vec1 = numeric(-1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1; vec1 = numeric(0.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1; vec1 = numeric(-1.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
}

// [[Rcpp::export]]
void test_int() {
  {
    Vec<double> vec1; vec1 = integer(5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    int s = 5;
    Vec<double> vec2; vec2 = integer(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
    Vec<double> vec3; vec3 = integer(50000);
    ass(vec3.size() == 50000, "Vector should be of size 50000");
    Vec<double> vec4; vec4 = integer(500000);
    ass(vec4.size() == 500000, "Vector should be of size 500000");
    Vec<double> vec5; vec5 = integer(5000000);
    ass(vec5.size() == 5000000, "Vector should be of size 5000000");
  }
  {
    Vec<double> vec1; vec1 = integer(5.0);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.0;
    Vec<double> vec2; vec2 = integer(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    Vec<double> vec1; vec1 = integer(5.5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.5;
    Vec<double> vec2; vec2 = integer(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    try {
      Vec<double> vec1; vec1 = integer(0);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1; vec1 = integer(-1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1; vec1 = integer(0.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1; vec1 = integer(-1.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1; vec1 = integer(false);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    Vec<double> vec2; vec2 = integer(true);
    ass(vec2.size() == 1, "Vector should be of size 1");
  }
}

// [[Rcpp::export]]
void test_bool() {
  {
    Vec<double> vec1; vec1 = logical(5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    int s = 5;
    Vec<double> vec2; vec2 = logical(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
    Vec<double> vec3; vec3 = logical(50000);
    ass(vec3.size() == 50000, "Vector should be of size 50000");
    Vec<double> vec4; vec4 = logical(500000);
    ass(vec4.size() == 500000, "Vector should be of size 500000");
    Vec<double> vec5; vec5 = logical(5000000);
    ass(vec5.size() == 5000000, "Vector should be of size 5000000");
  }
  {
    Vec<double> vec1; vec1 = logical(5.0);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.0;
    Vec<double> vec2; vec2 = logical(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    Vec<double> vec1; vec1 = logical(5.5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.5;
    Vec<double> vec2; vec2 = logical(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    try {
      Vec<double> vec1; vec1 = logical(0);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1; vec1 = logical(-1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1; vec1 = logical(0.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1; vec1 = logical(-1.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1; vec1 = logical(false);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    Vec<double> vec2; vec2 = logical(true);
    ass(vec2.size() == 1, "Vector should be of size 1");
  }
}

// [[Rcpp::export]]
void test_vector() {
  {
    Vec<double> v(SI{1});
    v[0] = 10;
    Vec<double> vec1; vec1 = numeric(v);
    ass(vec1.size() == 10, "Vector should be of size 10");
  }
  {
    Vec<double> v(SI{3});
    v[0] = 10;
    try {
      numeric(v);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid length argument";
      ass(e.what() == expectedMessage,
          "test vector as length argument with size > 1");
    }
  }
  {
    Vec<double> v(SI{3});
    v[0] = -10;
    try {
      numeric(v);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid length argument";
      ass(e.what() == expectedMessage,
          "test vector as length argument with size > 1");
    }
  }
}
