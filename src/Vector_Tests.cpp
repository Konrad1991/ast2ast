#include "etr.hpp"
#include <stdexcept>
using namespace etr;
#include <cstdint>
#include <limits>
#include <sstream>

// [[Rcpp::export]]
void test_vector_numeric() {
  {
    Vec<double> vec1 = vector_numeric(5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    int s = 5;
    Vec<double> vec2 = vector_numeric(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
    Vec<double> vec3 = vector_numeric(50000);
    ass(vec3.size() == 50000, "Vector should be of size 50000");
    Vec<double> vec4 = vector_numeric(500000);
    ass(vec4.size() == 500000, "Vector should be of size 500000");
    Vec<double> vec5 = vector_numeric(5000000);
    ass(vec5.size() == 5000000, "Vector should be of size 5000000");
  }
  {
    Vec<double> vec1 = vector_numeric(5.0);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.0;
    Vec<double> vec2 = vector_numeric(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    Vec<double> vec1 = vector_numeric(5.5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.5;
    Vec<double> vec2 = vector_numeric(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    try {
      Vec<double> vec1 = vector_numeric(0);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1 = vector_numeric(-1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1 = vector_numeric(0.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1 = vector_numeric(-1.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1 = vector_numeric(false);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    Vec<double> vec2 = vector_numeric(true);
    ass(vec2.size() == 1, "Vector should be of size 1");
  }
}

// [[Rcpp::export]]
void test_vector_int() {
  {
    Vec<double> vec1 = vector_integer(5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    int s = 5;
    Vec<double> vec2 = vector_integer(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
    Vec<double> vec3 = vector_integer(50000);
    ass(vec3.size() == 50000, "Vector should be of size 50000");
    Vec<double> vec4 = vector_integer(500000);
    ass(vec4.size() == 500000, "Vector should be of size 500000");
    Vec<double> vec5 = vector_integer(5000000);
    ass(vec5.size() == 5000000, "Vector should be of size 5000000");
  }
  {
    Vec<double> vec1 = vector_integer(5.0);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.0;
    Vec<double> vec2 = vector_integer(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    Vec<double> vec1 = vector_integer(5.5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.5;
    Vec<double> vec2 = vector_integer(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    try {
      Vec<double> vec1 = vector_integer(0);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1 = vector_integer(-1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1 = vector_integer(0.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1 = vector_integer(-1.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1 = vector_integer(false);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    Vec<double> vec2 = vector_integer(true);
    ass(vec2.size() == 1, "Vector should be of size 1");
  }
}

// [[Rcpp::export]]
void test_vector_bool() {
  {
    Vec<double> vec1 = vector_logical(5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    int s = 5;
    Vec<double> vec2 = vector_logical(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
    Vec<double> vec3 = vector_logical(50000);
    ass(vec3.size() == 50000, "Vector should be of size 50000");
    Vec<double> vec4 = vector_logical(500000);
    ass(vec4.size() == 500000, "Vector should be of size 500000");
    Vec<double> vec5 = vector_logical(5000000);
    ass(vec5.size() == 5000000, "Vector should be of size 5000000");
  }
  {
    Vec<double> vec1 = vector_logical(5.0);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.0;
    Vec<double> vec2 = vector_logical(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    Vec<double> vec1 = vector_logical(5.5);
    ass(vec1.size() == 5, "Vector should be of size 5");
    double s = 5.5;
    Vec<double> vec2 = vector_logical(s);
    ass(vec1.size() == 5, "Vector should be of size 5");
  }
  {
    try {
      Vec<double> vec1 = vector_logical(0);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1 = vector_logical(-1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1 = vector_logical(0.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    try {
      Vec<double> vec1 = vector_logical(-1.1);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
  }
  {
    try {
      Vec<double> vec1 = vector_logical(false);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid times argument";
      ass(e.what() == expectedMessage, "test invalid times argument of vector");
    }
    Vec<double> vec2 = vector_logical(true);
    ass(vec2.size() == 1, "Vector should be of size 1");
  }
}

// [[Rcpp::export]]
void test_vector_vector() {
  {
    Vec<double> v(SI{1});
    v[0] = 10;
    Vec<double> vec1 = vector_numeric(v);
    ass(vec1.size() == 10, "Vector should be of size 10");
  }
  {
    Vec<double> v(SI{3});
    v[0] = 10;
    try {
      vector_numeric(v);
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
      vector_numeric(v);
    } catch (Rcpp::exception &e) {
      std::string expectedMessage = "invalid length argument";
      ass(e.what() == expectedMessage,
          "test vector as length argument with size > 1");
    }
  }
}
