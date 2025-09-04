#include "etr.hpp"
#include <stdexcept>
using namespace etr;

/*
// NOTE: compilation fails as desired
void test_scalar_subsetting() {
  bool b = true;
  int i = 10;
  double d = 3.14;
  subset(b, 1);
  subset(b, 1, 1);
  subset(i, 1);
  subset(i, 1, 1);
  subset(d, 1);
  subset(d, 1, 1);
}
*/

// [[Rcpp::export]]
void test_subset_with_scalar() {
  Vec<int> l; l = c(1, 2, 3, 4);
  int scalar = 2;
  auto result1 = subset(l, scalar);
  static_assert(std::is_same_v<decltype(result1[0]), int&>);
  auto result2 = subset(l, 2.4);
  static_assert(std::is_same_v<decltype(result2[0]), int&>);
  ass<"Expected 2">(result1 == 2);
  ass<"Expected 2">(result2 == 2);
  auto result3 = subset(l, true);
  ass<"Expected length 4">(result3.size() == 4);
  auto result4 = subset(l, false);
  ass<"Expected length 4">(result4.size() == 0);
  try {
    auto result = subset(l, 0);
  } catch (Rcpp::exception &e) {
    std::string expected = "Negative indices are not supported";
    ass(e.what() == expected, "Expected out of boundary error");
  }
  try {
    auto result = subset(l, 0.4);
  } catch (Rcpp::exception &e) {
    std::string expected = "Negative indices are not supported";
    ass(e.what() == expected, "Expected out of boundary error");
  }
}

// [[Rcpp::export]]
void test_vector_subsetting() {
  Vec<double> v; v = colon(1, 16);
  // NOTE: boolsubsetting
  {
    std::string s = "BoolSubsetting: ";
    ass(subset(v, true).size() == 16, std::string(s) + "vec size == 16");
    bool b = true;
    ass(subset(v, b).size() == 16, std::string(s) + "vec size == 16");
    try {
      subset(v, false);
    } catch (Rcpp::exception &e) {
      std::string expect = "Variable[FALSE] subsetting is not supported. Sorry";
      ass(expect == e.what(), std::string(s) + "Variable[FALSE] results in runtime error");
    }
    try {
      b = false;
      subset(v, b);
    } catch (Rcpp::exception &e) {
      std::string expect = "Variable[FALSE] subsetting is not supported. Sorry";
      ass(expect == e.what(), std::string(s) + "Variable[FALSE] results in runtime error");
    }
  }
  {
    std::string s = "BoolSubsetting: ";
    ass(subset(colon(1, 16), true).size() == 16, std::string(s) + "vec size == 16");
    try {
      subset(colon(1, 16), false);
    } catch (Rcpp::exception &e) {
      std::string expect = "Variable[FALSE] subsetting is not supported. Sorry";
      ass(expect == e.what(), std::string(s) + "Variable[FALSE] results in runtime error");
    }
    try {
      bool b = false;
      subset(colon(1, 5), b);
    } catch (Rcpp::exception &e) {
      std::string expect = "Variable[FALSE] subsetting is not supported. Sorry";
      ass(expect == e.what(), std::string(s) + "Variable[FALSE] results in runtime error");
    }
  }
  // NOTE: int subsetting
  {
    std::string s = "IntSubsetting: ";
    try {
      subset(v,-1);
    } catch (Rcpp::exception &e) {
      std::string expect = "Negative indices are not supported";
      ass(expect == e.what(), std::string(s) + "Invalid index argument");
    }
    ass(subset(v, 1).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(v, 1)[0] == 1, std::string(s) + "vec[1] == 1");
    ass(subset(v, 16)[0] == 16, std::string(s) + "vec[16] == 16");
    int i = 1;
    ass(subset(v, i).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(v, i)[0] == 1, std::string(s) + "vec[1] == 1");
    i = 16;
    ass(subset(v, i)[0] == 16, std::string(s) + "vec[16] == 16");
  }
  {
    std::string s = "IntSubsetting: ";
    try {
      subset(c(1, 2, 3), -1);
    } catch (Rcpp::exception &e) {
      std::string expect = "Negative indices are not supported";
      ass(expect == e.what(), std::string(s) + "Invalid index argument");
    }
    ass(subset(c(1, 2, 3), 1).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(c(1, 2, 3), 2) == 2, std::string(s) + "vec[2] == 2");
    ass(subset(c(1, 2, 3), 3) == 3, std::string(s) + "vec[3] == 3");
    int i = 1;
    ass(subset(c(1, 2, 3), i).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(c(1, 2, 3), i) == 1, std::string(s) + "vec[1] == 1");
    i = 16;
    ass(subset(colon(1, 16), i) == 16, std::string(s) + "vec[16] == 16");
  }
  // NOTE: double subsetting
  {
    std::string s = "DoubleSubsetting 1: ";
    try {
      subset(v, -1.0);
    } catch (Rcpp::exception &e) {
      ass(expect == e.what(), std::string(s) + "invalid index argument");
    }

    ass(subset(v, 1.0).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(v, 1.0)[0] == 1, std::string(s) + "vec[1] == 1");
    ass(subset(v,16.0)[0] == 16, std::string(s) + "vec[16] == 16");
    double d = 1.0;
    ass(subset(v, d).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(v, d)[0] == 1, std::string(s) + "vec[1] == 1");
    d = 16;
    ass(subset(v, d)[0] == 16, std::string(s) + "vec[16] == 16");

    ass(subset(v, 1.1).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(v, 1.1)[0] == 1, std::string(s) + "vec[1] == 1");
    ass(subset(v, 16.1)[0] == 16, std::string(s) + "vec[16] == 16");
    d = 1.1;
    ass(subset(v, d).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(v, d)[0] == 1, std::string(s) + "vec[1] == 1");
    d = 16;
    ass(subset(v, d)[0] == 16, std::string(s) + "vec[16] == 16");
  }
  {
    std::string s = "DoubleSubsetting 2: ";
    try {
      subset(v,-1.0);
    } catch (Rcpp::exception &e) {
      std::string expect = "invalid index argument";
      ass(expect == e.what(), std::string(s) + "invalid index argument");
    }

    ass(subset(v,1.0).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(v, 1.0)[0] == 1, std::string(s) + "vec[1] == 1");
    ass(subset(v, 16.0)[0] == 16, std::string(s) + "vec[16] == 16");
    double d = 1.0;
    ass(subset(v, d).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(v, d)[0] == 1, std::string(s) + "vec[1] == 1");
    d = 16;
    ass(subset(v, d)[0] == 16, std::string(s) + "vec[16] == 16");

    ass(subset(v, 1.1).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(v, 1.1)[0] == 1, std::string(s) + "vec[1] == 1");
    ass(subset(v, 16.1)[0] == 16, std::string(s) + "vec[16] == 16");
    d = 1.1;
    ass(subset(v, d).size() == 1, std::string(s) + "vec(1).size() == 1");
    ass(subset(v, d)[0] == 1, "vec[1] == 1");
    d = 16;
    ass(subset(v, d)[0] == 16, std::string(s) + "vec[16] == 16");
  }
  // NOTE: vector<double|int>
  {
    std::string s = "Vec<double|int>: ";
    int size = 3;
    double *d = new double[size];
    Vec<double, Borrow<double>> b(d, size);
    b = c(1, 2, 3);
    Vec<double> idx; idx = c(1, 4, 7);
    ass(subset(v, idx).size() == 3, std::string(s) + "vec size == 3");
    ass(subset(v, c(1, 2, 3))[1] == 2, std::string(s) + "vec(c(1, 2, 3)[2] == 2)");
    ass(subset(v, subset(v, 1)).size() == 1, std::string(s) + "vec size = 1");
    ass(subset(v, c(1, 2, 3) + 1)[1] == 3, std::string(s) + "vec[c(1, 2, 3) + 1][2] == 3");
    ass(subset(v, idx + 1)[1] == 5, std::string(s) + "vec[idx + 1][2] == 5");
    ass(subset(v,b).size() == 3, std::string(s) + "vec[b] size == 3");
    ass(subset(v, b + b).size() == 3, std::string(s) + "vec[b+b] size == 3");
    delete[] d;
  }
  // NOTE: vector<bool>
  {
    std::string s = "Vec<bool>: ";
    Vec<bool> b(SI{4});
    b[0] = true;
    b[1] = false;
    b[2] = true;
    b[3] = true;
    ass(subset(v, b).size() == 3, std::string(s) + "vec[b] == 12");
  }
}

// [[Rcpp::export]]
void test_matrix_subsetting() {
  Mat<double> m(4, 4); m = matrix(colon(1, 16), 4, 4);
  // NOTE: matrix bool + bool|int|double
  {
    std::string s = "MatrixBoolScalarSubsetting: ";
    Vec<int> idx; idx = c(1, 5, 8);
    ass(subset(m, true, true).size() == 16, std::string(s) + "vec[T, T]");
    ass(subset(m, true, 1).size() == 4, std::string(s) + "vec[T, 1]");
    ass(subset(m, true, 1.1).size() == 4, std::string(s) + "vec[T, 1.1]");
    ass(subset(m, true, c(1, 2, 3) + 1).size() == 12, std::string(s) + "vec[T, c(1, 2, 3) + 1]");
    ass(subset(m, true, idx).size() == 12, std::string(s) + "vec[T, idx]");
  }
  {
    std::string s = "MatrixBoolScalarSubsetting: ";
    Vec<int> idx; idx = c(1, 5, 8);
    ass(subset(matrix(colon(1, 16), 4, 4), true, true).size() == 16,
        std::string(s) + "vec[T, T]");
    ass(subset(matrix(colon(1, 16), 4, 4), true, 1).size() == 4,
        std::string(s) + "vec[T, 1]");
    ass(subset(matrix(colon(1, 16), 4, 4), true, 1.1).size() == 4,
        std::string(s) + "vec[T,1.1]");
    ass(subset(matrix(colon(1, 16), 4, 4), true, c(1, 2, 3) + 1).size() ==
        12,
        std::string(s) + "vec[T, c(1, 2, 3) + 1]");
    ass(subset(matrix(colon(1, 16), 4, 4), true, idx).size() == 12,
        std::string(s) + "vec[T, idx]");
  }
  // NOTE:matrix int + bool|int|double
  {
    std::string s = "MatrixIntScalarSubsetting: ";
    Vec<int> idx; idx = c(1, 5, 8);
    ass(subset(m, 1, true).size() == 4, std::string(s) + "vec[T, 4]");
    ass(subset(m, 1, 1).size() == 1, std::string(s) + "vec[1, 1]");
    ass(subset(m, 1, 1.1).size() == 1, std::string(s) + "vec[1, 1.1]");
    ass(subset(m, 2, c(1, 2, 3) + 1).size() == 3, std::string(s) + "vec[2, c(1, 2, 3) + 1]");
    ass(subset(m, 3, idx).size() == 3, std::string(s) + "vec[T, idx]");
  }
  // NOTE:matrix double + bool|int|double
  {
    std::string s = "MatrixDoubleScalarSubsetting: ";
    Vec<int> idx; idx = c(1, 5, 8);
    ass(subset(m, 1.1, true).size() == 4, std::string(s) + "vec[T, 4]");
    ass(subset(m, 1.1, 1).size() == 1, std::string(s) + "vec[1, 1]");
    ass(subset(m, 1.1, 1.1).size() == 1, std::string(s) + "vec[1, 1.1]");
    ass(subset(m, 2.1, c(1, 2, 3) + 1).size() == 3, std::string(s) + "vec[2, c(1, 2, 3) + 1]");
    ass(subset(m, 3.1, idx).size() == 3, std::string(s) + "vec[T, idx]");
  }
  // NOTE: matrix vec<bool|int|double> + bool
  {
    std::string s = "MatrixVecBoolSubsetting: ";
    Vec<bool> b(SI{4});
    b[0] = true;
    b[1] = false;
    b[2] = true;
    b[3] = true;
    ass(subset(m, b, true).size() == 12, std::string(s) + "vec[b, T]");
    Vec<int> idx; idx = c(1, 2, 4);
    ass(subset(m, idx, true).size() == 12, std::string(s) + "vec[i, T]");
    Vec<double> d; d = c(1.1, 2.2, 4.9);
    ass(subset(m, d, true).size() == 12, std::string(s) + "vec[d, T]");
  }
  // NOTE: matrix vec<bool|int|double> + int
  {
    std::string s = "MatrixVecIntSubsetting: ";
    Vec<bool> b(SI{4});
    b[0] = true;
    b[1] = false;
    b[2] = true;
    b[3] = true;
    ass(subset(m, b, 1).size() == 3, std::string(s) + "vec[b, 1]");
    Vec<int> idx; idx = c(1, 2, 4);

    ass(subset(m, idx, 1).size() == 3, std::string(s) + "vec[i, 1]");
    Vec<double> d; d = c(1.1, 2.2, 4.9);
    ass(subset(m, d, 1).size() == 3, std::string(s) + "vec[d, 1]");
  }
  // NOTE: matrix vec<bool|int|double> + double
  {
    std::string s = "MatrixVecDoubleSubsetting: ";
    Vec<bool> b(SI{4});
    b[0] = true;
    b[1] = false;
    b[2] = true;
    b[3] = true;
    ass(subset(m, b, 1.1).size() == 3, std::string(s) + "vec[b, 1.1]");
    Vec<int> idx; idx = c(1, 2, 4);
    ass(subset(m, idx, 2.2).size() == 3, std::string(s) + "vec[i, 2.2]");
    Vec<double> d; d = c(1.1, 2.2, 4.9);
    ass(subset(m, d, 3.1).size() == 3, std::string(s) + "vec[d, 3.1]");
  }
  // NOTE: matrix vec<bool|int|double> + vec<bool|int|double>
  {
    std::string s = "MatrixVecVecSubsetting: ";
    Vec<bool> b(SI{4});
    b[0] = true;
    b[1] = false;
    b[2] = true;
    b[3] = true;
    Vec<int> idx; idx = c(1, 2, 4);
    Vec<double> d; d = c(1.1, 2.2, 4.9);
    ass(subset(m, b, b).size() == 9, std::string(s) + "vec[b, b]");
    ass(subset(m, b, idx).size() == 9, std::string(s) + "vec[b, idx]");
    ass(subset(m, b, d).size() == 9, std::string(s) + "vec[b, d]");
    ass(subset(m, idx, b).size() == 9, std::string(s) + "vec[idx, b]");
    ass(subset(m, idx, d).size() == 9, std::string(s) + "vec[idx, d]");
    ass(subset(m, idx, idx).size() == 9, std::string(s) + "vec[idx, idx]");
    ass(subset(m, d, b).size() == 9, std::string(s) + "vec[d, b]");
    ass(subset(m, d, idx).size() == 9, std::string(s) + "vec[d, idx]");
    ass(subset(m, d, d).size() == 9, std::string(s) + "vec[d,d]");
  }
  {
    std::string s = "MatrixVecVecSubsetting: ";
    ass(subset(m, c(1, 2, 3), c(1, 2, 3)).size() == 9, std::string(s) + "vec[idx, d]");
    ass(subset(m, c(1, 2, 3), c(1.1, 2.2, 3.3)).size() == 9, std::string(s) + "vec[idx, idx]");
    ass(subset(m, c(1.1, 2.2, 3.3), c(1, 2, 3)).size() == 9, std::string(s) + "vec[d, idx]");
    ass(subset(m, c(1.1, 2.2, 3.3), c(1.1, 2.2, 3.3)).size() == 9,
        std::string(s) + "vec[d,d]");
    ass(subset(m, c(1, 2, 3) + c(1, 2, 3), c(1, 2, 3)).size() == 9,
        std::string(s) + "vec[idx, d]");
    ass(subset(m, c(1, 2, 3) + 4, c(1.1, 2.2, 3.3)).size() == 9,
        std::string(s) + "vec[idx, idx]");
    ass(subset(m, c(1.1, 2.2, 3.3) + c(1.1, 2.2, 3.3), c(1, 2, 3)).size() == 9,
        std::string(s) + "vec[d, idx]");
    ass(subset(m, c(1.1, 2.2, 3.3) + 5.6, c(1.1, 2.2, 3.3)).size() == 9,
        std::string(s) + "vec[d,d]");

    ass(subset(m, c(1, 2, 3), c(1, 2, 3) + 5.5).size() == 9, std::string(s) + "vec[idx,d]");
    ass(subset(m, c(1, 2, 3), c(1.1, 2.2, 3.3) + 5).size() == 9,
        std::string(s) + "vec[idx, idx]");
    ass(subset(m, c(1.1, 2.2, 3.3), c(1, 2, 3) + c(1, 2)).size() == 9,
        std::string(s) + "vec[d, idx]");

    ass(subset(m, c(1.1, 2.2, 3.3), c(1.1, 2.2, 3.3) + c(1.1, 2.2, 3.3, 4.4))
        .size() == 12,
        std::string(s) + "vec[d,d]");
  }
}

// [[Rcpp::export]]
void test_at_subsetting() {
  Vec<double> v; v = colon(1, 16);
  Mat<double> m(4, 4); m = matrix(colon(1, 16), 4, 4);
  // NOTE: at testing
  {
    std::string s = "AtSubsetting: ";
    at(v, 1) = 10.1;
    ass(at(v, 1) == 10.1, std::string(s) + "at mutable");
    try {
      at(v, 0);
    } catch (Rcpp::exception &e) {
      std::string expected = "Error: out of boundaries";
      ass(e.what() == expected, std::string(s) + "Error out of boundaries");
    }
    try {
      at(v, 2000);
    } catch (Rcpp::exception &e) {
      std::string expected = "Error: out of boundaries";
      ass(e.what() == expected, std::string(s) + "Error out of boundaries");
    }
    at(m, 2, 3) = 20.3;
    ass(at(m, 2, 3) == 20.3, std::string(s) + "at mutable matrix");
    try {
      at(m, 0, 0);
    } catch (Rcpp::exception &e) {
      std::string expected = "Error: out of boundaries";
      ass(e.what() == expected, std::string(s) + "Error out of boundaries");
    }
    try {
      at(m, 2000, 1);
    } catch (Rcpp::exception &e) {
      std::string expected = "Error: out of boundaries";
      ass(e.what() == expected, std::string(s) + "Error out of boundaries");
    }
  }
  {
    std::string s = "AtSubsetting: ";
    ass(at(c(1, 2, 3), 1), std::string(s) + "at r vec");
    ass(at(matrix(c(1, 2, 3, 4), 2, 2), 2) == 2, std::string(s) + "r matrix");
    ass(at(matrix(c(1, 2, 3, 4), 2, 2), 2, 2) == 4, std::string(s) + "r matrix");
  }
}

