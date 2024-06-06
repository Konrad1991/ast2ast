#include <stdexcept>
#define STANDALONE_ETR
#include "../include/etr.hpp"
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
void test_vector_subsetting() {
  Vec<double> v = colon(1, 16);
  // NOTE: boolsubsetting
  {
    std::string s = "BoolSubsetting: ";
    ass(v(true).size() == 16, s + "vec size == 16");
    bool b = true;
    ass(v(b).size() == 16, s + "vec size == 16");
    try {
      v(false);
    } catch (std::runtime_error &e) {
      std::string expect = "Variable[FALSE] subsetting is not supported. Sorry";
      ass(expect == e.what(), s + "Variable[FALSE] results in runtime error");
    }
    try {
      b = false;
      v(b);
    } catch (std::runtime_error &e) {
      std::string expect = "Variable[FALSE] subsetting is not supported. Sorry";
      ass(expect == e.what(), s + "Variable[FALSE] results in runtime error");
    }
  }
  {
    std::string s = "BoolSubsetting: ";
    ass(subset(colon(1, 16), true).size() == 16, s + "vec size == 16");
    try {
      subset(colon(1, 16), false);
    } catch (std::runtime_error &e) {
      std::string expect = "Variable[FALSE] subsetting is not supported. Sorry";
      ass(expect == e.what(), s + "Variable[FALSE] results in runtime error");
    }
    try {
      bool b = false;
      subset(colon(1, 5), b);
    } catch (std::runtime_error &e) {
      std::string expect = "Variable[FALSE] subsetting is not supported. Sorry";
      ass(expect == e.what(), s + "Variable[FALSE] results in runtime error");
    }
  }
  // NOTE: int subsetting
  {
    std::string s = "IntSubsetting: ";
    try {
      v(-1);
    } catch (std::runtime_error &e) {
      std::string expect = "Invalid index argc";
      ass(expect == e.what(), s + "Invalid index argument");
    }
    ass(v(1).size() == 1, s + "vec(1).size() == 1");
    ass(v(1)[0] == 1, s + "vec[1] == 1");
    ass(v(16)[0] == 16, s + "vec[16] == 16");
    int i = 1;
    ass(v(i).size() == 1, s + "vec(1).size() == 1");
    ass(v(i)[0] == 1, s + "vec[1] == 1");
    i = 16;
    ass(v(i)[0] == 16, s + "vec[16] == 16");
  }
  {
    std::string s = "IntSubsetting: ";
    try {
      subset(coca(1, 2, 3), -1);
    } catch (std::runtime_error &e) {
      std::string expect = "Invalid index argc";
      ass(expect == e.what(), s + "Invalid index argument");
    }
    ass(subset(coca(1, 2, 3), 1).size() == 1, s + "vec(1).size() == 1");
    ass(subset(coca(1, 2, 3), 2) == 2, s + "vec[2] == 2");
    ass(subset(coca(1, 2, 3), 3) == 3, s + "vec[3] == 3");
    int i = 1;
    ass(subset(coca(1, 2, 3), i).size() == 1, s + "vec(1).size() == 1");
    ass(subset(coca(1, 2, 3), i) == 1, s + "vec[1] == 1");
    i = 16;
    ass(subset(colon(1, 16), i) == 16, s + "vec[16] == 16");
  }

  // NOTE: double subsetting
  {
    std::string s = "DoubleSubsetting: ";
    try {
      v(-1.0);
    } catch (std::runtime_error &e) {
      std::string expect = "invalid index argument";
      ass(expect == e.what(), s + "invalid index argument");
    }

    ass(v(1.0).size() == 1, s + "vec(1).size() == 1");
    ass(v(1.0)[0] == 1, s + "vec[1] == 1");
    ass(v(16.0)[0] == 16, s + "vec[16] == 16");
    double d = 1.0;
    ass(v(d).size() == 1, s + "vec(1).size() == 1");
    ass(v(d)[0] == 1, s + "vec[1] == 1");
    d = 16;
    ass(v(d)[0] == 16, s + "vec[16] == 16");

    ass(v(1.1).size() == 1, s + "vec(1).size() == 1");
    ass(v(1.1)[0] == 1, s + "vec[1] == 1");
    ass(v(16.1)[0] == 16, s + "vec[16] == 16");
    d = 1.1;
    ass(v(d).size() == 1, s + "vec(1).size() == 1");
    ass(v(d)[0] == 1, s + "vec[1] == 1");
    d = 16;
    ass(v(d)[0] == 16, s + "vec[16] == 16");
  }
  {
    std::string s = "DoubleSubsetting: ";
    try {
      v(-1.0);
    } catch (std::runtime_error &e) {
      std::string expect = "invalid index argument";
      ass(expect == e.what(), s + "invalid index argument");
    }

    ass(v(1.0).size() == 1, s + "vec(1).size() == 1");
    ass(v(1.0)[0] == 1, s + "vec[1] == 1");
    ass(v(16.0)[0] == 16, s + "vec[16] == 16");
    double d = 1.0;
    ass(v(d).size() == 1, s + "vec(1).size() == 1");
    ass(v(d)[0] == 1, s + "vec[1] == 1");
    d = 16;
    ass(v(d)[0] == 16, s + "vec[16] == 16");

    ass(v(1.1).size() == 1, s + "vec(1).size() == 1");
    ass(v(1.1)[0] == 1, s + "vec[1] == 1");
    ass(v(16.1)[0] == 16, s + "vec[16] == 16");
    d = 1.1;
    ass(v(d).size() == 1, s + "vec(1).size() == 1");
    ass(v(d)[0] == 1, "vec[1] == 1");
    d = 16;
    ass(v(d)[0] == 16, s + "vec[16] == 16");
  }
  // NOTE: vector<double|int>
  {
    std::string s = "Vec<double|int>: ";
    int size = 3;
    double *d = new double[size];
    Vec<double, Borrow<double>> b(d, size);
    b = coca(1, 2, 3);
    Vec<double> idx = coca(1, 4, 7);
    ass(v(idx).size() == 3, s + "vec size == 3");
    ass(v(coca(1, 2, 3))[1] == 2, s + "vec(c(1, 2, 3)[2] == 2)");
    ass(v(v(1)).size() == 1, s + "vec size = 1");
    ass(v(coca(1, 2, 3) + 1)[1] == 3, s + "vec[c(1, 2, 3) + 1][2] == 3");
    ass(v(idx + 1)[1] == 5, s + "vec[idx + 1][2] == 5");
    ass(v(b).size() == 3, s + "vec[b] size == 3");
    ass(v(b + b).size() == 3, s + "vec[b+b] size == 3");
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
    ass(v(b).size() == 12, s + "vec[b] == 12");
  }
  // NOTE: matrix bool + bool|int|double
  {
    std::string s = "MatrixBoolScalarSubsetting: ";
    v.d.setMatrix(true, 4, 4);
    Vec<int> idx = coca(1, 5, 8);
    ass(v(true, true).size() == 16, s + "vec[T, T]");
    ass(v(true, 1).size() == 4, s + "vec[T, 1]");
    ass(v(true, 1.1).size() == 4, s + "vec[T, 1.1]");
    ass(v(true, coca(1, 2, 3) + 1).size() == 12, s + "vec[T, c(1, 2, 3) + 1]");
    ass(v(true, idx).size() == 12, s + "vec[T, idx]");
  }
  {
    std::string s = "MatrixBoolScalarSubsetting: ";
    Vec<int> idx = coca(1, 5, 8);
    ass(subset(matrix(colon(1, 16), 4, 4), true, true).size() == 16,
        s + "vec[T, T]");
    ass(subset(matrix(colon(1, 16), 4, 4), true, 1).size() == 4,
        s + "vec[T, 1]");
    ass(subset(matrix(colon(1, 16), 4, 4), true, 1.1).size() == 4,
        s + "vec[T,1.1]");
    ass(subset(matrix(colon(1, 16), 4, 4), true, coca(1, 2, 3) + 1).size() ==
            12,
        s + "vec[T, c(1, 2, 3) + 1]");
    ass(subset(matrix(colon(1, 16), 4, 4), true, idx).size() == 12,
        s + "vec[T, idx]");
  }

  // NOTE:matrix int + bool|int|double
  {
    std::string s = "MatrixIntScalarSubsetting: ";
    v.d.setMatrix(true, 4, 4);
    Vec<int> idx = coca(1, 5, 8);
    ass(v(1, true).size() == 4, s + "vec[T, 4]");
    ass(v(1, 1).size() == 1, s + "vec[1, 1]");
    ass(v(1, 1.1).size() == 1, s + "vec[1, 1.1]");
    ass(v(2, coca(1, 2, 3) + 1).size() == 3, s + "vec[2, c(1, 2, 3) + 1]");
    ass(v(3, idx).size() == 3, s + "vec[T, idx]");
  }
  // NOTE:matrix double + bool|int|double
  {
    std::string s = "MatrixDoubleScalarSubsetting: ";
    v.d.setMatrix(true, 4, 4);
    Vec<int> idx = coca(1, 5, 8);
    ass(v(1.1, true).size() == 4, s + "vec[T, 4]");
    ass(v(1.1, 1).size() == 1, s + "vec[1, 1]");
    ass(v(1.1, 1.1).size() == 1, s + "vec[1, 1.1]");
    ass(v(2.1, coca(1, 2, 3) + 1).size() == 3, s + "vec[2, c(1, 2, 3) + 1]");
    ass(v(3.1, idx).size() == 3, s + "vec[T, idx]");
  }
  // NOTE: matrix vec<bool|int|double> + bool
  {
    std::string s = "MatrixVecBoolSubsetting: ";
    v.d.setMatrix(true, 4, 4);
    Vec<bool> b(SI{4});
    b[0] = true;
    b[1] = false;
    b[2] = true;
    b[3] = true;
    ass(v(b, true).size() == 12, s + "vec[b, T]");
    Vec<int> idx = coca(1, 2, 4);
    ass(v(idx, true).size() == 12, s + "vec[i, T]");
    Vec<double> d = coca(1.1, 2.2, 4.9);
    ass(v(d, true).size() == 12, s + "vec[d, T]");
  }
  // NOTE: matrix vec<bool|int|double> + int
  {
    std::string s = "MatrixVecIntSubsetting: ";
    v.d.setMatrix(true, 4, 4);
    Vec<bool> b(SI{4});
    b[0] = true;
    b[1] = false;
    b[2] = true;
    b[3] = true;
    ass(v(b, 1).size() == 3, s + "vec[b, 1]");
    Vec<int> idx = coca(1, 2, 4);
    ass(v(idx, 1).size() == 3, s + "vec[i, 1]");
    Vec<double> d = coca(1.1, 2.2, 4.9);
    ass(v(d, 1).size() == 3, s + "vec[d, 1]");
  }
  // NOTE: matrix vec<bool|int|double> + double
  {
    std::string s = "MatrixVecDoubleSubsetting: ";
    v.d.setMatrix(true, 4, 4);
    Vec<bool> b(SI{4});
    b[0] = true;
    b[1] = false;
    b[2] = true;
    b[3] = true;
    ass(v(b, 1.1).size() == 3, s + "vec[b, 1.1]");
    Vec<int> idx = coca(1, 2, 4);
    ass(v(idx, 2.2).size() == 3, s + "vec[i, 2.2]");
    Vec<double> d = coca(1.1, 2.2, 4.9);
    ass(v(d, 3.1).size() == 3, s + "vec[d, 3.1]");
  }
  // NOTE: matrix vec<bool|int|double> + vec<bool|int|double>
  {
    std::string s = "MatrixVecVecSubsetting: ";
    v.d.setMatrix(true, 4, 4);
    Vec<bool> b(SI{4});
    b[0] = true;
    b[1] = false;
    b[2] = true;
    b[3] = true;
    Vec<int> idx = coca(1, 2, 4);
    Vec<double> d = coca(1.1, 2.2, 4.9);
    ass(v(b, b).size() == 9, s + "vec[b, b]");
    ass(v(b, idx).size() == 9, s + "vec[b, idx]");
    ass(v(b, d).size() == 9, s + "vec[b, d]");
    ass(v(idx, b).size() == 9, s + "vec[idx, b]");
    ass(v(idx, d).size() == 9, s + "vec[idx, d]");
    ass(v(idx, idx).size() == 9, s + "vec[idx, idx]");
    ass(v(d, b).size() == 9, s + "vec[d, b]");
    ass(v(d, idx).size() == 9, s + "vec[d, idx]");
    ass(v(d, d).size() == 9, s + "vec[d,d]");
  }
  {
    // TODO: bool testing is missing as currently the binaryOperation for
    // comparison are not up to date
    std::string s = "MatrixVecVecSubsetting: ";
    v.d.setMatrix(true, 4, 4);
    ass(v(coca(1, 2, 3), coca(1, 2, 3)).size() == 9, s + "vec[idx, d]");
    ass(v(coca(1, 2, 3), coca(1.1, 2.2, 3.3)).size() == 9, s + "vec[idx, idx]");
    ass(v(coca(1.1, 2.2, 3.3), coca(1, 2, 3)).size() == 9, s + "vec[d, idx]");
    ass(v(coca(1.1, 2.2, 3.3), coca(1.1, 2.2, 3.3)).size() == 9,
        s + "vec[d,d]");
    ass(v(coca(1, 2, 3) + coca(1, 2, 3), coca(1, 2, 3)).size() == 9,
        s + "vec[idx, d]");
    ass(v(coca(1, 2, 3) + 4, coca(1.1, 2.2, 3.3)).size() == 9,
        s + "vec[idx, idx]");
    ass(v(coca(1.1, 2.2, 3.3) + coca(1.1, 2.2, 3.3), coca(1, 2, 3)).size() == 9,
        s + "vec[d, idx]");
    ass(v(coca(1.1, 2.2, 3.3) + 5.6, coca(1.1, 2.2, 3.3)).size() == 9,
        s + "vec[d,d]");

    ass(v(coca(1, 2, 3), coca(1, 2, 3) + 5.5).size() == 9, s + "vec[idx,d]");
    ass(v(coca(1, 2, 3), coca(1.1, 2.2, 3.3) + 5).size() == 9,
        s + "vec[idx, idx]");
    ass(v(coca(1.1, 2.2, 3.3), coca(1, 2, 3) + coca(1, 2)).size() == 9,
        s + "vec[d, idx]");

    ass(v(coca(1.1, 2.2, 3.3), coca(1.1, 2.2, 3.3) + coca(1.1, 2.2, 3.3, 4.4))
                .size() == 12,
        s + "vec[d,d]");
  }
  // NOTE: at testing
  {
    std::string s = "AtSubsetting: ";
    v.d.setMatrix(true, 4, 4);
    at(v, 1) = 10.1;
    ass(at(v, 1) == 10.1, s + "at mutable");
    try {
      at(v, 0);
    } catch (std::runtime_error &e) {
      std::string expected = "Error: out of boundaries --> value below 1";
      ass(e.what() == expected, s + "Error out of boundaries");
    }
    try {
      at(v, 2000);
    } catch (std::runtime_error &e) {
      std::string expected =
          "Error: out of boundaries --> value beyond size of vector";
      ass(e.what() == expected, s + "Error out of boundaries");
    }
    at(v, 2, 3) = 20.3;
    ass(at(v, 2, 3) == 20.3, s + "at mutable matrix");
    try {
      at(v, 0, 0);
    } catch (std::runtime_error &e) {
      std::string expected =
          "Error: out of boundaries --> value beyond size of vector";
      ass(e.what() == expected, s + "Error out of boundaries");
    }
    try {
      at(v, 2000, 1);
    } catch (std::runtime_error &e) {
      std::string expected =
          "Error: out of boundaries --> value beyond size of vector";
      ass(e.what() == expected, s + "Error out of boundaries");
    }
  }
  {
    std::string s = "AtSubsetting: ";
    ass(at(coca(1, 2, 3), 1), s + "at r vec");
    ass(at(matrix(coca(1, 2, 3, 4), 2, 2), 2) == 3, s + "r matrix");
    ass(at(matrix(coca(1, 2, 3, 4), 2, 2), 2, 2) == 4, s + "r matrix");
  }
}
