#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

// [[Rcpp::export]]
void tests_allocation() {
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };

  // 1. Test basic concatenate
  {
    Array<Double, Buffer<Double>> a;
    a = c(Double(1.1), Double(2.2), Double(3.3));
    ass<"c, with doubles-> size">(a.size() == 3);
    ass<"c, with doubles 0">(compare(get_val(a.get(0)), 1.1));
    ass<"c, with doubles 1">(compare(get_val(a.get(1)), 2.2));
    ass<"c, with doubles 2">(compare(get_val(a.get(2)), 3.3));
  }
  // 2. Can I use Variable<Double> as type?
  {
    Array<Variable<Double>, Buffer<Variable<Double>>> a;
    a = c(Double(1.1), Double(4.4), Variable<Double>(3.3));
    ass<"c, with doubles-> size">(a.size() == 3);
    ass<"c, with doubles 0">(compare(get_val(a.get(0)), 1.1));
    ass<"c, with doubles 1">(compare(get_val(a.get(1)), 4.4));
    ass<"c, with doubles 2">(compare(get_val(a.get(2)), 3.3));
    var expr = a.get(0)*a.get(0);
    Array<Variable<Double>, Buffer<Variable<Double>>> b;
    // Does the deriv information get lost?
    b = c(expr);
    var v = b.get(0);
    auto d = derivatives(v, wrt(a.get(0)));
    ass<"Deriv of vector">(compare(get_val(d[0]), 2.2));
  }
  // 3. Usage of Dual
  {
    Array<Dual, Buffer<Dual>> a;
    a = c(Double(1.1), Dual(2.2, 1.0), Dual(3.3, 0.0));
    ass<"c, with doubles-> size">(a.size() == 3);
    ass<"c, with doubles 0">(compare(get_val(a.get(0)), 1.1));
    ass<"c, with doubles 1">(compare(get_val(a.get(1)), 2.2));
    ass<"c, with doubles 2">(compare(get_val(a.get(2)), 3.3));
    ass<"c, with doubles 0">(compare(a.get(0).dot, 0.0));
    ass<"c, with doubles 1">(compare(a.get(1).dot, 1.0));
    ass<"c, with doubles 2">(compare(a.get(2).dot, 0.0));
  }

  // 4. Colon
  {
    // Increasing Integers
    Array<Double, Buffer<Double>> a;
    a = colon(Integer(1), Integer(10));
    ass<": size">(a.size() == 10);
    for (std::size_t i = 0; i < 10; i++) {
      ass<": elements">(compare(get_val(a.get(i)), static_cast<double>(i + 1) ));
    }
    // Increasing Doubles
    a = colon(Double(0.1), Double(10.1));
    ass<": size">(a.size() == 11);
    for (std::size_t i = 0; i < 11; i++) {
      ass<": elements">(compare(get_val(a.get(i)), static_cast<double>(i + 0.1) ));
    }
    // Increasing Doubles with edge case
    a = colon(Double(0.1), Double(10.0));
    ass<": size">(a.size() == 10);
    for (std::size_t i = 0; i < 10; i++) {
      ass<": elements">(compare(get_val(a.get(i)), static_cast<double>(i + 0.1) ));
    }
    // Edge case start == end
    a = colon(Integer(1), Integer(1));
    ass<": size">(a.size() == 1);
    ass<": elements">(compare(get_val(a.get(0)), 1));
    a = colon(Integer(0), Integer(0));
    ass<": size">(a.size() == 1);
    ass<": elements">(compare(get_val(a.get(0)), 0));
    a = colon(Integer(-1), Integer(-1));
    ass<": size">(a.size() == 1);
    ass<": elements">(compare(get_val(a.get(0)), -1));
    // Decreasing
    a = colon(Integer(10), Integer(1));
    ass<": size">(a.size() == 10);
    std::array<double, 10>expected{10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
    for (std::size_t i = 0; i < 10; i++) {
      ass<": elements">(compare(get_val(a.get(i)), expected[i]));
    }
    // Using arrays
    a = colon(c(Integer(1)), c(Integer(2)) );
    ass<": size">(a.size() == 2);
    for (std::size_t i = 0; i < 2; i++) {
      ass<": elements">(compare(get_val(a.get(i)), static_cast<double>(i + 1) ));
    }
  }

  // 5. Rep
  {
    // replicate of array & times is scalar
    Array<Double, Buffer<Double>> a;
    a = colon(Integer(1), Integer(2));
    a = rep(a, Integer(3));
    ass<": size">(a.size() == 6);
    std::array<double, 6> expected1{1, 2, 1, 2, 1, 2};
    for (std::size_t i = 0; i < 6; i++) {
      ass<": elements">(compare(get_val(a.get(i)), expected1[i]));
    }
    // replicate of array and times is array
    a = colon(Integer(1), Integer(2));
    a = rep(a, c(Integer(1)));
    ass<": size">(a.size() == 2);
    std::array<double, 2> expected2{1, 2};
    for (std::size_t i = 0; i < 2; i++) {
      ass<": elements">(compare(get_val(a.get(i)), expected2[i]));
    }
    // replicate of scalar & times is scalar
    a = rep(Double(3.14), Integer(3));
    ass<": size">(a.size() == 3);
    std::array<double, 3> expected3{3.14, 3.14, 3.14,};
    for (std::size_t i = 0; i < 3; i++) {
      ass<": elements">(compare(get_val(a.get(i)), expected3[i]));
    }
    // replicate of array and times is array
    a = rep(Double(5.5), c(Integer(3)));
    ass<": size">(a.size() == 3);
    std::array<double, 3> expected4{5.5, 5.5, 5.5};
    for (std::size_t i = 0; i < 3; i++) {
      ass<": elements">(compare(get_val(a.get(i)), expected4[i]));
    }
  }

  // 6. Vectors
  {
    // regualar vectors
    int size = 3;
    Array<Logical, Buffer<Logical>> l = logical(Integer(size));
    Array<Integer, Buffer<Integer>> i = integer(Integer(size));
    Array<Double, Buffer<Double>> d = numeric(Integer(size));
    Array<Dual, Buffer<Dual>> dua = numeric_dual(Integer(size));
    Array<Variable<Double>, Buffer<Variable<Double>>> rev_ad = numeric_rev_ad(Integer(size));
    ass<"vector size logical vec">(l.size() == 3);
    ass<"vector size integer vec">(i.size() == 3);
    ass<"vector size double vec">(d.size() == 3);
    ass<"vector size dual vec">(dua.size() == 3);
    ass<"vector size rev_ad vec">(rev_ad.size() == 3);

    // invalid size
    {
      int invalid1 = 0;
      try {
        Array<Logical, Buffer<Logical>> l = logical(Integer(invalid1));
      }
      catch (const std::exception& e) {
        const std::string expected = "size in fct vector/logical/integer/numeric/matrix/array has to be a positive integer";
        ass<"size in allocation is 0">(std::strcmp(e.what(), expected.c_str()) == 0);
      }

      int invalid2 = -1;
      try {
        Array<Logical, Buffer<Logical>> l = logical(Integer(invalid2));
      }
      catch (const std::exception& e) {
        const std::string expected = "size in fct vector/logical/integer/numeric/matrix/array has to be a positive integer";
        ass<"size in allocation is 0">(std::strcmp(e.what(), expected.c_str()) == 0);
      }
    }

    // vector with size argument of array
    {
      const Array<Integer, Buffer<Integer>> size = c(Integer(3));
      Array<Logical, Buffer<Logical>> l = logical(size);
      Array<Integer, Buffer<Integer>> i = integer(size);
      Array<Double, Buffer<Double>> d = numeric(size);
      Array<Dual, Buffer<Dual>> dua = numeric_dual(size);
      Array<Variable<Double>, Buffer<Variable<Double>>> rev_ad = numeric_rev_ad(size);
      ass<"vector size logical vec">(l.size() == 3);
      ass<"vector size integer vec">(i.size() == 3);
      ass<"vector size double vec">(d.size() == 3);
      ass<"vector size dual vec">(dua.size() == 3);
      ass<"vector size rev_ad vec">(rev_ad.size() == 3);
      // DOuble as size
      {
        const Array<Double, Buffer<Double>> size = c(Double(3.5));
        Array<Logical, Buffer<Logical>> l = logical(size);
        Array<Integer, Buffer<Integer>> i = integer(size);
        Array<Double, Buffer<Double>> d = numeric(size);
        Array<Dual, Buffer<Dual>> dua = numeric_dual(size);
        Array<Variable<Double>, Buffer<Variable<Double>>> rev_ad = numeric_rev_ad(size);
        ass<"vector size logical vec">(l.size() == 3);
        ass<"vector size integer vec">(i.size() == 3);
        ass<"vector size double vec">(d.size() == 3);
        ass<"vector size dual vec">(dua.size() == 3);
        ass<"vector size rev_ad vec">(rev_ad.size() == 3);
      }
      // Dual as size
      {
        const Array<Dual, Buffer<Dual>> size = c(Dual(3.5));
        Array<Logical, Buffer<Logical>> l = logical(size);
        Array<Integer, Buffer<Integer>> i = integer(size);
        Array<Double, Buffer<Double>> d = numeric(size);
        Array<Dual, Buffer<Dual>> dua = numeric_dual(size);
        Array<Variable<Double>, Buffer<Variable<Double>>> rev_ad = numeric_rev_ad(size);
        ass<"vector size logical vec">(l.size() == 3);
        ass<"vector size integer vec">(i.size() == 3);
        ass<"vector size double vec">(d.size() == 3);
        ass<"vector size dual vec">(dua.size() == 3);
        ass<"vector size rev_ad vec">(rev_ad.size() == 3);
      }
      // Variable<Double> as size
      {
        const Array<Variable<Double>, Buffer<Variable<Double>>> size = c(Variable<Double>(3.5));
        Array<Logical, Buffer<Logical>> l = logical(size);
        Array<Integer, Buffer<Integer>> i = integer(size);
        Array<Double, Buffer<Double>> d = numeric(size);
        Array<Dual, Buffer<Dual>> dua = numeric_dual(size);
        Array<Variable<Double>, Buffer<Variable<Double>>> rev_ad = numeric_rev_ad(size);
        ass<"vector size logical vec">(l.size() == 3);
        ass<"vector size integer vec">(i.size() == 3);
        ass<"vector size double vec">(d.size() == 3);
        ass<"vector size dual vec">(dua.size() == 3);
        ass<"vector size rev_ad vec">(rev_ad.size() == 3);
      }
      // Logical as size
      {
        const Array<Logical, Buffer<Logical>> size = c(Logical(true));
        Array<Logical, Buffer<Logical>> l = logical(size);
        Array<Integer, Buffer<Integer>> i = integer(size);
        Array<Double, Buffer<Double>> d = numeric(size);
        Array<Logical, Buffer<Logical>> dua = numeric_dual(size);
        Array<Variable<Double>, Buffer<Variable<Double>>> rev_ad = numeric_rev_ad(size);
        ass<"vector size logical vec">(l.size() == 1);
        ass<"vector size integer vec">(i.size() == 1);
        ass<"vector size double vec">(d.size() == 1);
        ass<"vector size dual vec">(dua.size() == 1);
        ass<"vector size rev_ad vec">(rev_ad.size() == 1);
      }
    }

  }

  // 7. Matrix
  {
    // regualr matrix
    Array<Double, Buffer<Double>> m = matrix(Double(3.14), Integer(3), Integer(4));
    ass<"matrix size">(m.size() == 12);
    ass<"matrix nrows">(m.get_dim()[0] == 3);
    ass<"matrix ncols">(m.get_dim()[1] == 4);
    for (std::size_t i = 0; i < m.size(); i++) {
      ass<"matrix content check">(compare(get_val(m.get(i)), 3.14));
    }
    // regular matrix filled with other array
    {
      Array<Double, Buffer<Double>> content = c(Double(1.1), Double(2.2), Double(3.3));
      Array<Double, Buffer<Double>> m = matrix(content, Integer(3), Integer(1));
      ass<"matrix size">(m.size() == 3);
      ass<"matrix nrows">(m.get_dim()[0] == 3);
      ass<"matrix ncols">(m.get_dim()[1] == 1);
      ass<"matrix content check">(compare(get_val(m.get(0)), 1.1));
      ass<"matrix content check">(compare(get_val(m.get(1)), 2.2));
      ass<"matrix content check">(compare(get_val(m.get(2)), 3.3));
    }

    // input size does not match nrow * ncol
    {
      try {
        Array<Double, Buffer<Double>> content = c(Double(1.1), Double(2.2), Double(3.3));
        Array<Double, Buffer<Double>> m = matrix(content, Integer(3), Integer(4));
      } catch (const std::exception& e) {
        const std::string expected = "Input for matrix does not match size (nrow * ncol)";
        ass<"input matrix does not match size">(std::strcmp(e.what(), expected.c_str()) == 0);
      }
    }

    // invalid nrow and/or ncol
    {
      Integer invalid_nrow(0);
      Integer invalid_ncol(0);
      Integer valid_nrow(2);
      Integer valid_col(3);
      const Logical val = Logical(true);
      // both invalid
      try {
        Array<Logical, Buffer<Logical>> l = matrix(val, invalid_nrow, invalid_ncol);
      }
      catch (const std::exception& e) {
        const std::string expected = "size in fct vector/logical/integer/numeric/matrix/array has to be a positive integer";
        ass<"size in allocation is 0">(std::strcmp(e.what(), expected.c_str()) == 0);
      }
      // rows invalid
      try {
        Array<Logical, Buffer<Logical>> l = matrix(val, invalid_nrow, valid_col);
      }
      catch (const std::exception& e) {
        const std::string expected = "size in fct vector/logical/integer/numeric/matrix/array has to be a positive integer";
        ass<"size in allocation is 0">(std::strcmp(e.what(), expected.c_str()) == 0);
      }
      // cols invalid
      try {
        Array<Logical, Buffer<Logical>> l = matrix(val, valid_nrow, invalid_ncol);
      }
      catch (const std::exception& e) {
        const std::string expected = "size in fct vector/logical/integer/numeric/matrix/array has to be a positive integer";
        ass<"size in allocation is 0">(std::strcmp(e.what(), expected.c_str()) == 0);
      }
    }

  }

  // 8. Array
  {
    // regualr array
    Double content = 3.14;
    Array<Integer, Buffer<Integer>> dims = c(Integer(2), Integer(3), Integer(4));
    Array<Double, Buffer<Double>> a = array(content, dims);
    ass<"matrix size">(a.size() == 24);
    ass<"matrix dim[1]">(a.get_dim()[0] == 2);
    ass<"matrix dim[2]">(a.get_dim()[1] == 3);
    ass<"matrix dim[3]">(a.get_dim()[2] == 4);
    for (std::size_t i = 0; i < a.size(); i++) {
      ass<"matrix content check">(compare(get_val(a.get(i)), 3.14));
    }
    // array fill with other array
    {
      Array<Double, Buffer<Double>> content = colon(Double(1), Double(24));
      Array<Integer, Buffer<Integer>> dims = c(Integer(2), Integer(3), Integer(4));
      Array<Double, Buffer<Double>> a = array(content, dims);
      ass<"matrix size">(a.size() == 24);
      ass<"matrix dim[1]">(a.get_dim()[0] == 2);
      ass<"matrix dim[2]">(a.get_dim()[1] == 3);
      ass<"matrix dim[3]">(a.get_dim()[2] == 4);
      for (std::size_t i = 0; i < a.size(); i++) {
        ass<"matrix content check">(compare(get_val(a.get(i)), static_cast<double>(i + 1)));
      }
    }
    // input size does not match product of dims
    {
      try {
        Array<Double, Buffer<Double>> content = c(Double(1.1), Double(2.2), Double(3.3));
        Array<Integer, Buffer<Integer>> dims = c(Integer(2), Integer(3), Integer(4));
        Array<Double, Buffer<Double>> m = array(content, dims);
      } catch (const std::exception& e) {
        const std::string expected = "Input for array does not match size (prod(dims))";
        ass<"input array does not match size">(std::strcmp(e.what(), expected.c_str()) == 0);
      }
    }
    // invalid dim
    {
      Integer invalid_dim1(0);
      Integer valid_dim(2);
      const Logical val = Logical(true);
      // both invalid
      try {
        Array<Logical, Buffer<Logical>> l = array(val, c(invalid_dim1));
      }
      catch (const std::exception& e) {
        const std::string expected = "size in fct vector/logical/integer/numeric/matrix/array has to be a positive integer";
        ass<"size in allocation is 0">(std::strcmp(e.what(), expected.c_str()) == 0);
      }
    }
  }

}
