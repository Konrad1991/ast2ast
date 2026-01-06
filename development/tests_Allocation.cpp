#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/Calculations/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/Calculations/UnaryCalculations.hpp"
#include "../inst/include/etr_bits/Subsetting.hpp"
#include "../inst/include/etr_bits/Allocation.hpp"
#include "../inst/include/etr_bits/Utilities/Printing.hpp"
using namespace etr;

int main() {
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
    // Increasing
    Array<Double, Buffer<Double>> a;
    a = colon(Integer(1), Integer(10));
    ass<": size">(a.size() == 10);
    for (std::size_t i = 0; i < 10; i++) {
      ass<": elements">(compare(get_val(a.get(i)), static_cast<double>(i + 1) ));
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


}
