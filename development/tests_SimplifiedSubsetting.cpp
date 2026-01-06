#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/Calculations/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/Calculations/UnaryCalculations.hpp"
#include "../inst/include/etr_bits/Subsetting.hpp"

using namespace etr;

int main() {
  auto compare = [](double l, double r) {
    double TOL = 1e-9;
    return std::abs(l - r) < TOL;
  };

  auto fill_0_11 = [](auto& arr) {
    for (std::size_t i = 0; i < arr.size(); i++) {
      arr.set(i, static_cast<double>(i));
    }
  };

  // ===========================================================================
  // 1) ExtractIndex: scalar types
  // ===========================================================================
  {
    ass<"ExtractIndex(Integer) -> 1-based integer">(ExtractIndex(Integer(7)) == 7);
    ass<"ExtractIndex(Double whole)">(ExtractIndex(Double(7.0)) == 7);
    ass<"ExtractIndex(Dual whole)">(ExtractIndex(Dual(7.0)) == 7);
    {
      Variable<Double> v = Double(7.0);
      ass<"ExtractIndex(Variable<Double>)">(ExtractIndex(v) == 7);
    }
  }

  // ===========================================================================
  // 2) ExtractIndex: array index (vector length 1)
  // ===========================================================================
  {
    Array<Integer, Buffer<Integer>> idx_i(SI{1});
    idx_i.set(0, Integer(5));
    ass<"ExtractIndex(Array<Integer>[[1]])">(ExtractIndex(idx_i) == 5);

    Array<Double, Buffer<Double>> idx_d(SI{1});
    idx_d.set(0, Double(5.0));
    ass<"ExtractIndex(Array<Double>[[1]])">(ExtractIndex(idx_d) == 5);
  }

  // ===========================================================================
  // 3) at(): 1D vector correctness (Integer / Double / Dual / Variable / Array-index)
  // ===========================================================================
  {
    Array<Double, Buffer<Double>> a(SI{12});
    fill_0_11(a);

    ass<"at(a, Integer(1)) -> 0">(compare(get_val(at(a, Integer(1))), 0.0));
    ass<"at(a, Integer(12)) -> 11">(compare(get_val(at(a, Integer(12))), 11.0));
    ass<"at(a, Integer(5)) -> 4">(compare(get_val(at(a, Integer(5))), 4.0));

    ass<"at(a, Double(1.0)) -> 0">(compare(get_val(at(a, Double(1.0))), 0.0));
    ass<"at(a, Double(12.0)) -> 11">(compare(get_val(at(a, Double(12.0))), 11.0));

    ass<"at(a, Dual(1.0)) -> 0">(compare(get_val(at(a, Dual(1.0))), 0.0));
    ass<"at(a, Dual(12.0)) -> 11">(compare(get_val(at(a, Dual(12.0))), 11.0));

    {
      Variable<Double> v1 = Double(1.0);
      Variable<Double> v12 = Double(12.0);
      ass<"at(a, Variable<Double>(1)) -> 0">(compare(get_val(at(a, v1)), 0.0));
      ass<"at(a, Variable<Double>(12)) -> 11">(compare(get_val(at(a, v12)), 11.0));
    }

    // Array index (length 1)
    Array<Integer, Buffer<Integer>> idx_i(SI{1});
    idx_i.set(0, Integer(6));
    ass<"at(a, Array<Integer>[1]) -> 5">(compare(get_val(at(a, idx_i)), 5.0));

    Array<Double, Buffer<Double>> idx_d(SI{1});
    idx_d.set(0, Double(6.0));
    ass<"at(a, Array<Double>[1]) -> 5">(compare(get_val(at(a, idx_d)), 5.0));
  }

  // ===========================================================================
  // 4) at(): 3D correctness + stride order
  // ===========================================================================
  {
    Array<Double, Buffer<Double>> a(SI{12});
    fill_0_11(a);

    a.dim = std::vector<std::size_t>{2, 3, 2};

    ass<"at 3D first (1,1,1) -> 0">(
      compare(get_val(at(a, Integer(1), Integer(1), Integer(1))), 0.0)
    );

    ass<"at 3D last (2,3,2) -> 11">(
      compare(get_val(at(a, Integer(2), Integer(3), Integer(2))), 11.0)
    );

    // same as your example
    ass<"at 3D interior (1,3,1) -> 4">(
      compare(get_val(at(a, Integer(1), Integer(3), Integer(1))), 4.0)
    );

    // also test with array-index args (length-1 vectors)
    Array<Integer, Buffer<Integer>> i1(SI{1}); i1.set(0, Integer(1));
    Array<Integer, Buffer<Integer>> i3(SI{1}); i3.set(0, Integer(3));
    Array<Integer, Buffer<Integer>> i2(SI{1}); i2.set(0, Integer(2));

    ass<"at 3D with vector indices (1,3,1) -> 4">(
      compare(get_val(at(a, i1, i3, i1)), 4.0)
    );

    ass<"at 3D with vector indices (2,3,2) -> 11">(
      compare(get_val(at(a, i2, i3, i2)), 11.0)
    );
  }

  // ===========================================================================
  // 5) Overload coverage: force both at() overloads to be instantiated
  // ===========================================================================
  {
    Array<Double, Buffer<Double>> a(SI{3});
    fill_0_11(a);
    // mutable overload should let us assign through the returned reference
    at(a, Integer(2)) = Double(123.0);
    ass<"at() mutable overload allows assignment">(compare(get_val(a.get(1)), 123.0));
    auto expr = a + Double(1.0);
    // Should compile and return something readable
    ass<"at(expr, Integer(1)) works">(compare(get_val(at(expr, Integer(1))), get_val(a.get(0)) + 1.0));
    ass<"at(expr, Integer(3)) works">(compare(get_val(at(expr, Integer(3))), get_val(a.get(2)) + 1.0));

    std::vector<double> owner(5); for (std::size_t i = 0; i < owner.size(); i++) owner[i] = static_cast<double>(i);
    Array<Double, Borrow<Double>> a_borrow(owner.data(), owner.size(), std::vector<std::size_t>{owner.size()});
    at(a_borrow, Integer(2)) = Double(123.0);
    ass<"at() mutable overload allows assignment of borrow">(compare(get_val(a_borrow.get(1)), 123.0));
    at(a_borrow, Integer(2)) = Integer(223.0);
    ass<"at() mutable overload allows assignment of borrow">(compare(get_val(a_borrow.get(1)), 223.0));
  }
  // ===========================================================================
  // 6) Assignments INTO Double elements (Borrow) via proxy (DoubleRef)
  // ===========================================================================
  auto expect_double_at = [&](auto& arr, std::size_t one_based_idx, double expected) {
    ass<"expect_double_at">(compare(get_val(arr.get(--one_based_idx)), expected));
  };
  {
    std::vector<double> owner(4, 0.0);
    std::vector<std::size_t> dim{4};
    Array<Double, Borrow<Double>> a_borrow(owner.data(), owner.size(), dim);

    at(a_borrow, Integer(1)) = Integer(7);
    ass<"Borrow Double <- Integer">(compare(owner[0], 7.0));

    at(a_borrow, Integer(2)) = Logical(true);
    ass<"Borrow Double <- Logical">(compare(owner[1], 1.0));

    at(a_borrow, Integer(3)) = Double(3.5);
    ass<"Borrow Double <- Double">(compare(owner[2], 3.5));

    at(a_borrow, Integer(4)) = Dual(9.0, 123.0);
    ass<"Borrow Double <- Dual (value part)">(compare(owner[3], 9.0));

    // also verify reading through at() matches
    expect_double_at(a_borrow, 1, 7.0);
    expect_double_at(a_borrow, 2, 1.0);
    expect_double_at(a_borrow, 3, 3.5);
    expect_double_at(a_borrow, 4, 9.0);
  }

  // ===========================================================================
  // 7) Assignments INTO Double elements (Borrow) via proxy (DoubleRef)
  // ===========================================================================
  auto expect_dual_at = [&](auto& arr, std::size_t one_based_idx, double expected_val, double expected_dot) {
    auto x = arr.get(--one_based_idx);
    ass<"expect_dual_at value">(compare(get_val(x), expected_val));
    ass<"expect_dual_at dot">(compare(x.dot, expected_dot));
  };
  {
    std::vector<double> owner(4, 0.0);
    std::vector<std::size_t> dim{4};
    Array<Double, Borrow<Double>> a_borrow(owner.data(), owner.size(), dim);

    at(a_borrow, Integer(1)) = Integer(7);
    ass<"Borrow Double <- Integer">(compare(owner[0], 7.0));

    at(a_borrow, Integer(2)) = Logical(true);
    ass<"Borrow Double <- Logical">(compare(owner[1], 1.0));

    at(a_borrow, Integer(3)) = Double(3.5);
    ass<"Borrow Double <- Double">(compare(owner[2], 3.5));

    at(a_borrow, Integer(4)) = Dual(9.0, 123.0);
    ass<"Borrow Double <- Dual (value part)">(compare(owner[3], 9.0));

    // also verify reading through at() matches
    expect_double_at(a_borrow, 1, 7.0);
    expect_double_at(a_borrow, 2, 1.0);
    expect_double_at(a_borrow, 3, 3.5);
    expect_double_at(a_borrow, 4, 9.0);
  }

  return 0;
}
