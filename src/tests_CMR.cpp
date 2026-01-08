#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

// [[Rcpp::export]]
void test_cmr() {
  auto compare = [](double l, double r) {
    double TOL = 1e-3;
    return std::abs(l - r) < TOL;
  };
  // preconditions
  {
    // mismatch length
    try {
      Array<Double, Buffer<Double, LBufferTrait>> t(SI{4});
      Array<Double, Buffer<Double, LBufferTrait>> y(SI{5});
      cmr(Double(0.5), t, y);
    } catch (const std::exception& e) {
      const std::string expected = "time_vec and param_vec differ in length in cmr";
      ass<"timevec size does not match param_vec size">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    // < 4 elements
    try {
      Array<Double, Buffer<Double, LBufferTrait>> t(SI{3});
      Array<Double, Buffer<Double, LBufferTrait>> y(SI{3});
      cmr(Double(0.5), t, y);
    } catch (const std::exception& e) {
      const std::string expected = "time and parameter require at least 4 elements";
      ass<"timevec and param vec require min 4 elemens">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // clamps at borders
  {
    Array<Double, Buffer<Double, LBufferTrait>> t(SI{5});
    Array<Double, Buffer<Double, LBufferTrait>> y(SI{5});
    for (int i = 0; i < 5; ++i) {
      t.set(i, Double(double(i))); // 0, 1, 2, 3, 4
      y.set(i, Double(10.0 * i + 5.0)); // 5, 15, 25, 35, 45
    }
    auto left  = cmr(Double(-1.0), t, y);
    auto right = cmr(Double(10.0), t, y);
    ass<"clamp left">(compare(get_val(left),  get_val(y.get(0))));
    ass<"clamp right">(compare(get_val(right), get_val(y.get(4))));
  }
  // knots values are hit
  {
    Array<Double, Buffer<Double, LBufferTrait>> t(SI{5});
    Array<Double, Buffer<Double, LBufferTrait>> y(SI{5});
    for (int i = 0; i < 5; ++i) {
      t.set(i, Double(double(i))); // 0, 1, 2, 3, 4
      y.set(i, Double(double(i*i))); // 0, 1, 4, 9, 16
    }
    for (int i = 0; i < 5; ++i) {
      auto res = cmr(Double(double(i)), t, y);
      ass<"knot interpolation">(compare(get_val(res), get_val(y.get(i))));
    }
  }
  // linear interpolation
  {
    Array<Double, Buffer<Double, LBufferTrait>> t(SI{5});
    Array<Double, Buffer<Double, LBufferTrait>> y(SI{5});
    const double intercept = 3.0;
    const double slope = 0.4;

    for (int i = 0; i < 5; ++i) {
      double ti = double(i);
      t.set(i, Double(ti));
      y.set(i, Double(intercept + slope * ti));
    }
    const double qs[] = {0.1, 0.5, 1.2, 1.9, 2.3, 3.7};
    for (double q : qs) {
      auto res = cmr(Double(q), t, y);
      ass<"linear interpolation">(compare(get_val(res), intercept + slope*q));
    }
  }
  // linear interpolation non uniform
  {
    Array<Double, Buffer<Double, LBufferTrait>> t(SI{5});
    Array<Double, Buffer<Double, LBufferTrait>> y(SI{5});

    const double slope = 2.0;
    const double intercept = -1.0;

    double tv[] = {0.0, 0.5, 2.0, 3.0, 10.0};
    for (int i = 0; i < 5; ++i) {
      t.set(i, Double(tv[i]));
      y.set(i, Double(slope * tv[i] + intercept));
    }

    const double qs[] = {0.25, 0.75, 1.5, 2.5, 9.0, 10};
    for (double q : qs) {
      auto res = cmr(Double(q), t, y);
      ass<"linear interpolation nonuniform">(compare(get_val(res), intercept + slope*q));
    }
  }
  // tests cmr works with Scalar
  {
    Array<Double, Buffer<Double, LBufferTrait>> t(SI{5});
    Array<Double, Buffer<Double, LBufferTrait>> y(SI{5});
    for (int i = 0; i < 5; ++i) {
      t.set(i, Double(double(i)));
      y.set(i, Double(100.0 + i));
    }
    // Integer input
    {
      auto res = cmr(Integer(2), t, y);
      ass<"Integer t works">(compare(get_val(res), get_val(y.get(2))));
    }
    // Logical input
    {
      auto res = cmr(Logical(true), t, y);
      ass<"Logical t works">(compare(get_val(res), get_val(y.get(1))));
    }
    // Double input
    {
      auto res = cmr(Double(3.0), t, y);
      ass<"Double t works">(compare(get_val(res), get_val(y.get(3))));
    }
    // Dual input
    {
      Dual d;
      d.val = 4.0;
      auto res = cmr(d, t, y);
      ass<"Dual t works">(compare(get_val(res), get_val(y.get(4))));
    }
    // rev_ad
    {
      Variable<Double> d = 4.0;
      auto res = cmr(d, t, y);
      ass<"Variable<Double> t works">(compare(get_val(res), get_val(y.get(4))));
    }
  }
  // tests cmr with Dual timeVec and param_vec
  {
    Array<Dual, Buffer<Dual, LBufferTrait>> t(SI{5});
    Array<Dual, Buffer<Dual, LBufferTrait>> y(SI{5});
    const double intercept = 3.0;
    const double slope = 0.4;

    for (int i = 0; i < 5; ++i) {
      double ti = double(i);
      t.set(i, Dual(ti));
      y.set(i, Dual(intercept + slope * ti));
    }
    const double qs[] = {0.1, 0.5, 1.2, 1.9, 2.3, 3.7};
    for (double q : qs) {
      auto res = cmr(Dual(q), t, y);
      ass<"linear interpolation with Dual vecs">(compare(get_val(res), intercept + slope*q));
    }
  }
  // tests cmr with Variable<Double> timeVec and param_vec
  {
    Array<Variable<Double>, Buffer<Variable<Double>, LBufferTrait>> t(SI{5});
    Array<Variable<Double>, Buffer<Variable<Double>, LBufferTrait>> y(SI{5});
    const double intercept = 3.0;
    const double slope = 0.4;

    for (int i = 0; i < 5; ++i) {
      double ti = double(i);
      t.set(i, Variable<Double>(ti));
      y.set(i, Variable<Double>(intercept + slope * ti));
    }
    const double qs[] = {0.1, 0.5, 1.2, 1.9, 2.3, 3.7};
    for (double q : qs) {
      auto res = cmr(Variable<Double>(q), t, y);
      ass<"linear interpolation with Variable<Double> vecs">(compare(get_val(res), intercept + slope*q));
    }
  }
  // exact equality clamping: t == first / last
  {
    Array<Double, Buffer<Double, LBufferTrait>> t(SI{5});
    Array<Double, Buffer<Double, LBufferTrait>> y(SI{5});
    for (int i = 0; i < 5; ++i) {
      t.set(i, Double(double(i)));
      y.set(i, Double(10.0 * i + 5.0));
    }
    ass<"t==first returns first">(compare(get_val(cmr(Double(0.0), t, y)), get_val(y.get(0))));
    ass<"t==last returns last">(compare(get_val(cmr(Double(4.0), t, y)), get_val(y.get(4))));
  }

  // constant signal invariance
  {
    Array<Double, Buffer<Double, LBufferTrait>> t(SI{6});
    Array<Double, Buffer<Double, LBufferTrait>> y(SI{6});
    for (int i = 0; i < 6; ++i) {
      t.set(i, Double(double(i) * 0.7));  // non-uniform-ish
      y.set(i, Double(42.0));
    }
    const double qs[] = {0.0, 0.35, 0.7, 1.05, 2.2, 3.49};
    for (double q : qs) {
      ass<"constant stays constant">(compare(get_val(cmr(Double(q), t, y)), 42.0));
    }
  }

  // array tInp overload uses first element
  {
    Array<Double, Buffer<Double, LBufferTrait>> t(SI{5});
    Array<Double, Buffer<Double, LBufferTrait>> y(SI{5});
    for (int i = 0; i < 5; ++i) {
      t.set(i, Double(double(i)));
      y.set(i, Double(100.0 + i));
    }

    Array<Double, Buffer<Double, LBufferTrait>> q1(SI{1});
    q1.set(0, Double(3.0));
    ass<"array tInp size1 works">(compare(get_val(cmr(q1, t, y)), get_val(y.get(3))));

    Array<Double, Buffer<Double, LBufferTrait>> q2(SI{2});
    q2.set(0, Double(2.0));
    q2.set(1, Double(4.0));
    // should still use first element (2.0)
    ass<"array tInp uses first elem">(compare(get_val(cmr(q2, t, y)), get_val(y.get(2))));
  }

  // timeVec must be strictly increasing
  {
    try {
      Array<Double, Buffer<Double, LBufferTrait>> t(SI{4});
      Array<Double, Buffer<Double, LBufferTrait>> y(SI{4});
      t.set(0, Double(0.0)); t.set(1, Double(1.0)); t.set(2, Double(1.0)); t.set(3, Double(2.0)); // duplicate
      y.set(0, Double(0.0)); y.set(1, Double(1.0)); y.set(2, Double(2.0)); y.set(3, Double(3.0));
      cmr(Double(1.5), t, y);
      ass<"expected strictly increasing assertion">(false);
    }
    catch (const std::exception& e) {
      const std::string expected = "timeVec must be strictly increasing";
      ass<"size in allocation is 0">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }

}
