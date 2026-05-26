#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

const double NaN = std::numeric_limits<double>::quiet_NaN();

Logical create_logical(const bool val, const bool is_na) {
  Logical res(val);
  res.is_na = is_na;
  return res;
}
Integer create_integer(const int val, const bool is_na) {
  Integer res(val);
  res.is_na = is_na;
  return res;
}
Double create_double(const double val, const bool is_na) {
  Double res(val);
  res.is_na = is_na;
  return res;
}
ReverseDouble create_reverse_double(const double val, const bool is_na) {
  ReverseDouble res(val);
  res.is_na = static_cast<uint8_t>(is_na);
  return res;
}
Dual create_dual(const double val, const double dot, const bool is_na, const bool is_na_dot) {
  Dual res(val, dot);
  res.is_na = is_na;
  res.is_na_dot = is_na_dot;
  return res;
}

template<typename L, typename R> void compare(const L& l, const R& r) {
  const double TOL = 1e-6;

  if constexpr (requires { l.isNaN(); } && requires {r.isNaN(); }) {
    if (l.isNaN() && r.isNaN()) {
      ass<"both values are NaN">(true);
      return;
    } else if (l.isNaN() && !r.isNaN()) {
      ass<"both values should be NaN">(false);
      return;
    }
  }
  if (l.isNA() && r.isNA()) {
    ass<"both values are NA">(true);
    return;
  } else if (l.isNA() && !r.isNA()){
    ass<"both values should be NA">(false);
    return;
  }
  Double diff = l - r;
  if (diff < 0.0) diff = -diff;
  ass<"Values dont match">(diff < TOL);

  if constexpr (IS<Decayed<L>, Dual> || IS<Decayed<L>, DualRef>) {
    Dual dl = l;
    Dual dr = r;
    if (dl.isNaNDot() && dr.isNaNDot()) {
      ass<"both derivatives are NaN">(true);
      return;
    } else if (dl.isNaNDot() && !dr.isNaNDot()) {
      ass<"both derivatives should be NaN">(false);
      return;
    }
    if (dl.isNADot() && dr.isNADot()) {
      ass<"both derivatives are NA">(true);
      return;
    } else if (dl.isNADot() && !dr.isNADot()) {
      ass<"both derivatives should be NA">(false);
      return;
    }
    Double diff_dot;
    diff_dot.val = dl.dot - dr.dot;
    if (diff_dot < 0.0) diff_dot = -diff_dot;
    ass<"Derivatives dont match">(diff_dot < TOL);
  }
}

// [[Rcpp::export]]
void test_scalars() {
  TAPE_INTERN.clear();

  Logical l1 = true;
  Logical l2 = Logical::NA();

  Integer i1 = 2;
  Integer i2 = Integer::NA();

  Double d1 = 3;
  Double d2 = Double::NA();
  Double d3 = Double::NaN();

  double d = 3.0;
  double d_dot = 4.0;
  Dual dua1(d, d_dot);
  Dual dua2(d, d_dot);
  dua2.is_na = true;
  dua2.is_na_dot = true;
  Dual dua3(d, d_dot);
  dua3.is_na = true;
  dua3.is_na_dot = false;
  Dual dua4(d, d_dot);
  dua4.is_na = false;
  dua4.is_na_dot = true;

  ReverseDouble revd1(5.0);
  ReverseDouble revd2 = d2;
  ReverseDouble revd3 = d3;

  bool lref_val1 = true;
  bool lref_na1 = false;
  LogicalRef lr_owned1(&lref_val1, &lref_na1);
  bool lref_val2 = true;
  bool lref_na2 = true;
  LogicalRef lr_owned2(&lref_val2, &lref_na2);

  int iref_val1 = 7;
  bool iref_na1 = false;
  IntegerRef ir_owned1(&iref_val1, &iref_na1);
  int iref_val2 = 7;
  bool iref_na2 = true;
  IntegerRef ir_owned2(&iref_val2, &iref_na2);

  double dref_val1 = 20.0;
  bool dref_na1 = false;
  DoubleRef dr_owned1(&dref_val1, &dref_na1);
  double dref_val2 = 20.0;
  bool dref_na2 = true;
  DoubleRef dr_owned2(&dref_val2, &dref_na2);

  double dualref_val1 = 2.0;
  double dualref_dot1 = 1.0;
  bool dualref_na1 = false;
  bool dualref_na_dot1 = false;
  DualRef duar_owned1(&dualref_val1, &dualref_dot1, &dualref_na1, &dualref_na_dot1);
  double dualref_val2 = 2.0;
  double dualref_dot2 = 1.0;
  bool dualref_na2 = true;
  bool dualref_na_dot2 = true;
  DualRef duar_owned2(&dualref_val2, &dualref_dot2, &dualref_na2, &dualref_na_dot2);
  double dualref_val3 = 3.0;
  double dualref_dot3 = 1.0;
  bool dualref_na3 = false;
  bool dualref_na_dot3 = true;
  DualRef duar_owned3(&dualref_val3, &dualref_dot3, &dualref_na3, &dualref_na_dot3);
  double dualref_val4 = 4.0;
  double dualref_dot4 = 1.0;
  bool dualref_na4 = true;
  bool dualref_na_dot4 = false;
  DualRef duar_owned4(&dualref_val4, &dualref_dot4, &dualref_na4, &dualref_na_dot4);

  // ReverseDouble has no reference wrapper; use plain ReverseDouble values.
  ReverseDouble revdr_owned1 = create_reverse_double(9.0, false);
  ReverseDouble revdr_owned2 = create_reverse_double(9.0, true);
  compare(sin(l1), Double(0.841470984807897));
  compare(sin(l2), Logical::NA());
  compare(sin(i1), Double(0.909297426825682));
  compare(sin(i2), Integer::NA());
  compare(sin(d1), Double(0.141120008059867));
  compare(sin(d2), Double::NA());
  compare(sin(d3), Double::NaN());
  compare(sin(dua1), Dual(0.141120008059867, -0.989992496600445));
  compare(sin(dua2), Dual::NA());
  compare(sin(dua3), Dual::NA());
  compare(sin(dua4), Dual(0.141120008059867, std::numeric_limits<double>::quiet_NaN()));
  compare(sin(revd1), Double(-0.958924274663138));
  compare(sin(revd2), ReverseDouble::NA());
  compare(sin(revd3), ReverseDouble::NaN());
  compare(sin(lr_owned1), Double(0.841470984807897));
  compare(sin(lr_owned2), Logical::NA());
  compare(sin(ir_owned1), Double(0.656986598718789));
  compare(sin(ir_owned2), Integer::NA());
  compare(sin(dr_owned1), Double(0.912945250727628));
  compare(sin(dr_owned2), Double::NA());
  compare(sin(duar_owned1), Dual(0.909297426825682, -0.416146836547142));
  compare(sin(duar_owned2), Dual::NA());
  compare(sin(duar_owned3), Dual(0.141120008059867, std::numeric_limits<double>::quiet_NaN()));
  compare(sin(duar_owned4), Dual::NA());
  compare(sin(revdr_owned1), Double(0.412118485241757));
  compare(sin(revdr_owned2), ReverseDouble::NA());


  compare(l1 * l1, create_logical(true, false));
  compare(l2 * l1, create_logical(true, true));
  compare(i1 * l1, create_integer(2, false));
  compare(i2 * l1, create_integer(1, true));
  compare(d1 * l1, create_double(3.000, false));
  compare(d2 * l1, create_double(NaN, true));
  compare(d2 * l1, create_double(NaN, true));
  compare(dua1 * l1, create_dual(3.000, 4, false, false));
  compare(dua2 * l1, create_dual(NaN, NaN, true, true));
  compare(dua3 * l1, create_dual(NaN, NaN, true, true));
  compare(dua4 * l1, create_dual(3.000, NaN, false, false));
  compare(revd1 * l1, create_reverse_double(5.000, false));
  compare(revd2 * l1, create_reverse_double(NaN, true));
  compare(revd3 * l1, create_reverse_double(NaN, false));
  compare(lr_owned1 * l1, create_logical(true, false));
  compare(lr_owned2 * l1, create_logical(true, true));
  compare(ir_owned1 * l1, create_integer(7, false));
  compare(ir_owned2 * l1, create_integer(1, true));
  compare(dr_owned1 * l1, create_double(20.000, false));
  compare(dr_owned2 * l1, create_double(NaN, true));
  compare(duar_owned1 * l1, create_dual(2.000, 1, false, false));
  compare(duar_owned2 * l1, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * l1, create_dual(3.000, NaN, false, false));
  compare(duar_owned4 * l1, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * l1, create_reverse_double(9.000, false));
  compare(revdr_owned2 * l1, create_reverse_double(NaN, true));
  compare(l1 * l2, create_logical(true, true));
  compare(l2 * l2, create_logical(true, true));
  compare(i1 * l2, create_integer(1, true));
  compare(i2 * l2, create_integer(1, true));
  compare(d1 * l2, create_double(NaN, true));
  compare(d2 * l2, create_double(NaN, true));
  compare(d2 * l2, create_double(NaN, true));
  compare(dua1 * l2, create_dual(NaN, NaN, true, true));
  compare(dua2 * l2, create_dual(NaN, NaN, true, true));
  compare(dua3 * l2, create_dual(NaN, NaN, true, true));
  compare(dua4 * l2, create_dual(NaN, NaN, true, true));
  compare(revd1 * l2, create_reverse_double(NaN, true));
  compare(revd2 * l2, create_reverse_double(NaN, true));
  compare(revd3 * l2, create_reverse_double(NaN, true));
  compare(lr_owned1 * l2, create_logical(true, true));
  compare(lr_owned2 * l2, create_logical(true, true));
  compare(ir_owned1 * l2, create_integer(1, true));
  compare(ir_owned2 * l2, create_integer(1, true));
  compare(dr_owned1 * l2, create_double(NaN, true));
  compare(dr_owned2 * l2, create_double(NaN, true));
  compare(duar_owned1 * l2, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * l2, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * l2, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * l2, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * l2, create_reverse_double(NaN, true));
  compare(revdr_owned2 * l2, create_reverse_double(NaN, true));
  compare(l1 * i1, create_integer(2, false));
  compare(l2 * i1, create_integer(1, true));
  compare(i1 * i1, create_integer(4, false));
  compare(i2 * i1, create_integer(1, true));
  compare(d1 * i1, create_double(6.000, false));
  compare(d2 * i1, create_double(NaN, true));
  compare(d2 * i1, create_double(NaN, true));
  compare(dua1 * i1, create_dual(6.000, 8, false, false));
  compare(dua2 * i1, create_dual(NaN, NaN, true, true));
  compare(dua3 * i1, create_dual(NaN, NaN, true, true));
  compare(dua4 * i1, create_dual(6.000, NaN, false, false));
  compare(revd1 * i1, create_reverse_double(10.000, false));
  compare(revd2 * i1, create_reverse_double(NaN, true));
  compare(revd3 * i1, create_reverse_double(NaN, false));
  compare(lr_owned1 * i1, create_integer(2, false));
  compare(lr_owned2 * i1, create_integer(1, true));
  compare(ir_owned1 * i1, create_integer(14, false));
  compare(ir_owned2 * i1, create_integer(1, true));
  compare(dr_owned1 * i1, create_double(40.000, false));
  compare(dr_owned2 * i1, create_double(NaN, true));
  compare(duar_owned1 * i1, create_dual(4.000, 2, false, false));
  compare(duar_owned2 * i1, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * i1, create_dual(6.000, NaN, false, false));
  compare(duar_owned4 * i1, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * i1, create_reverse_double(18.000, false));
  compare(revdr_owned2 * i1, create_reverse_double(NaN, true));
  compare(l1 * i2, create_integer(1, true));
  compare(l2 * i2, create_integer(1, true));
  compare(i1 * i2, create_integer(1, true));
  compare(i2 * i2, create_integer(1, true));
  compare(d1 * i2, create_double(NaN, true));
  compare(d2 * i2, create_double(NaN, true));
  compare(d2 * i2, create_double(NaN, true));
  compare(dua1 * i2, create_dual(NaN, NaN, true, true));
  compare(dua2 * i2, create_dual(NaN, NaN, true, true));
  compare(dua3 * i2, create_dual(NaN, NaN, true, true));
  compare(dua4 * i2, create_dual(NaN, NaN, true, true));
  compare(revd1 * i2, create_reverse_double(NaN, true));
  compare(revd2 * i2, create_reverse_double(NaN, true));
  compare(revd3 * i2, create_reverse_double(NaN, true));
  compare(lr_owned1 * i2, create_integer(1, true));
  compare(lr_owned2 * i2, create_integer(1, true));
  compare(ir_owned1 * i2, create_integer(1, true));
  compare(ir_owned2 * i2, create_integer(1, true));
  compare(dr_owned1 * i2, create_double(NaN, true));
  compare(dr_owned2 * i2, create_double(NaN, true));
  compare(duar_owned1 * i2, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * i2, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * i2, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * i2, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * i2, create_reverse_double(NaN, true));
  compare(revdr_owned2 * i2, create_reverse_double(NaN, true));
  compare(l1 * d1, create_double(3.000, false));
  compare(l2 * d1, create_double(NaN, true));
  compare(i1 * d1, create_double(6.000, false));
  compare(i2 * d1, create_double(NaN, true));
  compare(d1 * d1, create_double(9.000, false));
  compare(d2 * d1, create_double(NaN, true));
  compare(d2 * d1, create_double(NaN, true));
  compare(dua1 * d1, create_dual(9.000, 12, false, false));
  compare(dua2 * d1, create_dual(NaN, NaN, true, true));
  compare(dua3 * d1, create_dual(NaN, NaN, true, true));
  compare(dua4 * d1, create_dual(9.000, NaN, false, false));
  compare(revd1 * d1, create_reverse_double(15.000, false));
  compare(revd2 * d1, create_reverse_double(NaN, true));
  compare(revd3 * d1, create_reverse_double(NaN, false));
  compare(lr_owned1 * d1, create_double(3.000, false));
  compare(lr_owned2 * d1, create_double(NaN, true));
  compare(ir_owned1 * d1, create_double(21.000, false));
  compare(ir_owned2 * d1, create_double(NaN, true));
  compare(dr_owned1 * d1, create_double(60.000, false));
  compare(dr_owned2 * d1, create_double(NaN, true));
  compare(duar_owned1 * d1, create_dual(6.000, 3, false, false));
  compare(duar_owned2 * d1, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * d1, create_dual(9.000, NaN, false, false));
  compare(duar_owned4 * d1, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * d1, create_reverse_double(27.000, false));
  compare(revdr_owned2 * d1, create_reverse_double(NaN, true));
  compare(l1 * d2, create_double(NaN, true));
  compare(l2 * d2, create_double(NaN, true));
  compare(i1 * d2, create_double(NaN, true));
  compare(i2 * d2, create_double(NaN, true));
  compare(d1 * d2, create_double(NaN, true));
  compare(d2 * d2, create_double(NaN, true));
  compare(d2 * d2, create_double(NaN, true));
  compare(dua1 * d2, create_dual(NaN, NaN, true, true));
  compare(dua2 * d2, create_dual(NaN, NaN, true, true));
  compare(dua3 * d2, create_dual(NaN, NaN, true, true));
  compare(dua4 * d2, create_dual(NaN, NaN, true, true));
  compare(revd1 * d2, create_reverse_double(NaN, true));
  compare(revd2 * d2, create_reverse_double(NaN, true));
  compare(revd3 * d2, create_reverse_double(NaN, true));
  compare(lr_owned1 * d2, create_double(NaN, true));
  compare(lr_owned2 * d2, create_double(NaN, true));
  compare(ir_owned1 * d2, create_double(NaN, true));
  compare(ir_owned2 * d2, create_double(NaN, true));
  compare(dr_owned1 * d2, create_double(NaN, true));
  compare(dr_owned2 * d2, create_double(NaN, true));
  compare(duar_owned1 * d2, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * d2, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * d2, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * d2, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * d2, create_reverse_double(NaN, true));
  compare(revdr_owned2 * d2, create_reverse_double(NaN, true));
  compare(l1 * d2, create_double(NaN, true));
  compare(l2 * d2, create_double(NaN, true));
  compare(i1 * d2, create_double(NaN, true));
  compare(i2 * d2, create_double(NaN, true));
  compare(d1 * d2, create_double(NaN, true));
  compare(d2 * d2, create_double(NaN, true));
  compare(d2 * d2, create_double(NaN, true));
  compare(dua1 * d2, create_dual(NaN, NaN, true, true));
  compare(dua2 * d2, create_dual(NaN, NaN, true, true));
  compare(dua3 * d2, create_dual(NaN, NaN, true, true));
  compare(dua4 * d2, create_dual(NaN, NaN, true, true));
  compare(revd1 * d2, create_reverse_double(NaN, true));
  compare(revd2 * d2, create_reverse_double(NaN, true));
  compare(revd3 * d2, create_reverse_double(NaN, true));
  compare(lr_owned1 * d2, create_double(NaN, true));
  compare(lr_owned2 * d2, create_double(NaN, true));
  compare(ir_owned1 * d2, create_double(NaN, true));
  compare(ir_owned2 * d2, create_double(NaN, true));
  compare(dr_owned1 * d2, create_double(NaN, true));
  compare(dr_owned2 * d2, create_double(NaN, true));
  compare(duar_owned1 * d2, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * d2, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * d2, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * d2, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * d2, create_reverse_double(NaN, true));
  compare(revdr_owned2 * d2, create_reverse_double(NaN, true));
  compare(l1 * dua1, create_dual(3.000, 4, false, false));
  compare(l2 * dua1, create_dual(NaN, NaN, true, true));
  compare(i1 * dua1, create_dual(6.000, 8, false, false));
  compare(i2 * dua1, create_dual(NaN, NaN, true, true));
  compare(d1 * dua1, create_dual(9.000, 12, false, false));
  compare(d2 * dua1, create_dual(NaN, NaN, true, true));
  compare(d2 * dua1, create_dual(NaN, NaN, true, true));
  compare(dua1 * dua1, create_dual(9.000, 24, false, false));
  compare(dua2 * dua1, create_dual(NaN, NaN, true, true));
  compare(dua3 * dua1, create_dual(NaN, NaN, true, true));
  compare(dua4 * dua1, create_dual(9.000, NaN, false, false));
  compare(lr_owned1 * dua1, create_dual(3.000, 4, false, false));
  compare(lr_owned2 * dua1, create_dual(NaN, NaN, true, true));
  compare(ir_owned1 * dua1, create_dual(21.000, 28, false, false));
  compare(ir_owned2 * dua1, create_dual(NaN, NaN, true, true));
  compare(dr_owned1 * dua1, create_dual(60.000, 80, false, false));
  compare(dr_owned2 * dua1, create_dual(NaN, NaN, true, true));
  compare(duar_owned1 * dua1, create_dual(6.000, 11, false, false));
  compare(duar_owned2 * dua1, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * dua1, create_dual(9.000, NaN, false, false));
  compare(duar_owned4 * dua1, create_dual(NaN, NaN, true, true));
  compare(l1 * dua2, create_dual(NaN, NaN, true, true));
  compare(l2 * dua2, create_dual(NaN, NaN, true, true));
  compare(i1 * dua2, create_dual(NaN, NaN, true, true));
  compare(i2 * dua2, create_dual(NaN, NaN, true, true));
  compare(d1 * dua2, create_dual(NaN, NaN, true, true));
  compare(d2 * dua2, create_dual(NaN, NaN, true, true));
  compare(d2 * dua2, create_dual(NaN, NaN, true, true));
  compare(dua1 * dua2, create_dual(NaN, NaN, true, true));
  compare(dua2 * dua2, create_dual(NaN, NaN, true, true));
  compare(dua3 * dua2, create_dual(NaN, NaN, true, true));
  compare(dua4 * dua2, create_dual(NaN, NaN, true, true));
  compare(lr_owned1 * dua2, create_dual(NaN, NaN, true, true));
  compare(lr_owned2 * dua2, create_dual(NaN, NaN, true, true));
  compare(ir_owned1 * dua2, create_dual(NaN, NaN, true, true));
  compare(ir_owned2 * dua2, create_dual(NaN, NaN, true, true));
  compare(dr_owned1 * dua2, create_dual(NaN, NaN, true, true));
  compare(dr_owned2 * dua2, create_dual(NaN, NaN, true, true));
  compare(duar_owned1 * dua2, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * dua2, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * dua2, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * dua2, create_dual(NaN, NaN, true, true));
  compare(l1 * dua3, create_dual(NaN, NaN, true, true));
  compare(l2 * dua3, create_dual(NaN, NaN, true, true));
  compare(i1 * dua3, create_dual(NaN, NaN, true, true));
  compare(i2 * dua3, create_dual(NaN, NaN, true, true));
  compare(d1 * dua3, create_dual(NaN, NaN, true, true));
  compare(d2 * dua3, create_dual(NaN, NaN, true, true));
  compare(d2 * dua3, create_dual(NaN, NaN, true, true));
  compare(dua1 * dua3, create_dual(NaN, NaN, true, true));
  compare(dua2 * dua3, create_dual(NaN, NaN, true, true));
  compare(dua3 * dua3, create_dual(NaN, NaN, true, true));
  compare(dua4 * dua3, create_dual(NaN, NaN, true, true));
  compare(lr_owned1 * dua3, create_dual(NaN, NaN, true, true));
  compare(lr_owned2 * dua3, create_dual(NaN, NaN, true, true));
  compare(ir_owned1 * dua3, create_dual(NaN, NaN, true, true));
  compare(ir_owned2 * dua3, create_dual(NaN, NaN, true, true));
  compare(dr_owned1 * dua3, create_dual(NaN, NaN, true, true));
  compare(dr_owned2 * dua3, create_dual(NaN, NaN, true, true));
  compare(duar_owned1 * dua3, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * dua3, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * dua3, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * dua3, create_dual(NaN, NaN, true, true));
  compare(l1 * dua4, create_dual(3.000, NaN, false, false));
  compare(l2 * dua4, create_dual(NaN, NaN, true, true));
  compare(i1 * dua4, create_dual(6.000, NaN, false, false));
  compare(i2 * dua4, create_dual(NaN, NaN, true, true));
  compare(d1 * dua4, create_dual(9.000, NaN, false, false));
  compare(d2 * dua4, create_dual(NaN, NaN, true, true));
  compare(d2 * dua4, create_dual(NaN, NaN, true, true));
  compare(dua1 * dua4, create_dual(9.000, NaN, false, false));
  compare(dua2 * dua4, create_dual(NaN, NaN, true, true));
  compare(dua3 * dua4, create_dual(NaN, NaN, true, true));
  compare(dua4 * dua4, create_dual(9.000, NaN, false, false));
  compare(lr_owned1 * dua4, create_dual(3.000, NaN, false, false));
  compare(lr_owned2 * dua4, create_dual(NaN, NaN, true, true));
  compare(ir_owned1 * dua4, create_dual(21.000, NaN, false, false));
  compare(ir_owned2 * dua4, create_dual(NaN, NaN, true, true));
  compare(dr_owned1 * dua4, create_dual(60.000, NaN, false, false));
  compare(dr_owned2 * dua4, create_dual(NaN, NaN, true, true));
  compare(duar_owned1 * dua4, create_dual(6.000, NaN, false, false));
  compare(duar_owned2 * dua4, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * dua4, create_dual(9.000, NaN, false, false));
  compare(duar_owned4 * dua4, create_dual(NaN, NaN, true, true));
  compare(l1 * revd1, create_reverse_double(5.000, false));
  compare(l2 * revd1, create_reverse_double(NaN, true));
  compare(i1 * revd1, create_reverse_double(10.000, false));
  compare(i2 * revd1, create_reverse_double(NaN, true));
  compare(d1 * revd1, create_reverse_double(15.000, false));
  compare(d2 * revd1, create_reverse_double(NaN, true));
  compare(d2 * revd1, create_reverse_double(NaN, true));
  compare(revd1 * revd1, create_reverse_double(25.000, false));
  compare(revd2 * revd1, create_reverse_double(NaN, true));
  compare(revd3 * revd1, create_reverse_double(NaN, false));
  compare(lr_owned1 * revd1, create_reverse_double(5.000, false));
  compare(lr_owned2 * revd1, create_reverse_double(NaN, true));
  compare(ir_owned1 * revd1, create_reverse_double(35.000, false));
  compare(ir_owned2 * revd1, create_reverse_double(NaN, true));
  compare(dr_owned1 * revd1, create_reverse_double(100.000, false));
  compare(dr_owned2 * revd1, create_reverse_double(NaN, true));
  compare(revdr_owned1 * revd1, create_reverse_double(45.000, false));
  compare(revdr_owned2 * revd1, create_reverse_double(NaN, true));
  compare(l1 * revd2, create_reverse_double(NaN, true));
  compare(l2 * revd2, create_reverse_double(NaN, true));
  compare(i1 * revd2, create_reverse_double(NaN, true));
  compare(i2 * revd2, create_reverse_double(NaN, true));
  compare(d1 * revd2, create_reverse_double(NaN, true));
  compare(d2 * revd2, create_reverse_double(NaN, true));
  compare(d2 * revd2, create_reverse_double(NaN, true));
  compare(revd1 * revd2, create_reverse_double(NaN, true));
  compare(revd2 * revd2, create_reverse_double(NaN, true));
  compare(revd3 * revd2, create_reverse_double(NaN, true));
  compare(lr_owned1 * revd2, create_reverse_double(NaN, true));
  compare(lr_owned2 * revd2, create_reverse_double(NaN, true));
  compare(ir_owned1 * revd2, create_reverse_double(NaN, true));
  compare(ir_owned2 * revd2, create_reverse_double(NaN, true));
  compare(dr_owned1 * revd2, create_reverse_double(NaN, true));
  compare(dr_owned2 * revd2, create_reverse_double(NaN, true));
  compare(revdr_owned1 * revd2, create_reverse_double(NaN, true));
  compare(revdr_owned2 * revd2, create_reverse_double(NaN, true));
  compare(l1 * revd3, create_reverse_double(NaN, false));
  compare(l2 * revd3, create_reverse_double(NaN, true));
  compare(i1 * revd3, create_reverse_double(NaN, false));
  compare(i2 * revd3, create_reverse_double(NaN, true));
  compare(d1 * revd3, create_reverse_double(NaN, false));
  compare(d2 * revd3, create_reverse_double(NaN, true));
  compare(d2 * revd3, create_reverse_double(NaN, true));
  compare(revd1 * revd3, create_reverse_double(NaN, false));
  compare(revd2 * revd3, create_reverse_double(NaN, true));
  compare(revd3 * revd3, create_reverse_double(NaN, false));
  compare(lr_owned1 * revd3, create_reverse_double(NaN, false));
  compare(lr_owned2 * revd3, create_reverse_double(NaN, true));
  compare(ir_owned1 * revd3, create_reverse_double(NaN, false));
  compare(ir_owned2 * revd3, create_reverse_double(NaN, true));
  compare(dr_owned1 * revd3, create_reverse_double(NaN, false));
  compare(dr_owned2 * revd3, create_reverse_double(NaN, true));
  compare(revdr_owned1 * revd3, create_reverse_double(NaN, false));
  compare(revdr_owned2 * revd3, create_reverse_double(NaN, true));
  compare(l1 * lr_owned1, create_logical(true, false));
  compare(l2 * lr_owned1, create_logical(true, true));
  compare(i1 * lr_owned1, create_integer(2, false));
  compare(i2 * lr_owned1, create_integer(1, true));
  compare(d1 * lr_owned1, create_double(3.000, false));
  compare(d2 * lr_owned1, create_double(NaN, true));
  compare(d2 * lr_owned1, create_double(NaN, true));
  compare(dua1 * lr_owned1, create_dual(3.000, 4, false, false));
  compare(dua2 * lr_owned1, create_dual(NaN, NaN, true, true));
  compare(dua3 * lr_owned1, create_dual(NaN, NaN, true, true));
  compare(dua4 * lr_owned1, create_dual(3.000, NaN, false, false));
  compare(revd1 * lr_owned1, create_reverse_double(5.000, false));
  compare(revd2 * lr_owned1, create_reverse_double(NaN, true));
  compare(revd3 * lr_owned1, create_reverse_double(NaN, false));
  compare(lr_owned1 * lr_owned1, create_logical(true, false));
  compare(lr_owned2 * lr_owned1, create_logical(true, true));
  compare(ir_owned1 * lr_owned1, create_integer(7, false));
  compare(ir_owned2 * lr_owned1, create_integer(1, true));
  compare(dr_owned1 * lr_owned1, create_double(20.000, false));
  compare(dr_owned2 * lr_owned1, create_double(NaN, true));
  compare(duar_owned1 * lr_owned1, create_dual(2.000, 1, false, false));
  compare(duar_owned2 * lr_owned1, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * lr_owned1, create_dual(3.000, NaN, false, false));
  compare(duar_owned4 * lr_owned1, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * lr_owned1, create_reverse_double(9.000, false));
  compare(revdr_owned2 * lr_owned1, create_reverse_double(NaN, true));
  compare(l1 * lr_owned2, create_logical(true, true));
  compare(l2 * lr_owned2, create_logical(true, true));
  compare(i1 * lr_owned2, create_integer(1, true));
  compare(i2 * lr_owned2, create_integer(1, true));
  compare(d1 * lr_owned2, create_double(NaN, true));
  compare(d2 * lr_owned2, create_double(NaN, true));
  compare(d2 * lr_owned2, create_double(NaN, true));
  compare(dua1 * lr_owned2, create_dual(NaN, NaN, true, true));
  compare(dua2 * lr_owned2, create_dual(NaN, NaN, true, true));
  compare(dua3 * lr_owned2, create_dual(NaN, NaN, true, true));
  compare(dua4 * lr_owned2, create_dual(NaN, NaN, true, true));
  compare(revd1 * lr_owned2, create_reverse_double(NaN, true));
  compare(revd2 * lr_owned2, create_reverse_double(NaN, true));
  compare(revd3 * lr_owned2, create_reverse_double(NaN, true));
  compare(lr_owned1 * lr_owned2, create_logical(true, true));
  compare(lr_owned2 * lr_owned2, create_logical(true, true));
  compare(ir_owned1 * lr_owned2, create_integer(1, true));
  compare(ir_owned2 * lr_owned2, create_integer(1, true));
  compare(dr_owned1 * lr_owned2, create_double(NaN, true));
  compare(dr_owned2 * lr_owned2, create_double(NaN, true));
  compare(duar_owned1 * lr_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * lr_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * lr_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * lr_owned2, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * lr_owned2, create_reverse_double(NaN, true));
  compare(revdr_owned2 * lr_owned2, create_reverse_double(NaN, true));
  compare(l1 * ir_owned1, create_integer(7, false));
  compare(l2 * ir_owned1, create_integer(1, true));
  compare(i1 * ir_owned1, create_integer(14, false));
  compare(i2 * ir_owned1, create_integer(1, true));
  compare(d1 * ir_owned1, create_double(21.000, false));
  compare(d2 * ir_owned1, create_double(NaN, true));
  compare(d2 * ir_owned1, create_double(NaN, true));
  compare(dua1 * ir_owned1, create_dual(21.000, 28, false, false));
  compare(dua2 * ir_owned1, create_dual(NaN, NaN, true, true));
  compare(dua3 * ir_owned1, create_dual(NaN, NaN, true, true));
  compare(dua4 * ir_owned1, create_dual(21.000, NaN, false, false));
  compare(revd1 * ir_owned1, create_reverse_double(35.000, false));
  compare(revd2 * ir_owned1, create_reverse_double(NaN, true));
  compare(revd3 * ir_owned1, create_reverse_double(NaN, false));
  compare(lr_owned1 * ir_owned1, create_integer(7, false));
  compare(lr_owned2 * ir_owned1, create_integer(1, true));
  compare(ir_owned1 * ir_owned1, create_integer(49, false));
  compare(ir_owned2 * ir_owned1, create_integer(1, true));
  compare(dr_owned1 * ir_owned1, create_double(140.000, false));
  compare(dr_owned2 * ir_owned1, create_double(NaN, true));
  compare(duar_owned1 * ir_owned1, create_dual(14.000, 7, false, false));
  compare(duar_owned2 * ir_owned1, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * ir_owned1, create_dual(21.000, NaN, false, false));
  compare(duar_owned4 * ir_owned1, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * ir_owned1, create_reverse_double(63.000, false));
  compare(revdr_owned2 * ir_owned1, create_reverse_double(NaN, true));
  compare(l1 * ir_owned2, create_integer(1, true));
  compare(l2 * ir_owned2, create_integer(1, true));
  compare(i1 * ir_owned2, create_integer(1, true));
  compare(i2 * ir_owned2, create_integer(1, true));
  compare(d1 * ir_owned2, create_double(NaN, true));
  compare(d2 * ir_owned2, create_double(NaN, true));
  compare(d2 * ir_owned2, create_double(NaN, true));
  compare(dua1 * ir_owned2, create_dual(NaN, NaN, true, true));
  compare(dua2 * ir_owned2, create_dual(NaN, NaN, true, true));
  compare(dua3 * ir_owned2, create_dual(NaN, NaN, true, true));
  compare(dua4 * ir_owned2, create_dual(NaN, NaN, true, true));
  compare(revd1 * ir_owned2, create_reverse_double(NaN, true));
  compare(revd2 * ir_owned2, create_reverse_double(NaN, true));
  compare(revd3 * ir_owned2, create_reverse_double(NaN, true));
  compare(lr_owned1 * ir_owned2, create_integer(1, true));
  compare(lr_owned2 * ir_owned2, create_integer(1, true));
  compare(ir_owned1 * ir_owned2, create_integer(1, true));
  compare(ir_owned2 * ir_owned2, create_integer(1, true));
  compare(dr_owned1 * ir_owned2, create_double(NaN, true));
  compare(dr_owned2 * ir_owned2, create_double(NaN, true));
  compare(duar_owned1 * ir_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * ir_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * ir_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * ir_owned2, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * ir_owned2, create_reverse_double(NaN, true));
  compare(revdr_owned2 * ir_owned2, create_reverse_double(NaN, true));
  compare(l1 * dr_owned1, create_double(20.000, false));
  compare(l2 * dr_owned1, create_double(NaN, true));
  compare(i1 * dr_owned1, create_double(40.000, false));
  compare(i2 * dr_owned1, create_double(NaN, true));
  compare(d1 * dr_owned1, create_double(60.000, false));
  compare(d2 * dr_owned1, create_double(NaN, true));
  compare(d2 * dr_owned1, create_double(NaN, true));
  compare(dua1 * dr_owned1, create_dual(60.000, 80, false, false));
  compare(dua2 * dr_owned1, create_dual(NaN, NaN, true, true));
  compare(dua3 * dr_owned1, create_dual(NaN, NaN, true, true));
  compare(dua4 * dr_owned1, create_dual(60.000, NaN, false, false));
  compare(revd1 * dr_owned1, create_reverse_double(100.000, false));
  compare(revd2 * dr_owned1, create_reverse_double(NaN, true));
  compare(revd3 * dr_owned1, create_reverse_double(NaN, false));
  compare(lr_owned1 * dr_owned1, create_double(20.000, false));
  compare(lr_owned2 * dr_owned1, create_double(NaN, true));
  compare(ir_owned1 * dr_owned1, create_double(140.000, false));
  compare(ir_owned2 * dr_owned1, create_double(NaN, true));
  compare(dr_owned1 * dr_owned1, create_double(400.000, false));
  compare(dr_owned2 * dr_owned1, create_double(NaN, true));
  compare(duar_owned1 * dr_owned1, create_dual(40.000, 20, false, false));
  compare(duar_owned2 * dr_owned1, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * dr_owned1, create_dual(60.000, NaN, false, false));
  compare(duar_owned4 * dr_owned1, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * dr_owned1, create_reverse_double(180.000, false));
  compare(revdr_owned2 * dr_owned1, create_reverse_double(NaN, true));
  compare(l1 * dr_owned2, create_double(NaN, true));
  compare(l2 * dr_owned2, create_double(NaN, true));
  compare(i1 * dr_owned2, create_double(NaN, true));
  compare(i2 * dr_owned2, create_double(NaN, true));
  compare(d1 * dr_owned2, create_double(NaN, true));
  compare(d2 * dr_owned2, create_double(NaN, true));
  compare(d2 * dr_owned2, create_double(NaN, true));
  compare(dua1 * dr_owned2, create_dual(NaN, NaN, true, true));
  compare(dua2 * dr_owned2, create_dual(NaN, NaN, true, true));
  compare(dua3 * dr_owned2, create_dual(NaN, NaN, true, true));
  compare(dua4 * dr_owned2, create_dual(NaN, NaN, true, true));
  compare(revd1 * dr_owned2, create_reverse_double(NaN, true));
  compare(revd2 * dr_owned2, create_reverse_double(NaN, true));
  compare(revd3 * dr_owned2, create_reverse_double(NaN, true));
  compare(lr_owned1 * dr_owned2, create_double(NaN, true));
  compare(lr_owned2 * dr_owned2, create_double(NaN, true));
  compare(ir_owned1 * dr_owned2, create_double(NaN, true));
  compare(ir_owned2 * dr_owned2, create_double(NaN, true));
  compare(dr_owned1 * dr_owned2, create_double(NaN, true));
  compare(dr_owned2 * dr_owned2, create_double(NaN, true));
  compare(duar_owned1 * dr_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * dr_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * dr_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * dr_owned2, create_dual(NaN, NaN, true, true));
  compare(revdr_owned1 * dr_owned2, create_reverse_double(NaN, true));
  compare(revdr_owned2 * dr_owned2, create_reverse_double(NaN, true));
  compare(l1 * duar_owned1, create_dual(2.000, 1, false, false));
  compare(l2 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(i1 * duar_owned1, create_dual(4.000, 2, false, false));
  compare(i2 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(d1 * duar_owned1, create_dual(6.000, 3, false, false));
  compare(d2 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(d2 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(dua1 * duar_owned1, create_dual(6.000, 11, false, false));
  compare(dua2 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(dua3 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(dua4 * duar_owned1, create_dual(6.000, NaN, false, false));
  compare(lr_owned1 * duar_owned1, create_dual(2.000, 1, false, false));
  compare(lr_owned2 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(ir_owned1 * duar_owned1, create_dual(14.000, 7, false, false));
  compare(ir_owned2 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(dr_owned1 * duar_owned1, create_dual(40.000, 20, false, false));
  compare(dr_owned2 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(duar_owned1 * duar_owned1, create_dual(4.000, 4, false, false));
  compare(duar_owned2 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * duar_owned1, create_dual(6.000, NaN, false, false));
  compare(duar_owned4 * duar_owned1, create_dual(NaN, NaN, true, true));
  compare(l1 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(l2 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(i1 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(i2 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(d1 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(d2 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(d2 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(dua1 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(dua2 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(dua3 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(dua4 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(lr_owned1 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(lr_owned2 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(ir_owned1 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(ir_owned2 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(dr_owned1 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(dr_owned2 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned1 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * duar_owned2, create_dual(NaN, NaN, true, true));
  compare(l1 * duar_owned3, create_dual(3.000, NaN, false, false));
  compare(l2 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(i1 * duar_owned3, create_dual(6.000, NaN, false, false));
  compare(i2 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(d1 * duar_owned3, create_dual(9.000, NaN, false, false));
  compare(d2 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(d2 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(dua1 * duar_owned3, create_dual(9.000, NaN, false, false));
  compare(dua2 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(dua3 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(dua4 * duar_owned3, create_dual(9.000, NaN, false, false));
  compare(lr_owned1 * duar_owned3, create_dual(3.000, NaN, false, false));
  compare(lr_owned2 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(ir_owned1 * duar_owned3, create_dual(21.000, NaN, false, false));
  compare(ir_owned2 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(dr_owned1 * duar_owned3, create_dual(60.000, NaN, false, false));
  compare(dr_owned2 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(duar_owned1 * duar_owned3, create_dual(6.000, NaN, false, false));
  compare(duar_owned2 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * duar_owned3, create_dual(9.000, NaN, false, false));
  compare(duar_owned4 * duar_owned3, create_dual(NaN, NaN, true, true));
  compare(l1 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(l2 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(i1 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(i2 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(d1 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(d2 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(d2 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(dua1 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(dua2 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(dua3 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(dua4 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(lr_owned1 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(lr_owned2 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(ir_owned1 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(ir_owned2 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(dr_owned1 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(dr_owned2 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(duar_owned1 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(duar_owned2 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(duar_owned3 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(duar_owned4 * duar_owned4, create_dual(NaN, NaN, true, true));
  compare(l1 * revdr_owned1, create_reverse_double(9.000, false));
  compare(l2 * revdr_owned1, create_reverse_double(NaN, true));
  compare(i1 * revdr_owned1, create_reverse_double(18.000, false));
  compare(i2 * revdr_owned1, create_reverse_double(NaN, true));
  compare(d1 * revdr_owned1, create_reverse_double(27.000, false));
  compare(d2 * revdr_owned1, create_reverse_double(NaN, true));
  compare(d2 * revdr_owned1, create_reverse_double(NaN, true));
  compare(revd1 * revdr_owned1, create_reverse_double(45.000, false));
  compare(revd2 * revdr_owned1, create_reverse_double(NaN, true));
  compare(revd3 * revdr_owned1, create_reverse_double(NaN, false));
  compare(lr_owned1 * revdr_owned1, create_reverse_double(9.000, false));
  compare(lr_owned2 * revdr_owned1, create_reverse_double(NaN, true));
  compare(ir_owned1 * revdr_owned1, create_reverse_double(63.000, false));
  compare(ir_owned2 * revdr_owned1, create_reverse_double(NaN, true));
  compare(dr_owned1 * revdr_owned1, create_reverse_double(180.000, false));
  compare(dr_owned2 * revdr_owned1, create_reverse_double(NaN, true));
  compare(revdr_owned1 * revdr_owned1, create_reverse_double(81.000, false));
  compare(revdr_owned2 * revdr_owned1, create_reverse_double(NaN, true));
  compare(l1 * revdr_owned2, create_reverse_double(NaN, true));
  compare(l2 * revdr_owned2, create_reverse_double(NaN, true));
  compare(i1 * revdr_owned2, create_reverse_double(NaN, true));
  compare(i2 * revdr_owned2, create_reverse_double(NaN, true));
  compare(d1 * revdr_owned2, create_reverse_double(NaN, true));
  compare(d2 * revdr_owned2, create_reverse_double(NaN, true));
  compare(d2 * revdr_owned2, create_reverse_double(NaN, true));
  compare(revd1 * revdr_owned2, create_reverse_double(NaN, true));
  compare(revd2 * revdr_owned2, create_reverse_double(NaN, true));
  compare(revd3 * revdr_owned2, create_reverse_double(NaN, true));
  compare(lr_owned1 * revdr_owned2, create_reverse_double(NaN, true));
  compare(lr_owned2 * revdr_owned2, create_reverse_double(NaN, true));
  compare(ir_owned1 * revdr_owned2, create_reverse_double(NaN, true));
  compare(ir_owned2 * revdr_owned2, create_reverse_double(NaN, true));
  compare(dr_owned1 * revdr_owned2, create_reverse_double(NaN, true));
  compare(dr_owned2 * revdr_owned2, create_reverse_double(NaN, true));
  compare(revdr_owned1 * revdr_owned2, create_reverse_double(NaN, true));
  compare(revdr_owned2 * revdr_owned2, create_reverse_double(NaN, true));
}

// Assign source `s` (numeric value v, derivative dot, NA flag na) to every
// scalar and scalar-reference target, then check the converted result.
// dot is the expected derivative on Dual targets (source dot if s is a Dual,
// 0 otherwise). When na is true compare() short-circuits on NA, so v/dot are
// ignored for those cases.
template<typename Src>
void check_assign_all(const Src& s, double v, double dot, bool na) {
  const int iv  = static_cast<int>(v);
  const bool bv = static_cast<bool>(v);

  // value targets: converting ctor + copy-assign
  Logical       aL;  aL  = s; compare(aL,  create_logical(bv, na));
  Integer       aI;  aI  = s; compare(aI,  create_integer(iv, na));
  Double        aD;  aD  = s; compare(aD,  create_double(v, na));
  Dual          aDu; aDu = s; compare(aDu, create_dual(v, dot, na, na));
  ReverseDouble aRD; aRD = s; compare(aRD, create_reverse_double(v, na));

  // reference targets: assignment writes through the referenced slot
  bool lv = false, lna = false;
  LogicalRef aLR(&lv, &lna); aLR = s; compare(Logical(aLR), create_logical(bv, na));

  int iiv = 0; bool iina = false;
  IntegerRef aIR(&iiv, &iina); aIR = s; compare(Integer(aIR), create_integer(iv, na));

  double ddv = 0.0; bool ddna = false;
  DoubleRef aDR(&ddv, &ddna); aDR = s; compare(Double(aDR), create_double(v, na));

  double duv = 0.0, dudot = 0.0; bool duna = false, dunadot = false;
  DualRef aDuR(&duv, &dudot, &duna, &dunadot); aDuR = s;
  compare(Dual(aDuR), create_dual(v, dot, na, na));

  // ReverseDouble has no reference wrapper
  ReverseDouble aRDR; aRDR = s;
  compare(aRDR, create_reverse_double(v, na));
}

// [[Rcpp::export]]
void test_scalar_assignment() {
  TAPE_INTERN.clear();

  check_assign_all(Logical(true),      1.0, 0.0, false);
  check_assign_all(Integer(4),         4.0, 0.0, false);
  check_assign_all(Double(6.0),        6.0, 0.0, false);
  check_assign_all(Dual(7.0, 2.0),     7.0, 2.0, false);
  check_assign_all(ReverseDouble(8.0), 8.0, 0.0, false);

  check_assign_all(Logical::NA(),       0.0, 0.0, true);
  check_assign_all(Integer::NA(),       0.0, 0.0, true);
  check_assign_all(Double::NA(),        0.0, 0.0, true);
  check_assign_all(Dual::NA(),          0.0, 0.0, true);
  check_assign_all(ReverseDouble::NA(), 0.0, 0.0, true);

  {
    bool sv = true, sna = false, dv = false, dna = true;
    LogicalRef src(&sv, &sna), dst(&dv, &dna);
    dst = src; compare(Logical(dst), create_logical(true, false));
  }
  {
    int sv = 42; bool sna = false; int dv = 0; bool dna = true;
    IntegerRef src(&sv, &sna), dst(&dv, &dna);
    dst = src; compare(Integer(dst), create_integer(42, false));
  }
  {
    double sv = 3.25; bool sna = false; double dv = 0.0; bool dna = true;
    DoubleRef src(&sv, &sna), dst(&dv, &dna);
    dst = src; compare(Double(dst), create_double(3.25, false));
  }
  {
    double sv = 2.0, sdot = 5.0; bool sna = false, snd = false;
    double dv = 0.0, ddot = 0.0; bool dna = true, dnd = true;
    DualRef src(&sv, &sdot, &sna, &snd), dst(&dv, &ddot, &dna, &dnd);
    dst = src; compare(Dual(dst), create_dual(2.0, 5.0, false, false));
  }
  {
    ReverseDouble src = create_reverse_double(9.0, false);
    ReverseDouble dst;
    dst = src; compare(dst, create_reverse_double(9.0, false));
  }
}
