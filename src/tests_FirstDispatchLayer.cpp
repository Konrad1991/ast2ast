// [[Rcpp::depends(RcppEigen)]]"
#include <RcppEigen.h>
#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

void test_scalar_return_types() {
  // Test correct types returned by methods defined in Logical
  {
    Logical l = true;
    Logical r = false;
    static_assert(IS<decltype(l.operator+(r)), Integer>, "Logical + Logical = Integer");
    static_assert(IS<decltype(l.operator-(r)), Integer>, "Logical - Logical = Integer");
    static_assert(IS<decltype(l.operator*(r)), Integer>, "Logical * Logical = Integer");
    static_assert(IS<decltype(l.operator/(r)), Double>, "Logical / Logical = Double");
    static_assert(IS<decltype(l.pow(r)), Double>, "Logical ^ Logical = Double");
    static_assert(IS<decltype(l.operator==(r)), Logical>, "Logical == Logical = Logical");
    static_assert(IS<decltype(l.operator<(r)), Logical>, "Logical < Logical = Logical");
    static_assert(IS<decltype(l.operator<=(r)), Logical>, "Logical <= Logical = Logical");
    static_assert(IS<decltype(l.operator>(r)), Logical>, "Logical > Logical = Logical");
    static_assert(IS<decltype(l.operator>=(r)), Logical>, "Logical >= Logical = Logical");
    static_assert(IS<decltype(l.operator!=(r)), Logical>, "Logical != Logical = Logical");
    static_assert(IS<decltype(l.operator&&(r)), Logical>, "Logical && Logical = Logical");
    static_assert(IS<decltype(l.operator||(r)), Logical>, "Logical || Logical = Logical");
    static_assert(IS<decltype(l.sin()), Double>, "sin(Logical) = Double");
    static_assert(IS<decltype(l.asin()), Double>, "asin(Logical) = Double");
    static_assert(IS<decltype(l.sinh()), Double>, "sinh(Logical) = Double");
    static_assert(IS<decltype(l.cos()), Double>, "cos(Logical) = Double");
    static_assert(IS<decltype(l.acos()), Double>, "acos(Logical) = Double");
    static_assert(IS<decltype(l.cosh()), Double>, "cosh(Logical) = Double");
    static_assert(IS<decltype(l.tan()), Double>, "tan(Logical) = Double");
    static_assert(IS<decltype(l.atan()), Double>, "atan(Logical) = Double");
    static_assert(IS<decltype(l.tanh()), Double>, "tanh(Logical) = Double");
    static_assert(IS<decltype(l.exp()), Double>, "exp(Logical) = Double");
    static_assert(IS<decltype(l.log()), Double>, "log(Logical) = Double");
    static_assert(IS<decltype(l.sqrt()), Double>, "sqrt(Logical) = Double");
    static_assert(IS<decltype(l.operator-()), Integer>, "-Logical = Integer");
  }

  // Test correct types returned by methods defined in Integer
  {
    Integer l = 1;
    Integer r = 2;
    static_assert(IS<decltype(l.operator+(r)), Integer>, "Integer + Integer = Integer");
    static_assert(IS<decltype(l.operator-(r)), Integer>, "Integer - Integer = Integer");
    static_assert(IS<decltype(l.operator*(r)), Integer>, "Integer * Integer = Integer");
    static_assert(IS<decltype(l.operator/(r)), Double>, "Integer / Integer = Double");
    static_assert(IS<decltype(l.pow(r)), Double>, "Integer ^ Integer = Double");
    static_assert(IS<decltype(l.operator==(r)), Logical>, "Integer == Integer = Logical");
    static_assert(IS<decltype(l.operator<(r)), Logical>, "Integer < Integer = Logical");
    static_assert(IS<decltype(l.operator<=(r)), Logical>, "Integer <= Integer = Logical");
    static_assert(IS<decltype(l.operator>(r)), Logical>, "Integer > Integer = Logical");
    static_assert(IS<decltype(l.operator>=(r)), Logical>, "Integer >= Integer = Logical");
    static_assert(IS<decltype(l.operator!=(r)), Logical>, "Integer != Integer = Logical");
    static_assert(IS<decltype(l.operator&&(r)), Logical>, "Integer && Integer = Logical");
    static_assert(IS<decltype(l.operator||(r)), Logical>, "Integer || Integer = Logical");
    static_assert(IS<decltype(l.sin()), Double>, "sin(Integer) = Double");
    static_assert(IS<decltype(l.asin()), Double>, "asin(Integer) = Double");
    static_assert(IS<decltype(l.sinh()), Double>, "sinh(Integer) = Double");
    static_assert(IS<decltype(l.cos()), Double>, "cos(Integer) = Double");
    static_assert(IS<decltype(l.acos()), Double>, "acos(Integer) = Double");
    static_assert(IS<decltype(l.cosh()), Double>, "cosh(Integer) = Double");
    static_assert(IS<decltype(l.tan()), Double>, "tan(Integer) = Double");
    static_assert(IS<decltype(l.atan()), Double>, "atan(Integer) = Double");
    static_assert(IS<decltype(l.tanh()), Double>, "tanh(Integer) = Double");
    static_assert(IS<decltype(l.exp()), Double>, "exp(Integer) = Double");
    static_assert(IS<decltype(l.log()), Double>, "log(Integer) = Double");
    static_assert(IS<decltype(l.sqrt()), Double>, "sqrt(Integer) = Double");
    static_assert(IS<decltype(l.operator-()), Integer>, "-Integer = Integer");
  }
  // Test correct types returned by methods defined in Double
  {
    Double l = 1.1;
    Double r = 2.2;
    static_assert(IS<decltype(l.operator+(r)), Double>, "Double + Double = Double");
    static_assert(IS<decltype(l.operator-(r)), Double>, "Double - Double = Double");
    static_assert(IS<decltype(l.operator*(r)), Double>, "Double * Double = Double");
    static_assert(IS<decltype(l.operator/(r)), Double>, "Double / Double = Double");
    static_assert(IS<decltype(l.pow(r)), Double>, "Double^ Double= Double");
    static_assert(IS<decltype(l.operator==(r)), Logical>, "Double == Double = Logical");
    static_assert(IS<decltype(l.operator<(r)), Logical>, "Double < Double = Logical");
    static_assert(IS<decltype(l.operator<=(r)), Logical>, "Double <= Double = Logical");
    static_assert(IS<decltype(l.operator>(r)), Logical>, "Double > Double = Logical");
    static_assert(IS<decltype(l.operator>=(r)), Logical>, "Double >= Double = Logical");
    static_assert(IS<decltype(l.operator!=(r)), Logical>, "Double != Double = Logical");
    static_assert(IS<decltype(l.operator&&(r)), Logical>, "Double && Double = Logical");
    static_assert(IS<decltype(l.operator||(r)), Logical>, "Double || Double = Logical");
    static_assert(IS<decltype(l.sin()), Double>, "sin(Double) = Double");
    static_assert(IS<decltype(l.asin()), Double>, "asin(Double) = Double");
    static_assert(IS<decltype(l.sinh()), Double>, "sinh(Double) = Double");
    static_assert(IS<decltype(l.cos()), Double>, "cos(Double) = Double");
    static_assert(IS<decltype(l.acos()), Double>, "acos(Double) = Double");
    static_assert(IS<decltype(l.cosh()), Double>, "cosh(Double) = Double");
    static_assert(IS<decltype(l.tan()), Double>, "tan(Double) = Double");
    static_assert(IS<decltype(l.atan()), Double>, "atan(Double) = Double");
    static_assert(IS<decltype(l.tanh()), Double>, "tanh(Double) = Double");
    static_assert(IS<decltype(l.exp()), Double>, "exp(Double) = Double");
    static_assert(IS<decltype(l.log()), Double>, "log(Double) = Double");
    static_assert(IS<decltype(l.sqrt()), Double>, "sqrt(Double) = Double");
    static_assert(IS<decltype(l.operator-()), Double>, "-Double = Double");
  }
  // Test correct types returned by methods defined in Dual
  {
    Dual l(1.1, 0.0);
    Dual r(2.2, 1.0);
    static_assert(IS<decltype(l.operator+(r)), Dual>, "Dual + Dual = Dual");
    static_assert(IS<decltype(l.operator-(r)), Dual>, "Dual - Dual = Dual");
    static_assert(IS<decltype(l.operator*(r)), Dual>, "Dual * Dual = Dual");
    static_assert(IS<decltype(l.operator/(r)), Dual>, "Dual / Dual = Dual");
    static_assert(IS<decltype(l.pow(r)), Dual>, "Dual^ Dual= Dual");
    static_assert(IS<decltype(l.operator==(r)), Logical>, "Dual == Dual = Logical");
    static_assert(IS<decltype(l.operator<(r)), Logical>, "Dual < Dual = Logical");
    static_assert(IS<decltype(l.operator<=(r)), Logical>, "Dual <= Dual = Logical");
    static_assert(IS<decltype(l.operator>(r)), Logical>, "Dual > Dual = Logical");
    static_assert(IS<decltype(l.operator>=(r)), Logical>, "Dual >= Dual = Logical");
    static_assert(IS<decltype(l.operator!=(r)), Logical>, "Dual != Dual = Logical");
    static_assert(IS<decltype(l.operator&&(r)), Logical>, "Dual && Dual = Logical");
    static_assert(IS<decltype(l.operator||(r)), Logical>, "Dual || Dual = Logical");
    static_assert(IS<decltype(l.sin()), Dual>, "sin(Dual) = Dual");
    static_assert(IS<decltype(l.asin()), Dual>, "asin(Dual) = Dual");
    static_assert(IS<decltype(l.sinh()), Dual>, "sinh(Dual) = Dual");
    static_assert(IS<decltype(l.cos()), Dual>, "cos(Dual) = Dual");
    static_assert(IS<decltype(l.acos()), Dual>, "acos(Dual) = Dual");
    static_assert(IS<decltype(l.cosh()), Dual>, "cosh(Dual) = Dual");
    static_assert(IS<decltype(l.tan()), Dual>, "tan(Dual) = Dual");
    static_assert(IS<decltype(l.atan()), Dual>, "atan(Dual) = Dual");
    static_assert(IS<decltype(l.tanh()), Dual>, "tanh(Dual) = Dual");
    static_assert(IS<decltype(l.exp()), Dual>, "exp(Dual) = Dual");
    static_assert(IS<decltype(l.log()), Dual>, "log(Dual) = Dual");
    static_assert(IS<decltype(l.sqrt()), Dual>, "sqrt(Dual) = Dual");
    static_assert(IS<decltype(l.operator-()), Dual>, "-Dual = Dual");
  }
}

void test_scalar_methods() {
  auto compare = [](auto l, auto r) {
    double TOL = 1E-03;
    return std::abs(l - r) < TOL;
  };

  // Logical methods
  {
    Logical l = true;
    Logical r = false;
    ass<"true + false = 1">( (l.operator+(r)).val == 1);
    ass<"true + true = 2">( (l.operator+(l)).val == 2);
    ass<"false + false = 0">( (r.operator+(r)).val == 0);

    ass<"true - false = 1">( (l.operator-(r)).val == 1);
    ass<"true - true = 0">( (l.operator-(l)).val == 0);
    ass<"false - false = 0">( (r.operator-(r)).val == 0);

    ass<"true * false = 0">( (l.operator*(r)).val == 0);
    ass<"true * true = 1">( (l.operator*(l)).val == 1);
    ass<"false * false = 0">( (r.operator*(r)).val == 0);

    ass<"true / false = Inf">( !(l.operator/(r)).isFinite() );
    ass<"true / true = 1">( (l.operator/(l)).val == 1);
    ass<"false / false = NaN">( (r.operator/(r)).isNaN() );

    ass<"true ^ false = 1">( (l.pow(r)).val == 1);
    ass<"true ^ true = 1">( (l.pow(l)).val == 1);
    ass<"false ^ false = 1">( (r.pow(r)).val == 1);
    ass<"false ^ true= 0">( (r.pow(l)).val == 0);

    ass<"true == false = false">( (l.operator==(r)).val == false);
    ass<"true == true = true">( (l.operator==(l)).val == true);
    ass<"false == false = true">( (r.operator==(r)).val == true);

    ass<"true < false = false">( (l.operator<(r)).val == false);
    ass<"false < true = true">( (r.operator<(l)).val == true);
    ass<"true < true = false">( (l.operator<(l)).val == false);
    ass<"false < false = false">( (r.operator<(r)).val == false);

    ass<"true <= false = false">( (l.operator<=(r)).val == false);
    ass<"false <= true = true">( (r.operator<=(l)).val == true);
    ass<"true <= true = true">( (l.operator<=(l)).val == true);
    ass<"false <= false = true">( (r.operator<=(r)).val == true);

    ass<"true > false = true">( (l.operator>(r)).val == true);
    ass<"false > true = false">( (r.operator>(l)).val == false);
    ass<"true > true = false">( (l.operator>(l)).val == false);
    ass<"false > false = false">( (r.operator>(r)).val == false);

    ass<"true >= false = true">( (l.operator>=(r)).val == true);
    ass<"false >= true = false">( (r.operator>=(l)).val == false);
    ass<"true >= true = true">( (l.operator>=(l)).val == true);
    ass<"false >= false = true">( (r.operator>=(r)).val == true);

    ass<"true != false = true">( (l.operator!=(r)).val == true);
    ass<"false != true = true">( (r.operator!=(l)).val == true);
    ass<"true != true = false">( (l.operator!=(l)).val == false);
    ass<"false != false = false">( (r.operator!=(r)).val == false);

    ass<"true && false = false">( (l.operator&&(r)).val == false);
    ass<"false && true = false">( (r.operator&&(l)).val == false);
    ass<"true && true = true">( (l.operator&&(l)).val == true);
    ass<"false && false = false">( (r.operator&&(r)).val == false);

    ass<"true || false = true">( (l.operator||(r)).val == true);
    ass<"false || true = true">( (r.operator||(l)).val == true);
    ass<"true || true = true">( (l.operator||(l)).val == true);
    ass<"false || false = false">( (r.operator||(r)).val == false);

    ass<"sin(true)">( compare(l.sin().val, std::sin(1.0)));
    ass<"sin(false)">( compare(r.sin().val, std::sin(0.0)));
    ass<"asin(true)">( compare(l.asin().val, std::asin(1.0)));
    ass<"asin(false)">( compare(r.asin().val, std::asin(0.0)));
    ass<"sinh(true)">( compare(l.sinh().val, std::sinh(1.0)));
    ass<"sinh(false)">( compare(r.sinh().val, std::sinh(0.0)));
    ass<"cos(true)">( compare(l.cos().val, std::cos(1.0)));
    ass<"cos(false)">( compare(r.cos().val, std::cos(0.0)));
    ass<"acos(true)">( compare(l.acos().val, std::acos(1.0)));
    ass<"acos(false)">( compare(r.acos().val, std::acos(0.0)));
    ass<"cosh(true)">( compare(l.cosh().val, std::cosh(1.0)));
    ass<"cosh(false)">( compare(r.cosh().val, std::cosh(0.0)));
    ass<"tan(true)">( compare(l.tan().val, std::tan(1.0)));
    ass<"tan(false)">( compare(r.tan().val, std::tan(0.0)));
    ass<"atan(true)">( compare(l.atan().val, std::atan(1.0)));
    ass<"atan(false)">( compare(r.atan().val, std::atan(0.0)));
    ass<"tanh(true)">( compare(l.tanh().val, std::tanh(1.0)));
    ass<"tanh(false)">( compare(r.tanh().val, std::tanh(0.0)));
    ass<"exp(true)">( compare(l.exp().val, std::exp(1.0)));
    ass<"exp(false)">( compare(r.exp().val, std::exp(0.0)));
    ass<"log(true)">( compare(l.log().val, std::log(1.0)));
    ass<"log(false)">( (r.log() == -Double::Inf()).val );
    ass<"sqrt(true)">( compare(l.sqrt().val, std::sqrt(1.0)));
    ass<"sqrt(false)">( compare(r.sqrt().val, std::sqrt(0.0)));
    ass<"-true">( l.operator-().val == -1);
    ass<"-false">( r.operator-().val == 0);
  }
  // Integer methods
  {
    Integer l = 1;
    Integer r = 2;
    ass<"1 + 2 = 3">( (l.operator+(r)).val == 3);
    ass<"1 - 2 = -1">( (l.operator-(r)).val == -1);
    ass<"2 * 2 = 4">( (r.operator*(r)).val == 4);
    ass<"1 / 2 = 0.5">( compare((l.operator/(r)).val, 0.5));
    ass<"2 ^ 2 = 4">( r.pow(r).val == 4);

    ass<"1 == 2 = false">( l.operator==(r).val == false);
    ass<"1 < 2 = true">( l.operator<(r).val == true);
    ass<"1 <= 1 = true">( l.operator<=(l).val == true);
    ass<"1 > 2 = false">( l.operator>(r).val == false);
    ass<"1 >= 2 = false">( l.operator>=(r).val == false);
    ass<"1 != 2 = true">( l.operator!=(r).val == true);
    Integer z = 0;
    ass<"0 && 1 = false">( z.operator&&(l).val == false);
    ass<"1 && 1 = true">( l.operator&&(l).val == true);
    ass<"0 || 1 = true">( z.operator||(l).val == true);
    ass<"1 || 1 = true">( l.operator||(l).val == true);

    ass<"sin(1)">( compare(l.sin().val, std::sin(1.0)));
    ass<"sin(2)">( compare(r.sin().val, std::sin(2.0)));
    ass<"asin(1)">( compare(l.asin().val, std::asin(1.0)));
    ass<"asin(2)">( r.asin().isNaN() );
    ass<"sinh(1)">( compare(l.sinh().val, std::sinh(1.0)));
    ass<"sinh(2)">( compare(r.sinh().val, std::sinh(2.0)));
    ass<"cos(1)">( compare(l.cos().val, std::cos(1.0)));
    ass<"cos(2)">( compare(r.cos().val, std::cos(2.0)));
    ass<"acos(1)">( compare(l.acos().val, std::acos(1.0)));
    ass<"acos(2)">( r.acos().isNaN() );
    ass<"cosh(1)">( compare(l.cosh().val, std::cosh(1.0)));
    ass<"cosh(2)">( compare(r.cosh().val, std::cosh(2.0)));
    ass<"tan(1)">( compare(l.tan().val, std::tan(1.0)));
    ass<"tan(2)">( compare(r.tan().val, std::tan(2.0)));
    ass<"atan(1)">( compare(l.atan().val, std::atan(1.0)));
    ass<"atan(2)">( compare(r.atan().val, std::atan(2.0)));
    ass<"tanh(1)">( compare(l.tanh().val, std::tanh(1.0)));
    ass<"tanh(2)">( compare(r.tanh().val, std::tanh(2.0)));
    ass<"exp(1)">( compare(l.exp().val, std::exp(1.0)));
    ass<"exp(2)">( compare(r.exp().val, std::exp(2.0)));
    ass<"log(1)">( compare(l.log().val, std::log(1.0)));
    ass<"log(0)">( (z.log() == -Double::Inf()).val);
    ass<"sqrt(1)">( compare(l.sqrt().val, std::sqrt(1.0)));
    ass<"sqrt(2)">( compare(r.sqrt().val, std::sqrt(2.0)));
    ass<"-2">( r.operator-().val == -2);
  }
  // Double methods
  {
    Double l = 1.1;
    Double r = 2.2;
    ass<"1.1 + 2.2 = 3.3">( compare( (l.operator+(r)).val, 3.3) );
    ass<"1.1 - 2.2 = -1.1">( compare( (l.operator-(r)).val, -1.1) );
    ass<"2.2 * 2.2 = 4.84">( compare( (r.operator*(r)).val, 4.84) );
    ass<"1.1 / 2.2 = 0.5">( compare( (l.operator/(r)).val, 0.5));
    ass<"2.2 ^ 2.2 = 5.66">( compare( r.pow(r).val, std::pow(2.2, 2.2)) );

    ass<"1.1 == 2.2 = false">( l.operator==(r).val == false);
    ass<"1.1 < 2.2 = true">( l.operator<(r).val == true);
    ass<"1.1 <= 1.1 = true">( l.operator<=(l).val == true);
    ass<"1.1 > 2.2 = false">( l.operator>(r).val == false);
    ass<"1.1 >= 2.2 = false">( l.operator>=(r).val == false);
    ass<"1.1 != 2.2 = true">( l.operator!=(r).val == true);
    Double z = 0.0;
    ass<"0.0 && 1.1 = false">( z.operator&&(l).val == false);
    ass<"1.1 && 1.1 = true">( l.operator&&(l).val == true);
    ass<"0.0 || 1.1 = true">( z.operator||(l).val == true);
    ass<"1.1 || 1.1 = true">( l.operator||(l).val == true);

    ass<"sin(1.1)">( compare(l.sin().val, std::sin(1.1)));
    ass<"sin(2.2)">( compare(r.sin().val, std::sin(2.2)));
    Double valid = 0.5;
    ass<"asin(0.5)">( compare(valid.asin().val, std::asin(0.5)));
    ass<"asin(2.2)">( r.asin().isNaN());
    ass<"sinh(1.1)">( compare(l.sinh().val, std::sinh(1.1)));
    ass<"sinh(2.2)">( compare(r.sinh().val, std::sinh(2.2)));
    ass<"cos(1.1)">( compare(l.cos().val, std::cos(1.1)));
    ass<"cos(2.2)">( compare(r.cos().val, std::cos(2.2)));
    ass<"acos(0.5)">( compare(valid.acos().val, std::acos(0.5)));
    ass<"acos(2.2)">( r.acos().isNaN() );
    ass<"cosh(1.1)">( compare(l.cosh().val, std::cosh(1.1)));
    ass<"cosh(2.2)">( compare(r.cosh().val, std::cosh(2.2)));
    ass<"tan(1.1)">( compare(l.tan().val, std::tan(1.1)));
    ass<"tan(2.2)">( compare(r.tan().val, std::tan(2.2)));
    ass<"atan(1.1)">( compare(l.atan().val, std::atan(1.1)));
    ass<"atan(2.2)">( compare(r.atan().val, std::atan(2.2)));
    ass<"tanh(1.1)">( compare(l.tanh().val, std::tanh(1.1)));
    ass<"tanh(2.2)">( compare(r.tanh().val, std::tanh(2.2)));
    ass<"exp(1.1)">( compare(l.exp().val, std::exp(1.1)));
    ass<"exp(2.2)">( compare(r.exp().val, std::exp(2.2)));
    ass<"log(1.1)">( compare(l.log().val, std::log(1.1)));
    ass<"log(0.0)">( (z.log() == -Double::Inf()).val );
    ass<"sqrt(1.1)">( compare(l.sqrt().val, std::sqrt(1.1)));
    ass<"sqrt(2.2)">( compare(r.sqrt().val, std::sqrt(2.2)));
    ass<"-2.2">( compare(r.operator-().val, -2.2));
  }

  // Dual methods
  {
    Dual l(1.1, 1.0);
    Dual r(2.2, 0.0);
    ass<"1.1 + 2.2 = 3.3">( compare( (l.operator+(r)).val, 3.3) );
    ass<"deriv: 1.1 + 2.2 = 1.0">( compare( (l.operator+(r)).dot, 1.0) );

    ass<"1.1 - 2.2 = -1.1">( compare( (l.operator-(r)).val, -1.1) );
    ass<"deriv: 1.1 - 2.2 = 1.0">( compare( (l.operator-(r)).dot, 1.0) );
    ass<"deriv: 2.2 - 1.0 = -1.0">( compare( (r.operator-(l)).dot, -1.0) );

    ass<"1.1 * 2.2 = 2.42">( compare( (l.operator*(r)).val, 2.42) );
    ass<"deriv: 1.1 * 2.2 = 2.2">( compare( (l.operator*(r)).dot, 2.2) );

    ass<"1.1 / 2.2 = 0.5">( compare( (l.operator/(r)).val, 0.5));
    ass<"deriv: 1.1 / (1.1*1.1) = 1 / (1.1*1.1)">( compare( (l.operator/(l.operator*(l))).dot,
                                                           -1.0 / (1.1 * 1.1)));

    ass<"1.1 ^ 1.1 = 1.11...">( compare( l.pow(l).val, std::pow(1.1, 1.1)) );
    ass<"deriv: 1.1 ^ 1.1 = x^x + x^x*ln(x)">( compare( l.pow(l).dot,
                                              std::pow(1.1, 1.1) + std::pow(1.1, 1.1) * std::log(1.1)) );

    ass<"1.1 == 2.2 = false">( l.operator==(r).val == false);
    ass<"1.1 < 2.2 = true">( l.operator<(r).val == true);
    ass<"1.1 <= 1.1 = true">( l.operator<=(l).val == true);
    ass<"1.1 > 2.2 = false">( l.operator>(r).val == false);
    ass<"1.1 >= 2.2 = false">( l.operator>=(r).val == false);
    ass<"1.1 != 2.2 = true">( l.operator!=(r).val == true);
    Dual z(0.0, 0.0);
    ass<"0.0 && 1.1 = false">( z.operator&&(l).val == false);
    ass<"1.1 && 1.1 = true">( l.operator&&(l).val == true);
    ass<"0.0 || 1.1 = true">( z.operator||(l).val == true);
    ass<"1.1 || 1.1 = true">( l.operator||(l).val == true);

    ass<"sin(1.1)">( compare(l.sin().val, std::sin(1.1)));
    ass<"deriv: sin(1.1) = cos(1.1)">( compare(l.sin().dot, std::cos(1.1)));

    Dual valid(0.5, 1.0);
    ass<"asin(0.5)">( compare(valid.asin().val, std::asin(0.5)));
    ass<"deriv: asin(0.5) = 1 / sqrt(1 - x^2)">( compare(valid.asin().dot,
                                                 1.0 / std::sqrt(1.0 - 0.5*0.5)) );
    ass<"asin(2.2)">( (r.asin().val));

    ass<"sinh(1.1)">( compare(l.sinh().val, std::sinh(1.1)));
    ass<"deriv: sinh(1.1) = cosh(1.1)">( compare(l.sinh().dot, std::cosh(1.1)));

    ass<"cos(1.1)">( compare(l.cos().val, std::cos(1.1)));
    ass<"deriv: cos(1.1) = -sin(1.1)">( compare(l.cos().dot, -std::sin(1.1)));

    ass<"acos(0.5)">( compare(valid.acos().val, std::acos(0.5)));
    ass<"deriv: acos(0.5) = -1 / sqrt(1 - x^2)">( compare(valid.acos().dot,
                                                 -1.0 / std::sqrt(1.0 - 0.5*0.5)) );
    ass<"acos(2.2)">( (r.acos().val));

    ass<"cosh(1.1)">( compare(l.cosh().val, std::cosh(1.1)));
    ass<"deriv: cosh(1.1) = sinh(1.1)">( compare(l.cosh().dot, std::sinh(1.1)));

    ass<"tan(1.1)">( compare(l.tan().val, std::tan(1.1)));
    ass<"deriv: tan(1.1) = 1 + tan(1.1)*tan(1.1)">( compare(l.tan().dot, 1.0 + std::tan(1.1)*std::tan(1.1)) );

    ass<"atan(1.1)">( compare(l.atan().val, std::atan(1.1)));
    ass<"deriv: atan(1.1) = 1 / (1 + 1.1*1.1)">( compare(l.atan().dot, 1.0 / (1 + 1.1*1.1)) );

    ass<"tanh(1.1)">( compare(l.tanh().val, std::tanh(1.1)));
    ass<"deriv: tanh(1.1) = 1 / (cosh(1.1)*cosh(1.1))">( compare(l.tanh().dot, 1.0 / (std::cosh(1.1) * std::cosh(1.1))) );

    ass<"exp(1.1)">( compare(l.exp().val, std::exp(1.1)));
    ass<"deriv: exp(1.1)">( compare(l.exp().dot, std::exp(1.1)));

    ass<"log(1.1)">( compare(l.log().val, std::log(1.1)));
    ass<"deriv: log(1.1) = 1/1.1">( compare(l.log().dot, 1.0 / 1.1) );
    ass<"log(0.0)">( z.log().val == -Double::Inf().val );
    ass<"log(0.0)">( std::isnan(z.log().dot) );
    ass<"log(0.0)">( z.log().isNaNDot() );
    ass<"log(0.0)">( !(z.log().isFinite()));

    ass<"sqrt(1.1)">( compare(l.sqrt().val, std::sqrt(1.1)));
    ass<"deriv: sqrt(1.1) = 1 / (2*sqrt(1.1))">( compare(l.sqrt().dot, 1.0 / (2.0*std::sqrt(1.1))) );

    ass<"-1.1">( compare(l.operator-().val, -1.1));
    ass<"deriv: -1.1 = -1.0">( compare(l.operator-().dot, -1.0));
  }
  // NA
  {
    ass<"NA + TRUE = NA">( (Logical::NA() + Logical(true)).isNA() );
    ass<"NA * 2L = NA">( (Integer::NA() *Integer(2)).isNA() );
    ass<"sin(NA) = NA">( (Double::NA().sin()).isNA() );
  }
}

// [[Rcpp::export]]
void test_first_dispatch_layer() {
  test_scalar_return_types();
  test_scalar_methods();
}
