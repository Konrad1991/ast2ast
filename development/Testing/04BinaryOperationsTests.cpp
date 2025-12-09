#include "etr.hpp"
#include <stdexcept>
#include <type_traits>
using namespace etr;

static inline bool approx(double a, double b, double tol = 1e-12) {
  return std::fabs(a - b) <= tol;
}

// [[Rcpp::export]]
void test_arithmetic_assign_to_buffer() {
  // l vec & l vec
  {
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> b; b = c(4, 5, 6);
    Vec<double> c;
    c = a + b;
    ass<"l vec & lvec: 5">(c[0] == 5);
    ass<"lvec & lvec: 7">(c[1] == 7);
    ass<"lvec & lvec: 9">(c[2] == 9);
    c = a - b;
    ass<"lvec & lvec: -3">(c[0] == -3);
    ass<"lvec & lvec: -3">(c[1] == -3);
    ass<"lvec & lvec: -3">(c[2] == -3);
    c = a * b;
    ass<"lvec & lvec: 4">(c[0] == 4);
    ass<"lvec & lvec: 10">(c[1] == 10);
    ass<"lvec & lvec: 18">(c[2] == 18);
    c = a / b;
    ass<"lvec & lvec: 1/4">(c[0] == 0.25);
    ass<"lvec & lvec: 2/5">(c[1] == 0.4);
    ass<"lvec & lvec: 1/2">(c[2] == 0.5);
    c = power(a, b);
    ass<"lvec & lvec: 1">(c[0] == 1);
    ass<"lvec & lvec: 2^5">(c[1] == 32);
    ass<"lvec & lvec: 3^6">(c[2] == 729);
  }
  // r vec & l vec
  {
    Vec<double> b; b = c(4, 5, 6);
    Vec<double> c;
    c = etr::c(1.0, 2.0, 3.0) + b;
    ass<"r vec & lvec: 5">(c[0] == 5);
    ass<"r vec & lvec: 7">(c[1] == 7);
    ass<"r vec & lvec: 9">(c[2] == 9);
    c = etr::c(1.0, 2.0, 3.0) - b;
    ass<"r vec & lvec: -3">(c[0] == -3);
    ass<"r vec & lvec: -3">(c[1] == -3);
    ass<"r vec & lvec: -3">(c[2] == -3);
    c = etr::c(1.0, 2.0, 3.0) * b;
    ass<"r vec & lvec: 4">(c[0] == 4);
    ass<"r vec & lvec: 10">(c[1] == 10);
    ass<"r vec & lvec: 18">(c[2] == 18);
    c = etr::c(1.0, 2.0, 3.0) / b;
    ass<"r vec & lvec: 1/4">(c[0] == 0.25);
    ass<"r vec & lvec: 2/5">(c[1] == 0.4);
    ass<"r vec & lvec: 1/2">(c[2] == 0.5);
    c = power(etr::c(1.0, 2.0, 3.0), b);
    ass<"r vec & lvec: 1">(c[0] == 1);
    ass<"r vec & lvec: 2^5">(c[1] == 32);
    ass<"r vec & lvec: 3^6">(c[2] == 729);
  }
  // l vec & r vec
  {
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> c;
    c = a + etr::c(4, 5, 6);
    ass<"l vec & r vec: 5">(c[0] == 5);
    ass<"l vec & r vec: 7">(c[1] == 7);
    ass<"l vec & r vec: 9">(c[2] == 9);
    c = a - etr::c(4, 5, 6);
    ass<"l vec & r vec: -3">(c[0] == -3);
    ass<"l vec & r vec: -3">(c[1] == -3);
    ass<"l vec & r vec: -3">(c[2] == -3);
    c = a * etr::c(4, 5, 6);
    ass<"l vec & r vec: 4">(c[0] == 4);
    ass<"l vec & r vec: 10">(c[1] == 10);
    ass<"l vec & r vec: 18">(c[2] == 18);
    c = a / etr::c(4, 5, 6);
    ass<"l vec & r vec: 1/4">(c[0] == 0.25);
    ass<"l vec & r vec: 2/5">(c[1] == 0.4);
    ass<"l vec & r vec: 1/2">(c[2] == 0.5);
    c = power(a, etr::c(4, 5, 6));
    ass<"l vec & r vec: 1">(c[0] == 1);
    ass<"l vec & r vec: 2^5">(c[1] == 32);
    ass<"l vec & r vec: 3^6">(c[2] == 729);
  }
  // r vec & r vec
  {
    Vec<double> c;
    c = etr::c(1, 2, 3) + etr::c(4, 5, 6);
    ass<"r vec & r vec: 5">(c[0] == 5);
    ass<"r vec & r vec: 7">(c[1] == 7);
    ass<"r vec & r vec: 9">(c[2] == 9);
    c = etr::c(1, 2, 3) - etr::c(4, 5, 6);
    ass<"r vec & r vec: -3">(c[0] == -3);
    ass<"r vec & r vec: -3">(c[1] == -3);
    ass<"r vec & r vec: -3">(c[2] == -3);
    c = etr::c(1, 2, 3) * etr::c(4, 5, 6);
    ass<"r vec & r vec: 4">(c[0] == 4);
    ass<"r vec & r vec: 10">(c[1] == 10);
    ass<"r vec & r vec: 18">(c[2] == 18);
    c = etr::c(1, 2, 3) / etr::c(4, 5, 6);
    ass<"r vec & r vec: 1/4">(c[0] == 0.25);
    ass<"r vec & r vec: 2/5">(c[1] == 0.4);
    ass<"r vec & r vec: 1/2">(c[2] == 0.5);
    c = power(etr::c(1, 2, 3), etr::c(4, 5, 6));
    ass<"r vec & r vec: 1">(c[0] == 1);
    ass<"r vec & r vec: 2^5">(c[1] == 32);
    ass<"r vec & r vec: 3^6">(c[2] == 729);
  }
  // l vec & scalar
  {
    Vec<double> a = c(1, 2, 3);
    Vec<double> c;
    c = a + 1.0;
    ass<"l vec & scalar: 2">(c[0] == 2);
    ass<"l vec & scalar: 3">(c[1] == 3);
    ass<"l vec & scalar: 4">(c[2] == 4);
    c = a - 1.0;
    ass<"l vec & scalar: 0">(c[0] == 0);
    ass<"l vec & scalar: 1">(c[1] == 1);
    ass<"l vec & scalar: 2">(c[2] == 2);
    c = a * 2.0;
    ass<"l vec & scalar: 2">(c[0] == 2);
    ass<"l vec & scalar: 4">(c[1] == 4);
    ass<"l vec & scalar: 6">(c[2] == 6);
    c = a / 2.0;
    ass<"l vec & scalar: 1/2">(c[0] == 0.5);
    ass<"l vec & scalar: 2/2">(c[1] == 1.0);
    ass<"l vec & scalar: 3/2">(c[2] == 1.5);
    c = power(a, 2.0);
    ass<"l vec & scalar: 1^2">(c[0] == 1);
    ass<"l vec & scalar: 2^2">(c[1] == 4);
    ass<"l vec & scalar: 3^2">(c[2] == 9);
  }
  // r vec & scalar
  {
    Vec<double> c;
    c = etr::c(1, 2, 3) + 1.0;
    ass<"r vec & scalar: 2">(c[0] == 2);
    ass<"r vec & scalar: 3">(c[1] == 3);
    ass<"r vec & scalar: 4">(c[2] == 4);
    c = etr::c(1, 2, 3) - 1.0;
    ass<"r vec & scalar: 0">(c[0] == 0);
    ass<"r vec & scalar: 1">(c[1] == 1);
    ass<"r vec & scalar: 2">(c[2] == 2);
    c = etr::c(1, 2, 3)* 2.0;
    ass<"r vec & scalar: 2">(c[0] == 2);
    ass<"r vec & scalar: 4">(c[1] == 4);
    ass<"r vec & scalar: 6">(c[2] == 6);
    c = etr::c(1, 2, 3)/ 2.0;
    ass<"r vec & scalar: 1/2">(c[0] == 0.5);
    ass<"r vec & scalar: 2/2">(c[1] == 1.0);
    ass<"r vec & scalar: 3/2">(c[2] == 1.5);
    c = power(etr::c(1, 2, 3), 2.0);
    ass<"r vec & scalar: 1^2">(c[0] == 1);
    ass<"r vec & scalar: 2^2">(c[1] == 4);
    ass<"r vec & scalar: 3^2">(c[2] == 9);
  }
  // scalar & l vec
  {
    Vec<double> a = c(1, 2, 3);
    Vec<double> c;
    c = 1.0 + a;
    ass<"scalar & l vec: 2">(c[0] == 2);
    ass<"scalar & l vec: 3">(c[1] == 3);
    ass<"scalar & l vec: 4">(c[2] == 4);
    c = 1.0 - a;
    ass<"scalar & l vec: 0">(c[0] == 0);
    ass<"scalar & l vec: 1">(c[1] == -1);
    ass<"scalar & l vec: 2">(c[2] == -2);
    c = 2.0*a;
    ass<"scalar & l vec: 2">(c[0] == 2);
    ass<"scalar & l vec: 4">(c[1] == 4);
    ass<"scalar & l vec: 6">(c[2] == 6);
    c = 6.0 / a;
    ass<"scalar & l vec: 6/1">(c[0] == 6.0);
    ass<"scalar & l vec: 6/2">(c[1] == 3.0);
    ass<"scalar & l vec: 6/3">(c[2] == 2.0);
    c = power(2.0, a);
    ass<"scalar & l vec: 2^1">(c[0] == 2);
    ass<"scalar & l vec: 2^2">(c[1] == 4);
    ass<"scalar & l vec: 2^3">(c[2] == 8);
  }
  // scalar & r vec
  {
    Vec<double> c;
    c = 1.0 + etr::c(1, 2, 3);
    ass<"scalar & r vec: 2">(c[0] == 2);
    ass<"scalar & r vec: 3">(c[1] == 3);
    ass<"scalar & r vec: 4">(c[2] == 4);
    c = 1.0 - etr::c(1, 2, 3);
    ass<"scalar & r vec: 0">(c[0] == 0);
    ass<"scalar & r vec: 1">(c[1] == -1);
    ass<"scalar & r vec: 2">(c[2] == -2);
    c = 2.0*etr::c(1, 2, 3);
    ass<"scalar & r vec: 2">(c[0] == 2);
    ass<"scalar & r vec: 4">(c[1] == 4);
    ass<"scalar & r vec: 6">(c[2] == 6);
    c = 6.0 / etr::c(1, 2, 3);
    ass<"scalar & r vec: 6/1">(c[0] == 6.0);
    ass<"scalar & r vec: 6/2">(c[1] == 3.0);
    ass<"scalar & r vec: 6/3">(c[2] == 2.0);
    c = power(2.0, etr::c(1, 2, 3));
    ass<"scalar & r vec: 2^1">(c[0] == 2);
    ass<"scalar & r vec: 2^2">(c[1] == 4);
    ass<"scalar & r vec: 2^3">(c[2] == 8);
  }
  // l vec & l borrow
  {
    Vec<double> a; a = c(1, 2, 3);
    double buf[3] = {4.0, 5.0, 6.0};
    Vec<double, Borrow<double>> b(buf, 3);
    Vec<double> c;
    c = a + b;
    ass<"l vec & l borrow: 5">(c[0] == 5);
    ass<"l vec & l borrow: 7">(c[1] == 7);
    ass<"l vec & l borrow: 9">(c[2] == 9);
    c = a - b;
    ass<"l vec & l borrow: -3">(c[0] == -3);
    ass<"l vec & l borrow: -3">(c[1] == -3);
    ass<"l vec & l borrow: -3">(c[2] == -3);
    c = a * b;
    ass<"l vec & l borrow: 4">(c[0] == 4);
    ass<"l vec & l borrow: 10">(c[1] == 10);
    ass<"l vec & l borrow: 18">(c[2] == 18);
    c = a / b;
    ass<"l vec & l borrow: 1/4">(c[0] == 0.25);
    ass<"l vec & l borrow: 2/5">(c[1] == 0.4);
    ass<"l vec & l borrow: 1/2">(c[2] == 0.5);
    c = power(a, b);
    ass<"l vec & l borrow: 1">(c[0] == 1);
    ass<"l vec & l borrow: 2^5">(c[1] == 32);
    ass<"l vec & l borrow: 3^6">(c[2] == 729);
  }
  // l vec & subset
  {
    Vec<double> a; a = c(1, 2, 3);
    Vec<int> indices = c(1, 2, 3);
    Vec<double> b; b = c(4, 5, 6);
    Vec<double> c;
    c = a + subset(b, indices);
    ass<"l vec & subset: 5">(c[0] == 5);
    ass<"l vec & subset: 7">(c[1] == 7);
    ass<"l vec & subset: 9">(c[2] == 9);
    c = a - subset(b, indices);
    ass<"l vec & subset: -3">(c[0] == -3);
    ass<"l vec & subset: -3">(c[1] == -3);
    ass<"l vec & subset: -3">(c[2] == -3);
    c = a * subset(b, indices);
    ass<"l vec & subset: 4">(c[0] == 4);
    ass<"l vec & subset: 10">(c[1] == 10);
    ass<"l vec & subset: 18">(c[2] == 18);
    c = a / subset(b, indices);
    ass<"l vec & subset: 1/4">(c[0] == 0.25);
    ass<"l vec & subset: 2/5">(c[1] == 0.4);
    ass<"l vec & subset: 1/2">(c[2] == 0.5);
    c = power(a, subset(b, indices));
    ass<"l vec & subset: 1">(c[0] == 1);
    ass<"l vec & subset: 2^5">(c[1] == 32);
    ass<"l vec & subset: 3^6">(c[2] == 729);
  }
  // l vec & binary op
  {
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> b; b = c(4, 5, 6);
    Vec<double> c;
    c = a + (b + 0.0);
    ass<"l vec & binary op: 5">(c[0] == 5);
    ass<"l vec & binary op: 7">(c[1] == 7);
    ass<"l vec & binary op: 9">(c[2] == 9);
    c = a - (b + 0.0);
    ass<"l vec & binary op: -3">(c[0] == -3);
    ass<"l vec & binary op: -3">(c[1] == -3);
    ass<"l vec & binary op: -3">(c[2] == -3);
    c = a * (b + 0.0);
    ass<"l vec & binary op: 4">(c[0] == 4);
    ass<"l vec & binary op: 10">(c[1] == 10);
    ass<"l vec & binary op: 18">(c[2] == 18);
    c = a / (b + 0.0);
    ass<"l vec & binary op: 1/4">(c[0] == 0.25);
    ass<"l vec & binary op: 2/5">(c[1] == 0.4);
    ass<"l vec & binary op: 3/2">(c[2] == 0.5);
    c = power(a, b + 0.0);
    ass<"l vec & binary op: 1">(c[0] == 1);
    ass<"l vec & binary op: 2^5">(c[1] == 32);
    ass<"l vec & binary op: 3^6">(c[2] == 729);
  }
  // l vec & unary op
  {
    Vec<double> pi2 = 0.5*3.141593;
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> c;
    c = a + sinus(pi2);
    ass<"l vec & unary op: 2">(approx(c[0], 2));
    ass<"l vec & unary op: 3">(approx(c[1], 3));
    ass<"l vec & unary op: 4">(approx(c[2], 4));
    c = a - sinus(pi2);
    ass<"l vec & unary op: 0">(approx(c[0], 0));
    ass<"l vec & unary op: 1">(approx(c[1], 1));
    ass<"l vec & unary op: 2">(approx(c[2], 2));
    c = a * sinus(pi2);
    ass<"l vec & unary op: 1">(approx(c[0], 1));
    ass<"l vec & unary op: 2">(approx(c[1], 2));
    ass<"l vec & unary op: 3">(approx(c[2], 3));
    c = a / sinus(pi2);
    ass<"l vec & unary op: 1/1">(approx(c[0], 1.0));
    ass<"l vec & unary op: 2/1">(approx(c[1], 2.0));
    ass<"l vec & unary op: 3/1">(approx(c[2], 3.0));
    c = power(a, sinus(pi2));
    ass<"l vec & unary op: 1">(approx(c[0], 1));
    ass<"l vec & unary op: 2^1">(approx(c[1], 2));
    ass<"l vec & unary op: 3^1">(approx(c[2], 3));
  }
  // l borrow & l vec
  {
    double buf[3] = {1.0, 2.0, 3.0};
    Vec<double, Borrow<double>> a(buf, 3);
    Vec<double> b = c(4.0, 5.0, 6.0);
    Vec<double> c;
    c = a + b;
    ass<"l borrow & l vec: 5">(c[0] == 5);
    ass<"l borrow & l vec: 7">(c[1] == 7);
    ass<"l borrow & l vec: 9">(c[2] == 9);
    c = a - b;
    ass<"l borrow & l vec: -3">(c[0] == -3);
    ass<"l borrow & l vec: -3">(c[1] == -3);
    ass<"l borrow & l vec: -3">(c[2] == -3);
    c = a * b;
    ass<"l borrow & l vec: 4">(c[0] == 4);
    ass<"l borrow & l vec: 10">(c[1] == 10);
    ass<"l borrow & l vec: 18">(c[2] == 18);
    c = a / b;
    ass<"l borrow & l vec: 1/4">(c[0] == 0.25);
    ass<"l borrow & l vec: 2/5">(c[1] == 0.4);
    ass<"l borrow & l vec: 1/2">(c[2] == 0.5);
    c = power(a, b);
    ass<"l borrow & l vec: 1">(c[0] == 1);
    ass<"l borrow & l vec: 2^5">(c[1] == 32);
    ass<"l borrow & l vec: 3^6">(c[2] == 729);
  }
  // subset & l vec
  {
    Vec<double> a; a = c(1, 2, 3);
    Vec<int> indices = c(1, 2, 3);
    Vec<double> b; b = c(4, 5, 6);
    Vec<double> c;
    c = subset(a, indices) + b;
    ass<"subset & l vec: 5">(c[0] == 5);
    ass<"subset & l vec: 7">(c[1] == 7);
    ass<"subset & l vec: 9">(c[2] == 9);
    c = subset(a, indices) - b;
    ass<"subset & l vec: -3">(c[0] == -3);
    ass<"subset & l vec: -3">(c[1] == -3);
    ass<"subset & l vec: -3">(c[2] == -3);
    c = subset(a, indices)*b;
    ass<"subset & l vec: 4">(c[0] == 4);
    ass<"subset & l vec: 10">(c[1] == 10);
    ass<"subset & l vec: 18">(c[2] == 18);
    c = subset(a, indices) / b;
    ass<"subset & l vec: 1/4">(c[0] == 0.25);
    ass<"subset & l vec: 2/5">(c[1] == 0.4);
    ass<"subset & l vec: 1/2">(c[2] == 0.5);
    c = power(subset(a, indices), b);
    ass<"subset & l vec: 1">(c[0] == 1);
    ass<"subset & l vec: 2^5">(c[1] == 32);
    ass<"subset & l vec: 3^6">(c[2] == 729);
  }
  // binary op & l vec
  {
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> b; b = c(4, 5, 6);
    Vec<double> c;
    c = (a + 0.0) + b;
    ass<"binary op & l vec: 5">(c[0] == 5);
    ass<"binary op & l vec: 7">(c[1] == 7);
    ass<"binary op & l vec: 9">(c[2] == 9);
    c = (a + 0.0) - b;
    ass<"binary op & l vec: -3">(c[0] == -3);
    ass<"binary op & l vec: -3">(c[1] == -3);
    ass<"binary op & l vec: -3">(c[2] == -3);
    c = (a + 0.0) * b;
    ass<"binary op & l vec: 4">(c[0] == 4);
    ass<"binary op & l vec: 10">(c[1] == 10);
    ass<"binary op & l vec: 18">(c[2] == 18);
    c = (a + 0.0) / b;
    ass<"binary op & l vec: 1/4">(c[0] == 0.25);
    ass<"binary op & l vec: 2/5">(c[1] == 0.4);
    ass<"binary op & l vec: 3/2">(c[2] == 0.5);
    c = power(a + 0.0, b);
    ass<"binary op & l vec: 1">(c[0] == 1);
    ass<"binary op & l vec: 2^5">(c[1] == 32);
    ass<"binary op & l vec: 3^6">(c[2] == 729);
  }
  // unary op + l vec
  {
    Vec<double> pi2 = 0.5*3.141593;
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> c;
    c = sinus(pi2) + a;
    ass<"unary op & l vec: 2">(approx(c[0], 2));
    ass<"unary op & l vec: 3">(approx(c[1], 3));
    ass<"unary op & l vec: 4">(approx(c[2], 4));
    c = sinus(pi2) - a;
    ass<"unary op & l vec: 0">(approx(c[0], 0));
    ass<"unary op & l vec: -1">(approx(c[1], -1));
    ass<"unary op & l vec: -2">(approx(c[2], -2));
    c = sinus(pi2) * a;
    ass<"unary op & l vec: 1">(approx(c[0], 1));
    ass<"unary op & l vec: 2">(approx(c[1], 2));
    ass<"unary op & l vec: 3">(approx(c[2], 3));
    c = sinus(pi2) / a;
    ass<"unary op & l vec: 1/1">(approx(c[0], 1.0));
    ass<"unary op & l vec: 1/2">(approx(c[1], 0.5));
    c = power(sinus(pi2), a);
    ass<"unary op & l vec: 1">(approx(c[0], 1));
    ass<"unary op & l vec: 1^2">(approx(c[1], 1));
    ass<"unary op & l vec: 1^3">(approx(c[2], 1));
  }
  // LHS also on RHS
  {
    Vec<double> x; x = c(1,2,3);
    x = x + c(1,1,1);
    ass(x[0]==2 && x[1]==3 && x[2]==4, "x = x + 1 (safe)");
    x = c(1,2,3);
    x = c(1,1,1) + x;
    ass(x[0]==2 && x[1]==3 && x[2]==4, "x = 1 + x (safe)");
  }
}

// [[Rcpp::export]]
void test_arithmetic_assign_to_borrow() {
  // NOTE: test arithmetic +, -, * and /
  {
    std::string s; s = "ArithmeticTests: ";
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> b; b = c(4, 5, 6);
    double buf[3] = {0.0, 0.0, 0.0};
    Vec<double, Borrow<double>> c(buf, 3);
    c = a + b;
    ass<"5">(c[0] == 5);
    ass<"7">(c[1] == 7);
    ass<"9">(c[2] == 9);
    c = a - b;
    ass<"-3">(c[0] == -3);
    ass<"-3">(c[1] == -3);
    ass<"-3">(c[2] == -3);
    c = a * b;
    ass<"4">(c[0] == 4);
    ass<"10">(c[1] == 10);
    ass<"18">(c[2] == 18);
    c = a / b;
    ass<"1/4">(c[0] == 0.25);
    ass<"2/5">(c[1] == 0.4);
    ass<"1/2">(c[2] == 0.5);
    c = power(a, b);
    ass<"1">(c[0] == 1);
    ass<"2^5">(c[1] == 32);
    ass<"3^6">(c[2] == 729);
  }
  {
    Vec<double> a; a = c(1,2,3);
    double buf0[3] = {10,20,30};
    Vec<double, Borrow<double>> r(buf0, 3);
    // LVec + arith; arith + LVec
    r = a + 5;    ass(r[0]==6 && r[1]==7 && r[2]==8, "a+scalar");
    r = 5 + a;    ass(r[0]==6 && r[1]==7 && r[2]==8, "scalar+a");
    // LVec + RVec; RVec + LVec
    r = a + c(10,20,30);    ass(r[0]==11 && r[1]==22 && r[2]==33, "L+R");
    r = c(10,20,30) + a;    ass(r[0]==11 && r[1]==22 && r[2]==33, "R+L");

    // LVec + Borrow; Borrow + LVec
    double buf1[3] = {10,20,30};
    Vec<double, Borrow<double>> br(buf1, 3);
    r = a + br;             ass(r[0]==11 && r[1]==22 && r[2]==33, "L+Borrow");
    r = br + a;             ass(r[0]==11 && r[1]==22 && r[2]==33, "Borrow+L");

    r = sin(a) + a;         ass(r.size()==3, "Unary+L size");
    r = a + sin(a);         ass(r.size()==3, "L+Unary size");

    r = (a + a) + c(1,1,1); ass(r[0]==3 && r[1]==5 && r[2]==7, "(L+L)+R");
    r = a + (a + c(1,1,1)); ass(r[0]==3 && r[1]==5 && r[2]==7, "L+(L+R)");

    Vec<int> ai; ai = c(1,2,3);
    r = a + ai;             ass(r[0]==2 && r[1]==4 && r[2]==6, "double+int");

    r = a + c(10);          ass(r[0]==11 && r[1]==12 && r[2]==13, "recycle 1");
    r = a + c(10,20);       ass(r[0]==11 && r[1]==22 && r[2]==13, "recycle 2");
  }
  {
    double bufA[3] = {1,2,3};
    Vec<double, Borrow<double>> rb(bufA, 3);
    Vec<double> a; a = c(10,20,30);

    rb = rb + a;   // LHS appears on RHS
    ass(rb[0]==11 && rb[1]==22 && rb[2]==33, "borrow alias add");
    ass(bufA[0]==11 && bufA[1]==22 && bufA[2]==33, "underlying updated");

    rb = c(1,1,1) + rb;
    ass(rb[0]==12 && rb[1]==23 && rb[2]==34, "scalar + borrow alias");
  }
  {
    double bufB[3] = {0,0,0};
    Vec<double, Borrow<double>> rcap(bufB, 3);
    bool threw = false;
    try { rcap = c(1,2,3,4); }
    catch (const Rcpp::exception&) { threw = true; }
    ass(threw, "borrow assign over capacity throws");
  }
  {
    double bufC[3] = {0,0,0};
    Vec<int, Borrow<int>> rint(reinterpret_cast<int*>(bufC), 3);
    Vec<double> da; da = c(1.9, 2.1, -3.7);
    rint = da;   // uses static_cast<int>
    ass(rint[0]==1 && rint[1]==2 && rint[2]==-3, "borrow int cast from double");
  }
}

// [[Rcpp::export]]
void test_comparison() {
  // NOTE: test comparisons
  {
    std::string s = "ComparisonTests: ";
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> b; b = c(1, 5, 2);
    Vec<bool> c; c = a == b;
    ass<"1 == 1">(c[0]);
    ass<"2 == 5">(!c[1]);
    ass<"3 == 2">(!c[2]);
    c = a != b;
    ass<"1 != 1">(!c[0]);
    ass<"2 != 5">(c[1]);
    ass<"3 != 2">(c[2]);
    c = a >= b;
    ass<"1 >= 1">(c[0]);
    ass<"2 >= 5">(!c[1]);
    ass<"3 >= 2">(c[2]);
    c = a <= b;
    ass<"1 <= 1">(c[0]);
    ass<"2 <= 5">(c[1]);
    ass<"3 <= 2">(!c[2]);
    c = a < b;
    ass<"1 < 1">(!c[0]);
    ass<"2 < 5">(c[1]);
    ass<"3 < 2">(!c[2]);
    c = a > b;
    ass<"1 > 1">(!c[0]);
    ass<"2 > 5">(!c[1]);
    ass<"3 > 2">(c[2]);

    c = etr::c(1, 2, 3, 8) > 5;
    ass<"1 > 5">(!c[0]);
    ass<"2 > 5">(!c[1]);
    ass<"3 > 5">(!c[2]);
    ass<"8 > 5">(c[3]);

    Vec<bool> bres;
    bres = 2 < a;
    ass(!bres[0] && !bres[1] && bres[2], "scalar < a");
  }
}
