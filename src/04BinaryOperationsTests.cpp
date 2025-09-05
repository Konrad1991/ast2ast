#include "etr.hpp"
#include <stdexcept>
#include <type_traits>
using namespace etr;

// [[Rcpp::export]]
void test_arithmetic_assign_to_buffer() {
  // NOTE: test arithmetic +, -, * and /
  {
    std::string s; s = "ArithmeticTests: ";
    Vec<double> a; a = c(1, 2, 3);
    Vec<double> b; b = c(4, 5, 6);
    Vec<double> c;
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
    Vec<double> r;
    // LVec + arith; arith + LVec
    r = a + 5;    ass(r[0]==6 && r[1]==7 && r[2]==8, "a+scalar");
    r = 5 + a;    ass(r[0]==6 && r[1]==7 && r[2]==8, "scalar+a");
    // LVec + RVec; RVec + LVec
    r = a + c(10,20,30);    ass(r[0]==11 && r[1]==22 && r[2]==33, "L+R");
    r = c(10,20,30) + a;    ass(r[0]==11 && r[1]==22 && r[2]==33, "R+L");

    // LVec + Borrow; Borrow + LVec
    double buf[3] = {10,20,30};
    Vec<double, Borrow<double>> br(buf, 3);
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
    Vec<double> x; x = c(1,2,3);
    x = x + c(1,1,1); // LHS also on RHS
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
