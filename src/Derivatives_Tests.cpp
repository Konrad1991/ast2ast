#include <ios>

// -O2 speed up!

#include "etr.hpp"
using namespace etr;

template <typename L, typename R> bool doubleEq(const L l, const R r) {
  if (std::abs(l - r) < 0.01) {
    return true;
  }
  return false;
}

template <typename L, typename R>
void doubleEqVec(const L &l, const R &r, std::string message) {
  for (size_t i = 0; i < l.size(); i++) {
    ass(doubleEq(l[i], r[i]), message + " " + std::to_string(i));
  }
}

// [[Rcpp::export]]
void test_derivs() {
  // NOTE: test sinus
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    double pi = 3.141593;
    a = coca<0>(av, pi, 2 * pi, 3 * pi, 4 * pi);
    b = coca<1>(av, 4, 5, 6, 7);
    b = sinus(a);
    Vec<double> resA = coca(3.141593, 6.283185, 9.424778, 12.566371);
    Vec<double> resB =
        coca(1.224647e-16, -2.449294e-16, 3.673940e-16, -4.898587e-16);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "sinus");
    doubleEqVec(bD, resB, "sinus");
    Vec<double> resADeriv = coca(1.0, 1.0, 1.0, 1.0);
    Vec<double> resBDeriv = coca(-1, 1.0, -1, 1.0);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "sinus");
    doubleEqVec(bDeriv, resBDeriv, "sinus");
  }
  // NOTE: general tests
  {
    etr::AllVars<2, 0, 0, 2> av(0, 0); // deriv with respect tp variable 1 = a
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 1, 2, 3);
    b = coca<1>(av, 4, 5, 6) * a;
    a = b + b;
    Vec<double> resA = coca(8, 20, 36);
    Vec<double> resB = coca(4, 10, 18);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "general");
    doubleEqVec(bD, resB, "general");
    Vec<double> resADeriv = coca(8, 10, 12);
    Vec<double> resBDeriv = coca(4, 5, 6);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "general");
    doubleEqVec(bDeriv, resBDeriv, "general");
  }
  // NOTE: further tests
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 1, 2, 3, 4);
    b = coca<1>(av, 4, 5, 6, 7);
    Vec<double> resA = coca(1, 2, 3, 4);
    Vec<double> resB = coca(4, 5, 6, 7);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "further tests");
    doubleEqVec(bD, resB, "further tests");
    Vec<double> resADeriv = coca(1, 1, 1, 1);
    Vec<double> resBDeriv = coca(0, 0, 0, 0);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "further tests Deriv");
    doubleEqVec(bDeriv, resBDeriv, "further tests Deriv");
  }
  // NOTE: test constants
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 1, 2, 3, 4);
    b = coca<1>(av, 4, 5, 6, 7);
    a = scalarDeriv<2>(av, 3.14);
    Vec<double> resA = coca(3.14);
    Vec<double> resB = coca(4, 5, 6, 7);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "constants tests");
    doubleEqVec(bD, resB, "constants tests");
    Vec<double> resADeriv = coca(1, 1, 1, 1);
    Vec<double> resBDeriv = coca(0, 0, 0, 0);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "constants tests Deriv");
    doubleEqVec(bDeriv, resBDeriv, "constants tests Deriv");
  }

  // NOTE: test constants
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 3, 6, 9, 12);
    b = coca<1>(av, 4, 5, 6, 7);
    b = a / scalarDeriv<2>(av, 3);
    Vec<double> resA = coca(3, 6, 9, 12);
    Vec<double> resB = coca(1, 2, 3, 4);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "constants tests 2");
    doubleEqVec(bD, resB, "constants tests 2");
    Vec<double> resADeriv = coca(1, 1, 1, 1);
    Vec<double> resBDeriv = coca(0.3333, 0.3333, 0.3333, 0.3333);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "constants tests Deriv 2");
    doubleEqVec(bDeriv, resBDeriv, "constants tests Deriv 2");
  }

  // NOTE: test constants
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 3, 6, 9, 12);
    b = coca<1>(av, 4, 5, 6, 7);
    b = (a / scalarDeriv<2>(av, 3)) + a * b * b;
    Vec<double> resA = coca(3, 6, 9, 12);
    Vec<double> resB = coca(3, 6, 9, 12) / 3 +
                       coca(3, 6, 9, 12) * coca(4, 5, 6, 7) * coca(4, 5, 6, 7);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "constants tests 3");
    doubleEqVec(bD, resB, "constants tests 3");
    Vec<double> resADeriv = coca(1, 1, 1, 1);
    Vec<double> resBDeriv = (0.3333 + coca(4, 5, 6, 7) * coca(4, 5, 6, 7));
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "constants tests Deriv 3");
    doubleEqVec(bDeriv, resBDeriv, "constants tests Deriv 3");
  }

  // NOTE: test comparisons
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 3, 6, 9, 12);
    b = coca<1>(av, 4, 5, 6, 7);
    b = a == b;
    Vec<double> resA = coca(3, 6, 9, 12);
    Vec<double> resB = coca(0, 0, 0, 0);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "comparisons");
    doubleEqVec(bD, resB, "comparisons");
    Vec<double> resADeriv = coca(1, 1, 1, 1);
    Vec<double> resBDeriv = coca(0, 0, 0, 0);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "comparisons Deriv");
    doubleEqVec(bDeriv, resBDeriv, "comparisons Deriv");
  }

  // NOTE: test unequal
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 3, 6, 9, 12);
    b = coca<1>(av, 4, 5, 6, 7);
    b = a != b;
    Vec<double> resA = coca(3, 6, 9, 12);
    Vec<double> resB = coca(1, 1, 1, 1);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "unequal");
    doubleEqVec(bD, resB, "unequal");
    Vec<double> resADeriv = coca(1, 1, 1, 1);
    Vec<double> resBDeriv = coca(0, 0, 0, 0);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "unequal Deriv");
    doubleEqVec(bDeriv, resBDeriv, "unequal Deriv");
  }

  // NOTE: test smaller
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 3, 6, 9, 12);
    b = coca<1>(av, 4, 5, 6, 7);
    b = a < b;
    Vec<double> resA = coca(3, 6, 9, 12);
    Vec<double> resB = coca(1, 0, 0, 0);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "smaller");
    doubleEqVec(bD, resB, "smaller");
    Vec<double> resADeriv = coca(1, 1, 1, 1);
    Vec<double> resBDeriv = coca(0, 0, 0, 0);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "smallerDeriv");
    doubleEqVec(bDeriv, resBDeriv, "smallerDeriv");
  }

  // NOTE: test larger
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 3, 6, 9, 12);
    b = coca<1>(av, 4, 5, 6, 7);
    b = a > b;
    Vec<double> resA = coca(3, 6, 9, 12);
    Vec<double> resB = coca(0, 1, 1, 1);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "larger");
    doubleEqVec(bD, resB, "larger");
    Vec<double> resADeriv = coca(1, 1, 1, 1);
    Vec<double> resBDeriv = coca(0, 0, 0, 0);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "larger Deriv");
    doubleEqVec(bDeriv, resBDeriv, "larger Deriv");
  }

  // NOTE: test larger equal
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 3, 6, 9, 12);
    b = coca<1>(av, 4, 5, 6, 7);
    b = a >= b;
    Vec<double> resA = coca(3, 6, 9, 12);
    Vec<double> resB = coca(0, 1, 1, 1);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "larger equal");
    doubleEqVec(bD, resB, "larger equal");
    Vec<double> resADeriv = coca(1, 1, 1, 1);
    Vec<double> resBDeriv = coca(0, 0, 0, 0);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "larger equalDeriv");
    doubleEqVec(bDeriv, resBDeriv, "larger equalDeriv");
  }

  // NOTE: test smaller equal
  {
    AllVars<2, 0, 0, 3> av(0, 0);
    Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> a(av);
    Vec<double, VarPointer<decltype(av), 1, 0>, VariableTypeTrait> b(av);
    a = coca<0>(av, 3, 6, 9, 12);
    b = coca<1>(av, 4, 5, 6, 7);
    b = a <= b;
    Vec<double> resA = coca(3, 6, 9, 12);
    Vec<double> resB = coca(1, 0, 0, 0);
    auto aD = a.d.get();
    auto bD = b.d.get();
    doubleEqVec(aD, resA, "smallerequal");
    doubleEqVec(bD, resB, "smallerequal");
    Vec<double> resADeriv = coca(1, 1, 1, 1);
    Vec<double> resBDeriv = coca(0, 0, 0, 0);
    auto aDeriv = get_derivs(a);
    auto bDeriv = get_derivs(b);
    doubleEqVec(aDeriv, resADeriv, "smallerequalDeriv");
    doubleEqVec(bDeriv, resBDeriv, "smallerequalDeriv");
  }
}
