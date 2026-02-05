//                  _  _
//  _   _|_ _  _|o_|__|_
// (_||_||_(_)(_|| |  |
//
// automatic differentiation made easier in C++
// https://github.com/autodiff/autodiff
//
// Licensed under the MIT License <http://opensource.org/licenses/MIT>.
//
// Copyright © 2018–2024 Allan Leal
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// This code is adapted a bit so that it can be used in ast2ast
// * Double is used instead of C++ native scalar types
// * combining the required information of meta.hpp, numericTraits, and var.hpp
// * runtime error with ass replaced
namespace etr{

//------------------------------------------------------------------------------
// For loop
//------------------------------------------------------------------------------
template<size_t i>
struct Index {
  constexpr static size_t index = i;
  constexpr operator size_t() const { return index; }
  constexpr operator size_t() { return index; }
};

template<size_t i, size_t ibegin, size_t iend, typename Function>
constexpr auto AuxFor(Function&& f) {
  if constexpr (i < iend) {
    f(Index<i>{});
    AuxFor<i + 1, ibegin, iend>(std::forward<Function>(f));
  }
}

template<size_t ibegin, size_t iend, typename Function>
constexpr auto For(Function&& f) {
  AuxFor<ibegin, ibegin, iend>(std::forward<Function>(f));
}

template<size_t iend, typename Function>
constexpr auto For(Function&& f) {
  For<0, iend>(std::forward<Function>(f));
}

//------------------------------------------------------------------------------
// Nodes
//------------------------------------------------------------------------------
template<typename T> struct VariableExpr;
template<typename T> struct IndependentVariableExpr;
template<typename T> struct DependentVariableExpr;
template<typename T> struct ConstantExpr;
template<typename T> struct UnaryExpr;
template<typename T> struct NegativeExpr;
template<typename T> struct BinaryExpr;
template<typename T> struct AddExpr;
template<typename T> struct SubExpr;
template<typename T> struct MulExpr;
template<typename T> struct DivExpr;
template<typename T> struct SinExpr;
template<typename T> struct CosExpr;
template<typename T> struct TanExpr;
template<typename T> struct SinhExpr;
template<typename T> struct CoshExpr;
template<typename T> struct TanhExpr;
template<typename T> struct ArcSinExpr;
template<typename T> struct ArcCosExpr;
template<typename T> struct ArcTanExpr;
template<typename T> struct ArcTan2Expr;
template<typename T> struct ExpExpr;
template<typename T> struct LogExpr;
template<typename T> struct PowExpr;
template<typename T> struct SqrtExpr;
template<typename T> struct Variable;

template<typename T>
struct VariableValueTypeNotDefinedFor {};

template<typename T>
struct VariableValueType;

template<typename T>
struct VariableValueType { using type = std::conditional_t<IsScalarOrScalarRef<T>, T, VariableValueTypeNotDefinedFor<T>>; };

template<typename T>
struct VariableValueType<Variable<T>> { using type = typename VariableValueType<T>::type; };

template<typename T>
struct VariableValueType<ExprPtr<T>> { using type = typename VariableValueType<T>::type; };

template<typename T>
struct VariableOrder { constexpr static auto value = 0; };

template<typename T>
struct VariableOrder<Variable<T>> { constexpr static auto value = 1 + VariableOrder<T>::value; };

template<typename T>
struct isVariable { constexpr static bool value = false; };

template<typename T>
struct isVariable<Variable<T>> { constexpr static bool value = true; };

template<typename T>
struct Expr {
  T val = {};
  explicit Expr(const T& v) : val(v) {}
  virtual ~Expr() {}
  virtual void bind_value(T* /* grad */) {} // binding value pointer --> writing the derivative during propagation
  virtual void bind_expr(ExprPtr<T>* /* gradx */) {} // binding an expression pointer --> wrting the derivative during propagation
  /// Update the contribution of this expression in the derivative of the root node of the expression tree.
  /// @param wprime The derivative of the root expression node w.r.t. the child expression of this expression node.
  virtual void propagate(const T& wprime) = 0;
  /// Update the contribution of this expression in the derivative of the root node of the expression tree.
  /// @param wprime The derivative of the root expression node w.r.t. the child expression of this expression node (as an expression).
  virtual void propagatex(const ExprPtr<T>& wprime) = 0;
  /// Update the value of this expression
  virtual void update() = 0;
};

/// The node in the expression tree representing either an independent or dependent variable.
template<typename T>
struct VariableExpr : Expr<T> {
  T* gradPtr = {};
  /// The derivative expression of the root expression node w.r.t. this variable (reusable for higher-order derivatives).
  ExprPtr<T>* gradxPtr = {};
  /// Construct a VariableExpr object with given value.
  VariableExpr(const T& v) : Expr<T>(v) {}
  virtual void bind_value(T* grad) { gradPtr = grad; }
  virtual void bind_expr(ExprPtr<T>* gradx) { gradxPtr = gradx; }
};

/// The node in the expression tree representing an independent variable.
template<typename T>
struct IndependentVariableExpr : VariableExpr<T> {
  using VariableExpr<T>::gradPtr;
  using VariableExpr<T>::gradxPtr;
  /// Construct an IndependentVariableExpr object with given value.
  IndependentVariableExpr(const T& v) : VariableExpr<T>(v) {}
  void propagate(const T& wprime) override {
    if(gradPtr) { *gradPtr = *gradPtr + wprime; }
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    if(gradxPtr) { *gradxPtr = *gradxPtr + wprime; }
  }
  void update() override {}
};

/// The node in the expression tree representing a dependent variable.
template<typename T>
struct DependentVariableExpr : VariableExpr<T> {
  using VariableExpr<T>::gradPtr;
  using VariableExpr<T>::gradxPtr;
  /// The expression tree that defines how the dependent variable is calculated.
  ExprPtr<T> expr;
  /// Construct an DependentVariableExpr object with given value.
  DependentVariableExpr(const ExprPtr<T>& e) : VariableExpr<T>(e->val), expr(e) {}
  void propagate(const T& wprime) override {
    if(gradPtr) { *gradPtr = *gradPtr + wprime; }
    expr->propagate(wprime);
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    if(gradxPtr) { *gradxPtr = *gradxPtr + wprime; }
    expr->propagatex(wprime);
  }
  void update() override {
    expr->update();
    this->val = expr->val;
  }
};

template<typename T>
struct ConstantExpr : Expr<T> {
  using Expr<T>::Expr;
  void propagate([[maybe_unused]] const T& wprime) override {}
  void propagatex([[maybe_unused]] const ExprPtr<T>& wprime) override {}
  void update() override {}
};
template<typename T> ExprPtr<T> constant(const T& val) { return std::make_shared<ConstantExpr<T>>(val); }

template<typename T>
struct UnaryExpr : Expr<T> {
  ExprPtr<T> x;
  UnaryExpr(const T& v, const ExprPtr<T>& e) : Expr<T>(v), x(e) {}
};

template<typename T>
struct BinaryExpr : Expr<T> {
  ExprPtr<T> l, r;
  BinaryExpr(const T& v, const ExprPtr<T>& ll, const ExprPtr<T>& rr) : Expr<T>(v), l(ll), r(rr) {}
};

template<typename T>
struct NegativeExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  using UnaryExpr<T>::UnaryExpr;
  void propagate(const T& wprime) override {
    x->propagate(-wprime);
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(-wprime);
  }
  void update() override {
    x->update();
    this->val = -x->val;
  }
};

template<typename T>
struct AddExpr : BinaryExpr<T> {
  // Using declarations for data members of base class
  using BinaryExpr<T>::l;
  using BinaryExpr<T>::r;
  using BinaryExpr<T>::BinaryExpr;
  void propagate(const T& wprime) override {
    l->propagate(wprime);
    r->propagate(wprime);
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    l->propagatex(wprime);
    r->propagatex(wprime);
  }
  void update() override {
    l->update();
    r->update();
    this->val = l->val + r->val;
  }
};
template<typename T>
struct SubExpr : BinaryExpr<T> {
  // Using declarations for data members of base class
  using BinaryExpr<T>::l;
  using BinaryExpr<T>::r;
  using BinaryExpr<T>::BinaryExpr;
  void propagate(const T& wprime) override {
    l->propagate(wprime);
    r->propagate(-wprime);
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    l->propagatex(wprime);  // (l - r)'l =  l'
    r->propagatex(-wprime); // (l - r)'r = -r'
  }
  void update() override {
    l->update();
    r->update();
    this->val = l->val - r->val;
  }
};

template<typename T>
struct MulExpr : BinaryExpr<T> {
  // Using declarations for data members of base class
  using BinaryExpr<T>::l;
  using BinaryExpr<T>::r;
  using BinaryExpr<T>::BinaryExpr;
  void propagate(const T& wprime) override {
    l->propagate(wprime * r->val); // (l * r)'l = w' * r
    r->propagate(wprime * l->val); // (l * r)'r = l * w'
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    l->propagatex(wprime * r);
    r->propagatex(wprime * l);
  }
  void update() override {
    l->update();
    r->update();
    this->val = l->val * r->val;
  }
};

template<typename T>
struct DivExpr : BinaryExpr<T> {
  // Using declarations for data members of base class
  using BinaryExpr<T>::l;
  using BinaryExpr<T>::r;
  using BinaryExpr<T>::BinaryExpr;
  void propagate(const T& wprime) override {
    const auto aux1 = Double(1.0) / r->val;
    const auto aux2 = -l->val * aux1 * aux1;
    l->propagate(wprime * aux1);
    r->propagate(wprime * aux2);
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    const auto aux1 = Double(1.0) / r;
    const auto aux2 = -l * aux1 * aux1;
    l->propagatex(wprime * aux1);
    r->propagatex(wprime * aux2);
  }
  void update() override {
    l->update();
    r->update();
    this->val = l->val / r->val;
  }
};

template<typename T>
struct SinExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  SinExpr(const T& v, const ExprPtr<T>& e) : UnaryExpr<T>(v, e) {}
  void propagate(const T& wprime) override {
    x->propagate(wprime * cos(x->val));
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(wprime * cos(x));
  }
  void update() override {
    x->update();
    this->val = sin(x->val);
  }
};
template<typename T>
struct CosExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  CosExpr(const T& v, const ExprPtr<T>& e) : UnaryExpr<T>(v, e) {}
  void propagate(const T& wprime) override {
    x->propagate(-wprime * sin(x->val));
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(-wprime * sin(x));
  }
  void update() override {
    x->update();
    this->val = cos(x->val);
  }
};
template<typename T>
struct TanExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  TanExpr(const T& v, const ExprPtr<T>& e) : UnaryExpr<T>(v, e) {}
  void propagate(const T& wprime) override {
    const auto aux = 1.0 / cos(x->val);
    x->propagate(wprime * aux * aux);
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    const auto aux = 1.0 / cos(x);
    x->propagatex(wprime * aux * aux);
  }
  void update() override {
    x->update();
    this->val = tan(x->val);
  }
};
template<typename T>
struct SinhExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  SinhExpr(const T& v, const ExprPtr<T>& e) : UnaryExpr<T>(v, e) {}
  void propagate(const T& wprime) override {
    x->propagate(wprime * cosh(x->val));
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(wprime * cosh(x));
  }
  void update() override {
    x->update();
    this->val = sinh(x->val);
  }
};
template<typename T>
struct CoshExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  CoshExpr(const T& v, const ExprPtr<T>& e) : UnaryExpr<T>(v, e) {}
  void propagate(const T& wprime) override {
    x->propagate(wprime * sinh(x->val));
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(wprime * sinh(x));
  }
  void update() override {
    x->update();
    this->val = cosh(x->val);
  }
};
template<typename T>
struct TanhExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  TanhExpr(const T& v, const ExprPtr<T>& e) : UnaryExpr<T>(v, e) {}
  void propagate(const T& wprime) override {
    const auto aux = 1.0 / cosh(x->val);
    x->propagate(wprime * aux * aux);
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    const auto aux = 1.0 / cosh(x);
    x->propagatex(wprime * aux * aux);
  }
  void update() override {
    x->update();
    this->val = tanh(x->val);
  }
};
template<typename T>
struct ArcSinExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  ArcSinExpr(const T& v, const ExprPtr<T>& e) : UnaryExpr<T>(v, e) {}
  void propagate(const T& wprime) override {
    x->propagate(wprime / sqrt(Double(1.0) - x->val * x->val));
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(wprime / sqrt(Double(1.0) - x * x));
  }
  void update() override {
    x->update();
    this->val = asin(x->val);
  }
};
template<typename T>
struct ArcCosExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  ArcCosExpr(const T& v, const ExprPtr<T>& e) : UnaryExpr<T>(v, e) {}
  void propagate(const T& wprime) override {
    x->propagate(-wprime / sqrt(Double(1.0) - x->val * x->val));
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(-wprime / sqrt(Double(1.0) - x * x));
  }
  void update() override {
    x->update();
    this->val = acos(x->val);
  }
};
template<typename T>
struct ArcTanExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  ArcTanExpr(const T& v, const ExprPtr<T>& e) : UnaryExpr<T>(v, e) {}
  void propagate(const T& wprime) override {
    x->propagate(wprime / (1.0 + x->val * x->val));
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(wprime / (1.0 + x * x));
  }
  void update() override {
    x->update();
    this->val = atan(x->val);
  }
};
template<typename T>
struct ExpExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::UnaryExpr;
  using UnaryExpr<T>::val;
  using UnaryExpr<T>::x;
  void propagate(const T& wprime) override {
    x->propagate(wprime * val); // exp(x)' = exp(x) * x'
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(wprime * exp(x));
  }
  void update() override {
    x->update();
    this->val = exp(x->val);
  }
};
template<typename T>
struct LogExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  using UnaryExpr<T>::UnaryExpr;
  void propagate(const T& wprime) override {
    x->propagate(wprime / x->val); // log(x)' = x'/x
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(wprime / x);
  }
  void update() override {
    x->update();
    this->val = log(x->val);
  }
};
template<typename T>
struct PowExpr : BinaryExpr<T> {
  // Using declarations for data members of base class
  using BinaryExpr<T>::val;
  using BinaryExpr<T>::l;
  using BinaryExpr<T>::r;
  T log_l;
  PowExpr(const T& v, const ExprPtr<T>& ll, const ExprPtr<T>& rr) : BinaryExpr<T>(v, ll, rr), log_l(log(ll->val)) {}
  void propagate(const T& wprime) override {
    using U = VariableValueType<T>;
    constexpr auto zero = U(0.0);
    const auto lval = l->val;
    const auto rval = r->val;
    const auto aux = wprime * pow(lval, rval - 1);
    l->propagate(aux * rval);
    const auto auxr = lval == zero ? 0.0 : lval * log(lval); // since x*log(x) -> 0 as x -> 0
    r->propagate(aux * auxr);
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    using U = VariableValueType<T>;
    constexpr auto zero = U(0.0);
    const auto aux = wprime * pow(l, r - 1);
    l->propagatex(aux * r);
    const auto auxr = l == zero ? 0.0*l : l * log(l); // since x*log(x) -> 0 as x -> 0
    r->propagatex(aux * auxr);
  }
  void update() override {
    l->update();
    r->update();
    this->val = pow(l->val, r->val);
  }
};
template<typename T>
struct PowConstantLeftExpr : BinaryExpr<T> {
  // Using declarations for data members of base class
  using BinaryExpr<T>::val;
  using BinaryExpr<T>::l;
  using BinaryExpr<T>::r;
  PowConstantLeftExpr(const T& v, const ExprPtr<T>& ll, const ExprPtr<T>& rr) : BinaryExpr<T>(v, ll, rr) {}
  void propagate(const T& wprime) override {
    const auto lval = l->val;
    const auto rval = r->val;
    const auto aux = wprime * pow(lval, rval - 1);
    const auto auxr = lval == 0.0 ? 0.0 : lval * log(lval); // since x*log(x) -> 0 as x -> 0
    r->propagate(aux * auxr);
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    const auto aux = wprime * pow(l, r - 1);
    const auto auxr = l == 0.0 ? 0.0*l : l * log(l); // since x*log(x) -> 0 as x -> 0
    r->propagatex(aux * auxr);
  }
  void update() override {
    r->update();
    this->val = pow(l->val, r->val);
  }
};
template<typename T>
struct PowConstantRightExpr : BinaryExpr<T> {
  // Using declarations for data members of base class
  using BinaryExpr<T>::val;
  using BinaryExpr<T>::l;
  using BinaryExpr<T>::r;
  PowConstantRightExpr(const T& v, const ExprPtr<T>& ll, const ExprPtr<T>& rr) : BinaryExpr<T>(v, ll, rr) {}
  void propagate(const T& wprime) override {
    l->propagate(wprime * pow(l->val, r->val - 1) * r->val); // pow(l, r)'l = r * pow(l, r - 1) * l'
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    l->propagatex(wprime * pow(l, r - 1) * r);
  }
  void update() override {
    l->update();
    this->val = pow(l->val, r->val);
  }
};

template<typename T>
struct SqrtExpr : UnaryExpr<T> {
  // Using declarations for data members of base class
  using UnaryExpr<T>::x;
  SqrtExpr(const T& v, const ExprPtr<T>& e) : UnaryExpr<T>(v, e) {}
  void propagate(const T& wprime) override {
    x->propagate(wprime / (Double(2.0) * sqrt(x->val))); // sqrt(x)' = 1/2 * 1/sqrt(x) * x'
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    x->propagatex(wprime / (Double(2.0) * sqrt(x)));
  }
  void update() override {
    x->update();
    this->val = sqrt(x->val);
  }
};

// Any expression yielding a boolean depending on arithmetic subexpressions
struct BooleanExpr {
  std::function<Logical()> expr;
  Logical val = {};
  explicit BooleanExpr(std::function<Logical()> expression) : expr(std::move(expression)) { update(); }
  operator bool() const { return val.val; }
  void update() { val = expr(); }
  auto operator! () const { return BooleanExpr([*this]() { return !(expr().val); }); }
};

/// Capture numeric comparison between two expression trees
template<typename T, typename Comparator>
auto expr_comparison(const ExprPtr<T>& l, const ExprPtr<T>& r, Comparator&& compare) {
  return BooleanExpr([=]() mutable -> Logical {
    l->update();
    r->update();
    return compare(l->val, r->val);
  });
}

template<typename Op> auto bool_expr_op(BooleanExpr& l, BooleanExpr& r, Op op) {
  return BooleanExpr([=]() mutable -> Logical {
    l.update();
    r.update();
    return op(l, r);
  });
}

/// Select between expression branches depending on a boolean expression
template<typename T>
struct ConditionalExpr : Expr<T> {
  // Using declarations for data members of base class
  BooleanExpr predicate;
  using Expr<T>::val;
  ExprPtr<T> l, r;
  ConditionalExpr(const BooleanExpr& wrappedPred, const ExprPtr<T>& ll, const ExprPtr<T>& rr) : Expr<T>(wrappedPred ? ll->val : rr->val), predicate(wrappedPred), l(ll), r(rr) {}
  void propagate(const T& wprime) override {
    if(predicate.val.val) l->propagate(wprime);
    else r->propagate(wprime);
  }
  void propagatex(const ExprPtr<T>& wprime) override {
    l->propagatex(derive(wprime, constant<T>(0.0)));
    r->propagatex(derive(constant<T>(0.0), wprime));
  }
  void update() override {
    predicate.update();
    if(predicate.val.val) {
      l->update();
      this->val = l->val;
    } else {
      r->update();
      this->val = r->val;
    }
  }
  ExprPtr<T> derive(const ExprPtr<T>& left, const ExprPtr<T>& right) const {
    return std::make_shared<ConditionalExpr>(predicate, left, right);
  }
};

//------------------------------------------------------------------------------
// Functions
//------------------------------------------------------------------------------
template<typename T> ExprPtr<T> operator-(const ExprPtr<T>& r) { return std::make_shared<NegativeExpr<T>>(-r->val, r); }

template<typename T> ExprPtr<T> operator+(const ExprPtr<T>& l, const ExprPtr<T>& r) { return std::make_shared<AddExpr<T>>(l->val + r->val, l, r); }
template<typename T> ExprPtr<T> operator-(const ExprPtr<T>& l, const ExprPtr<T>& r) { return std::make_shared<SubExpr<T>>(l->val - r->val, l, r); }
template<typename T> ExprPtr<T> operator*(const ExprPtr<T>& l, const ExprPtr<T>& r) { return std::make_shared<MulExpr<T>>(l->val * r->val, l, r); }
template<typename T> ExprPtr<T> operator/(const ExprPtr<T>& l, const ExprPtr<T>& r) { return std::make_shared<DivExpr<T>>(l->val / r->val, l, r); }

template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator+(const U& l, const ExprPtr<T>& r) { return constant<T>(l) + r; }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator-(const U& l, const ExprPtr<T>& r) { return constant<T>(l) - r; }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator*(const U& l, const ExprPtr<T>& r) { return constant<T>(l) * r; }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator/(const U& l, const ExprPtr<T>& r) { return constant<T>(l) / r; }

template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator+(const ExprPtr<T>& l, const U& r) { return l + constant<T>(r); }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator-(const ExprPtr<T>& l, const U& r) { return l - constant<T>(r); }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator*(const ExprPtr<T>& l, const U& r) { return l * constant<T>(r); }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator/(const ExprPtr<T>& l, const U& r) { return l / constant<T>(r); }

template<typename T> ExprPtr<T> sin(const ExprPtr<T>& x) { return std::make_shared<SinExpr<T>>(sin(x->val), x); }
template<typename T> ExprPtr<T> cos(const ExprPtr<T>& x) { return std::make_shared<CosExpr<T>>(cos(x->val), x); }
template<typename T> ExprPtr<T> tan(const ExprPtr<T>& x) { return std::make_shared<TanExpr<T>>(tan(x->val), x); }
template<typename T> ExprPtr<T> asin(const ExprPtr<T>& x) { return std::make_shared<ArcSinExpr<T>>(asin(x->val), x); }
template<typename T> ExprPtr<T> acos(const ExprPtr<T>& x) { return std::make_shared<ArcCosExpr<T>>(acos(x->val), x); }
template<typename T> ExprPtr<T> atan(const ExprPtr<T>& x) { return std::make_shared<ArcTanExpr<T>>(atan(x->val), x); }
template<typename T> ExprPtr<T> sinh(const ExprPtr<T>& x) { return std::make_shared<SinhExpr<T>>(sinh(x->val), x); }
template<typename T> ExprPtr<T> cosh(const ExprPtr<T>& x) { return std::make_shared<CoshExpr<T>>(cosh(x->val), x); }
template<typename T> ExprPtr<T> tanh(const ExprPtr<T>& x) { return std::make_shared<TanhExpr<T>>(tanh(x->val), x); }
template<typename T> ExprPtr<T> exp(const ExprPtr<T>& x) { return std::make_shared<ExpExpr<T>>(exp(x->val), x); }
template<typename T> ExprPtr<T> log(const ExprPtr<T>& x) { return std::make_shared<LogExpr<T>>(log(x->val), x); }
template<typename T> ExprPtr<T> sqrt(const ExprPtr<T>& x) { return std::make_shared<SqrtExpr<T>>(sqrt(x->val), x); }
template<typename T> ExprPtr<T> pow(const ExprPtr<T>& l, const ExprPtr<T>& r) { return std::make_shared<PowExpr<T>>(pow(l->val, r->val), l, r); }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> pow(const U& l, const ExprPtr<T>& r) { return std::make_shared<PowConstantLeftExpr<T>>(pow(l, r->val), constant<T>(l), r); }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> pow(const ExprPtr<T>& l, const U& r) { return std::make_shared<PowConstantRightExpr<T>>(pow(l->val, r), l, constant<T>(r)); }

//------------------------------------------------------------------------------
// Variable
//------------------------------------------------------------------------------
// Retrieve val
// --------------------------------------------------------------------------------------------------
using var = Variable<etr::Double>;
template<typename T> struct Variable;

template<typename T> requires IsArithV<T> inline auto get_scalar_val(const T& t) { return t; }
template<typename T> requires IsArithRefV<T> inline auto get_scalar_val(const T& t) {
  using DecayedT = Decayed<T>;
  using RetType = to_ast_scalar_t<DecayedT>;
  return RetType(t);
}
template<typename T> inline auto get_scalar_val(const ExprPtr<T>& t) { return t->val; }
template<typename T> inline auto get_scalar_val(const Variable<T>& t) { return t.expr->val; }
template<typename T> requires IS<T, BooleanExpr> inline auto get_scalar_val(const T& t) { return t.val; }

template<typename T> requires std::is_arithmetic_v<T> inline T get_val(const T& t) { return t; }
template<typename T> requires IsArithV<T> inline auto get_val(const T& t) { return t.val; }
template<typename T> requires IsArithRefV<T> inline auto get_val(const T& t) { return *(t.p_val); }
template<typename T> inline auto get_val(const ExprPtr<T>& t) { return t->val.val; }
template<typename T> inline auto get_val(const Variable<T>& t) { return t.expr->val.val; }
template<typename T> inline auto get_val(const BooleanExpr& t) { return t.val.val; }

/// The autodiff variable type used for detail mode automatic differentiation.
template<typename T>
struct Variable {
  using value_type = T;
  /// The pointer to the expression tree of variable operations
  ExprPtr<T> expr;
  /// Construct a default Variable object
  Variable() : Variable(Double(0.0)) {}
  /// Construct a copy of a Variable object
  Variable(const Variable& other) : Variable(other.expr) {}

  /// Construct a Variable object with given Scalar values from ast2ast
  template<typename U> requires IsScalarOrScalarRef<U>
  Variable(const U& val) : expr(std::make_shared<IndependentVariableExpr<T>>(val)) {}
  /// Construct a Variable object with given native C++ scalar value
  template<typename U> requires IsCppArithV<U>
  Variable(const U& val) : expr(std::make_shared<IndependentVariableExpr<T>>(val)) {}

  /// Construct a Variable object with given expression
  Variable(const ExprPtr<T>& e) : expr(std::make_shared<DependentVariableExpr<T>>(e)) {}
  /// Default copy assignment
  Variable& operator=(const Variable&) = default;
  /// Update the value of this variable with changes in its expression tree
  void update() { expr->update(); }
  void update(T value) {
    if(auto independentExpr = std::dynamic_pointer_cast<IndependentVariableExpr<T>>(expr)) {
      independentExpr->val = value;
      independentExpr->update();
    } else {
      ass<"Cannot update the value of a dependent expression stored in a variable">(false);
    }
  }
  /// Implicitly convert this Variable object into an expression pointer.
  operator const ExprPtr<T>&() const { return expr; }
  /// Assign an arithmetic value to this variable.
  template<typename U> requires IsScalarOrScalarRef<U>
  auto operator=(const U& val) -> Variable& { *this = Variable(val); return *this; }
  /// Assign an array to this variable
  template<typename U> requires IsArray<U>
  auto operator=(const U& arr) -> Variable& {
    using  inner = typename ExtractDataType<U>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    const auto arr_val = get_scalar_val(arr.get(0));
    *this = Variable(arr_val); return *this;
  }
  /// Assign an expression to this variable.
  auto operator=(const ExprPtr<T>& x) -> Variable& { *this = Variable(x); return *this; }
  // Assignment operators
  Variable& operator+=(const ExprPtr<T>& x) { *this = Variable(expr + x); return *this; }
  Variable& operator-=(const ExprPtr<T>& x) { *this = Variable(expr - x); return *this; }
  Variable& operator*=(const ExprPtr<T>& x) { *this = Variable(expr * x); return *this; }
  Variable& operator/=(const ExprPtr<T>& x) { *this = Variable(expr / x); return *this; }

  // Assignment operators with arithmetic values
  template<typename U> requires IsScalarOrScalarRef<U> Variable& operator+=(const U& x) { *this = Variable(expr + x); return *this; }
  template<typename U> requires IsScalarOrScalarRef<U> Variable& operator-=(const U& x) { *this = Variable(expr - x); return *this; }
  template<typename U> requires IsScalarOrScalarRef<U> Variable& operator*=(const U& x) { *this = Variable(expr * x); return *this; }
  template<typename U> requires IsScalarOrScalarRef<U> Variable& operator/=(const U& x) { *this = Variable(expr / x); return *this; }

  explicit operator T() const { return expr->val; }
  template<typename U>
  explicit operator U() const {
    return static_cast<U>(expr->val.val);
  }
  inline static Double NA() {
    Double x(std::numeric_limits<double>::quiet_NaN());
    x.is_na = true;
    return x;
  }
  inline static Double NaN() {
    return Double(std::numeric_limits<double>::quiet_NaN());
  }
  inline static Double Inf() {
    return Double(std::numeric_limits<double>::infinity());
  }
};

//------------------------------------------------------------------------------
// EXPRESSION TRAITS
//------------------------------------------------------------------------------
template<typename T> requires IsScalarOrScalarRef<T> T expr_value(const T& t) { return t; }
template<typename T> T expr_value(const ExprPtr<T>& t) { return t->val; }
template<typename T> T expr_value(const Variable<T>& t) { return t.expr->val; }

template<typename T, typename U>
using expr_common_t = common_type_t<decltype(expr_value(std::declval<T>())), decltype(expr_value(std::declval<U>()))>;

template<class> struct sfinae_true : std::true_type {};
template<typename T> static auto is_expr_test(int) -> sfinae_true<decltype(expr_value(std::declval<T>()))>;
template<typename T> static auto is_expr_test(long) -> std::false_type;
template<typename T> struct is_expr : decltype(is_expr_test<T>(0)) {};
template<typename T> constexpr bool is_expr_v = is_expr<T>::value;

template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> coerce_expr(const U& u) { return constant<T>(u); }
template<typename T> ExprPtr<T> coerce_expr(const ExprPtr<T>& t) { return t; }
template<typename T> ExprPtr<T> coerce_expr(const Variable<T>& t) { return t.expr; }

template<typename T, typename U> struct is_binary_expr : std::conditional_t<!(IsScalarOrScalarRef<T> && IsScalarOrScalarRef<U>) && is_expr_v<T> && is_expr_v<U>, std::true_type, std::false_type> {};
template<typename T, typename U> constexpr bool is_binary_expr_v = is_binary_expr<T, U>::value;

//------------------------------------------------------------------------------
// COMPARISON OPERATORS
//------------------------------------------------------------------------------
template<typename Comparator, typename T, typename U>
auto comparison_operator(const T& t, const U& u) {
  using C = expr_common_t<T, U>;
  return expr_comparison(coerce_expr<C>(t), coerce_expr<C>(u), Comparator {});
}

template<typename T, typename U> requires is_binary_expr_v<T, U>
auto operator == (const T& t, const U& u) { return comparison_operator<std::equal_to<>>(t, u); }
template<typename T, typename U> requires is_binary_expr_v<T, U>
auto operator != (const T& t, const U& u) { return comparison_operator<std::not_equal_to<>>(t, u); }
template<typename T, typename U> requires is_binary_expr_v<T, U>
auto operator <= (const T& t, const U& u) { return comparison_operator<std::less_equal<>>(t, u); }
template<typename T, typename U> requires is_binary_expr_v<T, U>
auto operator >= (const T& t, const U& u) { return comparison_operator<std::greater_equal<>>(t, u); }
template<typename T, typename U> requires is_binary_expr_v<T, U>
auto operator < (const T& t, const U& u) { return comparison_operator<std::less<>>(t, u); }
template<typename T, typename U> requires is_binary_expr_v<T, U>
auto operator > (const T& t, const U& u) { return comparison_operator<std::greater<>>(t, u); }
template<typename T, typename U> requires is_binary_expr_v<T, U>
auto operator && (const T& t, const U& u) { return comparison_operator<std::logical_and<>>(t, u); }
template<typename T, typename U> requires is_binary_expr_v<T, U>
auto operator || (const T& t, const U& u) { return comparison_operator<std::logical_or<>>(t, u); }

template<typename T, typename U> requires is_binary_expr_v<T, U>
auto operator & (const T& t, const U& u) { return comparison_operator<std::logical_and<>>(t, u); }
template<typename T, typename U> requires is_binary_expr_v<T, U>
auto operator | (const T& t, const U& u) { return comparison_operator<std::logical_or<>>(t, u); }

//------------------------------------------------------------------------------
// ARITHMETIC OPERATORS (DEFINED FOR ARGUMENTS OF TYPE Variable)
//------------------------------------------------------------------------------
template<typename T> ExprPtr<T> operator-(const Variable<T>& r) { return -r.expr; }

template<typename T> ExprPtr<T> operator+(const Variable<T>& l, const Variable<T>& r) { return l.expr + r.expr; }
template<typename T> ExprPtr<T> operator-(const Variable<T>& l, const Variable<T>& r) { return l.expr - r.expr; }
template<typename T> ExprPtr<T> operator*(const Variable<T>& l, const Variable<T>& r) { return l.expr * r.expr; }
template<typename T> ExprPtr<T> operator/(const Variable<T>& l, const Variable<T>& r) { return l.expr / r.expr; }

template<typename T> ExprPtr<T> operator+(const ExprPtr<T>& l, const Variable<T>& r) { return l + r.expr; }
template<typename T> ExprPtr<T> operator-(const ExprPtr<T>& l, const Variable<T>& r) { return l - r.expr; }
template<typename T> ExprPtr<T> operator*(const ExprPtr<T>& l, const Variable<T>& r) { return l * r.expr; }
template<typename T> ExprPtr<T> operator/(const ExprPtr<T>& l, const Variable<T>& r) { return l / r.expr; }

template<typename T> ExprPtr<T> operator+(const Variable<T>& l, const ExprPtr<T>& r) { return l.expr + r; }
template<typename T> ExprPtr<T> operator-(const Variable<T>& l, const ExprPtr<T>& r) { return l.expr - r; }
template<typename T> ExprPtr<T> operator*(const Variable<T>& l, const ExprPtr<T>& r) { return l.expr * r; }
template<typename T> ExprPtr<T> operator/(const Variable<T>& l, const ExprPtr<T>& r) { return l.expr / r; }

template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator+(const U& l, const Variable<T>& r) { return l + r.expr; }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator-(const U& l, const Variable<T>& r) { return l - r.expr; }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator*(const U& l, const Variable<T>& r) { return l * r.expr; }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator/(const U& l, const Variable<T>& r) { return l / r.expr; }

template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator+(const Variable<T>& l, const U& r) { return l.expr + r; }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator-(const Variable<T>& l, const U& r) { return l.expr - r; }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator*(const Variable<T>& l, const U& r) { return l.expr * r; }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> operator/(const Variable<T>& l, const U& r) { return l.expr / r; }

//------------------------------------------------------------------------------
// TRIGONOMETRIC FUNCTIONS (DEFINED FOR ARGUMENTS OF TYPE Variable)
//------------------------------------------------------------------------------
template<typename T> ExprPtr<T> sin(const Variable<T>& x) { return sin(x.expr); }
template<typename T> ExprPtr<T> cos(const Variable<T>& x) { return cos(x.expr); }
template<typename T> ExprPtr<T> tan(const Variable<T>& x) { return tan(x.expr); }
template<typename T> ExprPtr<T> asin(const Variable<T>& x) { return asin(x.expr); }
template<typename T> ExprPtr<T> acos(const Variable<T>& x) { return acos(x.expr); }
template<typename T> ExprPtr<T> atan(const Variable<T>& x) { return atan(x.expr); }
template<typename T> ExprPtr<T> sinh(const Variable<T>& x) { return sinh(x.expr); }
template<typename T> ExprPtr<T> cosh(const Variable<T>& x) { return cosh(x.expr); }
template<typename T> ExprPtr<T> tanh(const Variable<T>& x) { return tanh(x.expr); }

//------------------------------------------------------------------------------
// EXPONENTIAL AND LOGARITHMIC FUNCTIONS (DEFINED FOR ARGUMENTS OF TYPE Variable)
//------------------------------------------------------------------------------
template<typename T> ExprPtr<T> exp(const Variable<T>& x) { return exp(x.expr); }
template<typename T> ExprPtr<T> log(const Variable<T>& x) { return log(x.expr); }

//------------------------------------------------------------------------------
// POWER FUNCTIONS (DEFINED FOR ARGUMENTS OF TYPE Variable)
//------------------------------------------------------------------------------
template<typename T> ExprPtr<T> sqrt(const Variable<T>& x) { return sqrt(x.expr); }
template<typename T> ExprPtr<T> pow(const Variable<T>& l, const Variable<T>& r) { return pow(l.expr, r.expr); }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> pow(const U& l, const Variable<T>& r) { return pow(l, r.expr); }
template<typename T, typename U> requires IsScalarOrScalarRef<U> ExprPtr<T> pow(const Variable<T>& l, const U& r) { return pow(l.expr, r); }

//------------------------------------------------------------------------------
// OTHER FUNCTIONS (DEFINED FOR ARGUMENTS OF TYPE Variable)
//------------------------------------------------------------------------------
template<typename T> requires is_expr_v<T>
auto val(const T& t) { return expr_value(t); }

template<typename... Vars> struct Wrt {
  std::tuple<Vars...> args;
};

/// The keyword used to denote the variables *with respect to* the derivative is calculated.
template<typename... Args>
auto wrt(Args&&... args) {
  return Wrt<Args&&...>{ std::forward_as_tuple(std::forward<Args>(args)...) };
}

/// Return the derivatives of a dependent variable y with respect given independent variables.
template<typename T, typename... Vars>
auto derivatives(const Variable<T>& y, const Wrt<Vars...>& wrt) {
  constexpr auto N = sizeof...(Vars);
  std::array<T, N> values;
  values.fill(0.0);
  For<N>([&](auto i) constexpr {
    std::get<i>(wrt.args).expr->bind_value(&values.at(i));
  });
  y.expr->propagate(1.0);
  For<N>([&](auto i) constexpr {
    std::get<i>(wrt.args).expr->bind_value(nullptr);
  });
  return values;
}

/// Return the derivatives of a dependent variable y with respect given independent variables.
template<typename T, typename... Vars>
auto derivativesx(const Variable<T>& y, const Wrt<Vars...>& wrt) {
  constexpr auto N = sizeof...(Vars);
  std::array<Variable<T>, N> values;
  For<N>([&](auto i) constexpr {
    std::get<i>(wrt.args).expr->bind_expr(&values.at(i).expr);
  });
  y.expr->propagatex(constant<T>(1.0));
  For<N>([&](auto i) constexpr {
    std::get<i>(wrt.args).expr->bind_expr(nullptr);
  });
  return values;
}

template<typename T>
std::ostream& operator<<(std::ostream& out, const Variable<T>& x) {
  out << val(x);
  return out;
}

/// Output an ExprPrt object to the output stream.
template<typename T>
std::ostream& operator<<(std::ostream& out, const ExprPtr<T>& x) {
  out << val(x);
  return out;
}

// preserve na cast
// --------------------------------------------------------------------------------------------------
template <class Out, class In> inline Out cast_preserve_na(const In& x) {
  using InD = Decayed<In>;
  // Out Logical or Integer
  if constexpr (IsLogical<Out> || IsInteger<Out>) {
    // Inner type is logical or integer
    if constexpr (IsLogical<InD> || IsInteger<InD>) {
      if (x.isNA()) return Out::NA(); else return Out(get_val(x));
    }
    // Inner type is real
    else {
      const auto s = get_scalar_val(x);
      if (s.isNA()) {
        return Out::NA();
      } else if (s.isNaN()) {
        return Out::NA();
      } else if (s.isInfinite()) {
        return Out::NA();
      } else {
        return Out(get_val(s));
      }
    }
  }
  // Out is any real type
  else {
    // Inner type is logical or integer
    if constexpr (IsLogical<InD> || IsInteger<InD>) {
      if (x.isNA()) return Out::NA(); else return Out(get_val(x));
    }
    // Inner type is real
    else {
      const auto s = get_scalar_val(x);
      if (s.isNA()) {
        return Out::NA();
      } else if (s.isNaN()) {
        return Out::NaN();
      } else if (s.isInfinite()) {
        return Out::Inf();
      } else {
        return Out(get_val(s));
      }
    }
  }

}

// Conversion to Scalars
// --------------------------------------------------------------------------------------------------
template<typename T> Logical::Logical(Variable<T> v)    : val(static_cast<bool>(get_val(v))), is_na(get_scalar_val(v).is_na) {}
template<typename T> Integer::Integer(Variable<T> v)    : val(static_cast<int>(get_val(v))), is_na(get_scalar_val(v).is_na) {}

// Get Inner Data Type of Variable
// --------------------------------------------------------------------------------------------------
template <typename T> struct ExtractDataTypeFromVariable;

template <typename T> struct ExtractDataTypeFromVariable<Variable<T>> {
  using value_type = T;
};
template <typename T> struct ExtractDataTypeFromVariable<const Variable<T>> {
  using value_type = T const;
};
template <typename T> using ExtractedTypeFromVariableData = typename ExtractDataTypeFromVariable<T>::value_type;

}
