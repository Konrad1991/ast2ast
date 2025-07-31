// https://coin-or.github.io/CppAD/doc/cppad.htm
#include <iostream>
#include <vector>
#include <memory>
#include <array>
#include <utility>
#include <cmath>

#define STANDALONE_ETR
#include "../inst/include/etr.hpp"

struct NodeBase {
    virtual void backward(double) = 0;
    double grad = 0.0;
    virtual ~NodeBase() = default;
};

template <int N>
struct Node : NodeBase {
    double value;
    std::array<std::pair<NodeBase*, double>, N> parents;

    constexpr Node() : value(0.0) {} // TODO: should be removed. Just to please the compiler for now
    constexpr Node(double val) : value(val) {}

    constexpr void set_parents(std::array<std::pair<NodeBase*, double>, N> p) {
        parents = p;
    }

    void backward(double grad_output = 1.0) override {
        grad += grad_output;
        for (auto& [parent, local_grad] : parents) {
            parent->backward(grad_output * local_grad);
        }
    }
};

// Binary addition
template <int N1, int N2>
constexpr Node<2> add(Node<N1>& a, Node<N2>& b) {
  Node<2> out(a.value + b.value);
  out.set_parents({ { { &a, 1.0 }, { &b, 1.0 } } });
  return out;
}

// Binary multiplication
template <int N1, int N2>
constexpr Node<2> mul(Node<N1>& a, Node<N2>& b) {
  Node<2> out(a.value * b.value);
  out.set_parents({ { { &a, b.value }, { &b, a.value } } });
  return out;
}

// Unary sin
template <int N>
constexpr Node<1> sin(Node<N>& a) {
  Node<1> out(std::sin(a.value));
  out.set_parents({ { { &a, std::cos(a.value) } } });
  return out;
}

int main() {

  Node<0> x(2.0); // No parents
  Node<0> y(3.0);

  auto xy = mul(x, y);
  auto sum = add(xy, y);
  auto s = sin(sum);

  s.backward();

  std::cout << "df/dx = " << x.grad << "\n";
  std::cout << "df/dy = " << y.grad << "\n";

  etr::Vec<Node<0>> a{etr::SI{1}};
  etr::Vec<Node<0>> b;
  auto expr = a + b;
  etr::printTAST<decltype(expr)>();
  etr::printTAST<decltype(expr.d)>();
  etr::printTAST<decltype(expr.d.l)>();
  etr::printTAST<decltype(expr.d.r)>();
}
