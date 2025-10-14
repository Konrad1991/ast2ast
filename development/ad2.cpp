#include <iostream>
#include <vector>
#include <memory>
#include <array>
#include <utility>
#include <cmath>

struct Node {
  double value;
  double grad = 0.0;
  std::vector<std::pair<std::shared_ptr<Node>, double>> parents;
  Node(double val) : value(val) {}
  void set_parents(const std::vector<std::pair<std::shared_ptr<Node>, double>>& p) {
    parents = p;
  }
  void backward(double grad_output = 1.0) {
    grad += grad_output;
    for (auto& [parent, local_grad] : parents) {
      parent->backward(grad_output * local_grad);
    }
  }
};

std::shared_ptr<Node> operator+(std::shared_ptr<Node> a, std::shared_ptr<Node> b) {
  std::shared_ptr<Node> out = std::make_shared<Node>(a->value + b->value);
  out->set_parents(
    { { a, 1.0 }, { b, 1.0 } }
  );
  return out;
}

std::shared_ptr<Node> operator*(std::shared_ptr<Node> a, std::shared_ptr<Node> b) {
  std::shared_ptr<Node> out = std::make_shared<Node>(a->value * b->value);
  out->set_parents(
    { { a, b->value }, { b, a->value } }
  );
  return out;
}

std::shared_ptr<Node> sin(std::shared_ptr<Node> a) {
  std::shared_ptr<Node> out = std::make_shared<Node>(std::sin(a->value));
  out->set_parents(
    {{a, std::cos(a->value)}}
  );
  return out;
}

int main() {
  std::shared_ptr<Node> x = std::make_shared<Node>(2.0);
  std::shared_ptr<Node> y = std::make_shared<Node>(3.0);
  // auto res1 = sin(x*y + y);
  auto res2 = x*y + x;
  // res1->backward();
  res2->backward();
  std::cout << "df/dx = " << x->grad << "\n";
  std::cout << "df/dy = " << y->grad << "\n";
}
