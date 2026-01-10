#include <iostream>
#include <vector>
#include <memory>
#include <array>
#include <utility>
#include <cmath>

enum class Op : uint8_t {
  Const,
  Var,
  Add,
  Mul,
  Sin
};

struct Node {
  Op op;
  int a = -1; // child index (or lhs)
  int b = -1; // rhs index
  double val = 0.0; // primal
  double adj = 0.0; // adjoint
  double c = 0.0; // constant payload
  friend std::ostream& operator<<(std::ostream&, const Node&);
};
std::ostream& operator<<(std::ostream& os, const Node& x) {
  os << x.val << " " << x.adj;
  return os;
}

struct Tape {
  std::vector<Node> nodes;

  int push(Node n) {
    nodes.push_back(n);
    return static_cast<int>(nodes.size() - 1);
  }

  void forward_from(std::size_t start) {
    for (std::size_t i = start; i < nodes.size(); ++i) {
      auto& n = nodes[i];
      switch (n.op) {
        case Op::Const: break;
        case Op::Var:   break;
        case Op::Add:   n.val = nodes[n.a].val + nodes[n.b].val; break;
        case Op::Mul:   n.val = nodes[n.a].val * nodes[n.b].val; break;
        case Op::Sin:   n.val = std::sin(nodes[n.a].val); break;
      }
    }
  }

  void forward() { forward_from(0); }

  void reverse(int out) {
    for (auto& n : nodes) n.adj = 0.0;
    nodes[out].adj = 1.0;

    for (std::size_t ii = nodes.size(); ii-- > 0;) {
      auto& n = nodes[ii];
      const double w = n.adj;
      switch (n.op) {
        case Op::Add:
          nodes[n.a].adj += w;
          nodes[n.b].adj += w;
          break;
        case Op::Mul:
          nodes[n.a].adj += w * nodes[n.b].val;
          nodes[n.b].adj += w * nodes[n.a].val;
          break;
        case Op::Sin: {
          const double x = nodes[n.a].val;
          nodes[n.a].adj += w * std::cos(x);
          break;
        }
        default: break; // Const/Var
      }
    }
  }
};

int constant(Tape& t, double v) {
  return t.push(Node{Op::Const, -1, -1, v, 0.0, v});
}
int var(Tape& t, double v) {
  return t.push(Node{Op::Var, -1, -1, v, 0.0, 0.0});
}
int add(Tape& t, int a, int b) {
  return t.push(Node{Op::Add, a, b});
}
int mul(Tape& t, int a, int b) {
  return t.push(Node{Op::Mul, a, b});
}
int sin(Tape& t, int a) {
  return t.push(Node{Op::Sin, a, -1});
}

int main() {
  Tape tape;

  int ix = var(tape, 2.0);
  int iy = var(tape, 3.0);
  int ixy  = mul(tape, ix, iy);
  int ires = add(tape, ixy, ix); // res = x*y + x
  // Compute primal for what we have so far
  tape.forward();
  // From here on, only append nodes for the executed branch
  std::size_t start = tape.nodes.size();
  const double res_val = tape.nodes[ires].val;
  if (res_val > 10.0) {
    ires = add(tape, ires, iy); // res += y
  } else if (res_val > 5.0) {
    int c2 = constant(tape, 2.0);
    ires = mul(tape, ires, c2); // res *= 2
  } else {
    ires = sin(tape, ires); // res = sin(res)
  }
  tape.forward_from(start);
  tape.reverse(ires);

  std::cout << "x:   " << tape.nodes[ix]  << "\n";
  std::cout << "y:   " << tape.nodes[iy]  << "\n";
  std::cout << "res: " << tape.nodes[ires] << "\n";
}
