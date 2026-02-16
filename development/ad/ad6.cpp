#include <cmath>
#include <cstdint>
#include <limits>
#include <ostream>
#include <vector>
#include <cassert>
#include <iostream>

template<const char* Msg>
inline void ass(bool ok) {
  if (!ok) { throw std::runtime_error(Msg); }
}

enum class ROp : uint8_t {
  Var,     // independent variable (value set on creation)
  Const,   // constant payload
  Add, Sub, Mul, Div, Neg,
  Sin, Cos, Tan, Asin, Acos, Atan, Sinh, Cosh, Tanh,
  Exp, Log, Log10, Sqrt, Pow
};

struct RNode {
  ROp op{};
  int a{-1};
  int b{-1};

  double val{0.0};
  double adj{0.0};
  double c{0.0};
  bool is_na{false};
};

struct ReverseTape {
  std::vector<RNode> nodes;
  void reserve(std::size_t n) { nodes.reserve(n); }
  int push_node(const RNode& n) {
    nodes.push_back(n);
    return static_cast<int>(nodes.size() - 1);
  }

  int push_const(double v, bool is_na=false) {
    RNode n;
    n.op = ROp::Const;
    n.val = v;
    n.c = v;
    n.is_na = is_na;
    return push_node(n);
  }

  int push_var(double v, bool is_na=false) {
    RNode n;
    n.op = ROp::Var;
    n.val = v;
    n.is_na = is_na;
    return push_node(n);
  }

  int push_unary(ROp op, int a) {
    const auto& A = nodes[a];
    RNode n;
    n.op = op;
    n.a = a;
    n.is_na = A.is_na;

    if (n.is_na) {
      n.val = std::numeric_limits<double>::quiet_NaN();
      return push_node(n);
    }

    const double x = A.val;
    switch (op) {
      case ROp::Neg:  n.val = -x; break;
      case ROp::Sin:  n.val = std::sin(x); break;
      case ROp::Cos:  n.val = std::cos(x); break;
      case ROp::Tan:  n.val = std::tan(x); break;
      case ROp::Asin: n.val = std::asin(x); break;
      case ROp::Acos: n.val = std::acos(x); break;
      case ROp::Atan: n.val = std::atan(x); break;
      case ROp::Sinh: n.val = std::sinh(x); break;
      case ROp::Cosh: n.val = std::cosh(x); break;
      case ROp::Tanh: n.val = std::tanh(x); break;
      case ROp::Exp:  n.val = std::exp(x); break;
      case ROp::Log:  n.val = std::log(x); break;
      case ROp::Log10:n.val = std::log10(x); break;
      case ROp::Sqrt: n.val = std::sqrt(x); break;
      default:
        throw std::runtime_error("push_unary: unsupported op");
    }
    return push_node(n);
  }

  int push_binary(ROp op, int a, int b) {
    const auto& A = nodes[a];
    const auto& B = nodes[b];

    RNode n;
    n.op = op;
    n.a = a;
    n.b = b;
    n.is_na = (A.is_na || B.is_na);

    if (n.is_na) {
      n.val = std::numeric_limits<double>::quiet_NaN();
      return push_node(n);
    }

    const double x = A.val;
    const double y = B.val;
    switch (op) {
      case ROp::Add: n.val = x + y; break;
      case ROp::Sub: n.val = x - y; break;
      case ROp::Mul: n.val = x * y; break;
      case ROp::Div: n.val = x / y; break;
      case ROp::Pow: n.val = std::pow(x, y); break;
      default:
        throw std::runtime_error("push_binary: unsupported op");
    }
    return push_node(n);
  }

  void reverse(int out) {
    for (auto& n : nodes) n.adj = 0.0;
    nodes[out].adj = 1.0;
    for (std::size_t ii = nodes.size(); ii-- > 0;) {
      const auto& n = nodes[ii];
      const double w = n.adj;
      if (n.is_na) continue;
      switch (n.op) {
        case ROp::Add:
          nodes[n.a].adj += w;
          nodes[n.b].adj += w;
          break;
        case ROp::Sub:
          nodes[n.a].adj += w;
          nodes[n.b].adj -= w;
          break;
        case ROp::Mul:
          nodes[n.a].adj += w * nodes[n.b].val;
          nodes[n.b].adj += w * nodes[n.a].val;
          break;
        case ROp::Div: {
          const double x = nodes[n.a].val;
          const double y = nodes[n.b].val;
          nodes[n.a].adj += w / y;
          nodes[n.b].adj += w * (-x) / (y * y);
          break;
        }
        case ROp::Neg:
          nodes[n.a].adj += -w;
          break;
        case ROp::Sin: {
          const double x = nodes[n.a].val;
          nodes[n.a].adj += w * std::cos(x);
          break;
        }
        case ROp::Cos: {
          const double x = nodes[n.a].val;
          nodes[n.a].adj += w * (-std::sin(x));
          break;
        }
        case ROp::Tan: {
          const double x = nodes[n.a].val;
          const double c = std::cos(x);
          nodes[n.a].adj += w * (1.0 / (c * c));
          break;
        }
        case ROp::Asin: {
          const double x = nodes[n.a].val;
          nodes[n.a].adj += w * (1.0 / std::sqrt(1.0 - x*x));
          break;
        }
        case ROp::Acos: {
          const double x = nodes[n.a].val;
          nodes[n.a].adj += w * (-1.0 / std::sqrt(1.0 - x*x));
          break;
        }
        case ROp::Atan: {
          const double x = nodes[n.a].val;
          nodes[n.a].adj += w * (1.0 / (1.0 + x*x));
          break;
        }
        case ROp::Sinh: {
          const double x = nodes[n.a].val;
          nodes[n.a].adj += w * std::cosh(x);
          break;
        }
        case ROp::Cosh: {
          const double x = nodes[n.a].val;
          nodes[n.a].adj += w * std::sinh(x);
          break;
        }
        case ROp::Tanh: {
          const double x = nodes[n.a].val;
          const double t = std::tanh(x);
          nodes[n.a].adj += w * (1.0 - t*t);
          break;
        }
        case ROp::Exp: {
          nodes[n.a].adj += w * nodes[ii].val;
          break;
        }
        case ROp::Log: {
          const double x = nodes[n.a].val;
          nodes[n.a].adj += w * (1.0 / x);
          break;
        }
        case ROp::Log10: {
          const double x = nodes[n.a].val;
          nodes[n.a].adj += w * (1.0 / (x * std::log(10.0)));
          break;
        }
        case ROp::Sqrt: {
          const double y = nodes[ii].val;
          nodes[n.a].adj += w * (0.5 / y);
          break;
        }
        case ROp::Pow: {
          const double x = nodes[n.a].val;
          const double y = nodes[n.b].val;
          const double f = nodes[ii].val;
          nodes[n.a].adj += w * (y * f / x);
          nodes[n.b].adj += w * (f * std::log(x));
          break;
        }
        default:
          break;
      }
    }
  }
};

struct ReverseDouble {
  ReverseTape* tape{nullptr};
  int id{-1};
  bool is_na{false};
  inline ReverseDouble() = default;
  inline ReverseDouble(ReverseTape& t, double v) {
    tape = &t;
    is_na = false;
    id = t.push_const(v, false);
  }
  inline ReverseDouble(ReverseTape& t, double v, bool na) {
    tape = &t;
    is_na = na;
    id = t.push_const(na ? std::numeric_limits<double>::quiet_NaN() : v, na);
  }
  // Later construction from etr::Logical, etr::Integer, and etr::Double are required
  static inline ReverseDouble Var(ReverseTape& t, double v) {
    ReverseDouble x;
    x.tape = &t;
    x.is_na = false;
    x.id = t.push_var(v, false);
    return x;
  }
  static inline ReverseDouble NA(ReverseTape& t) {
    ReverseDouble x;
    x.tape = &t;
    x.is_na = true;
    x.id = t.push_const(std::numeric_limits<double>::quiet_NaN(), true);
    return x;
  }
  inline double val() const {
    return tape->nodes[id].val;
  }
  inline double grad() const {
    return tape->nodes[id].adj;
  }
  inline bool isNA() const noexcept { return is_na; }
  inline bool isNaN() const noexcept { return !is_na && std::isnan(val()); }
  inline bool isFinite() const noexcept { return !is_na && std::isfinite(val()); }
  inline bool isInfinite() const noexcept { return !is_na && !isNaN() && !std::isfinite(val()); }
  inline void assert_same_tape(const ReverseDouble& other) const {
#ifndef NDEBUG
    assert(tape == other.tape && "ReverseDouble: operands are on different tapes");
#endif
  }
  inline ReverseDouble operator+(const ReverseDouble& r) const {
    if (is_na || r.is_na) return ReverseDouble::NA(*tape);
    assert_same_tape(r);
    ReverseDouble out;
    out.tape = tape;
    out.is_na = false;
    out.id = tape->push_binary(ROp::Add, id, r.id);
    out.is_na = tape->nodes[out.id].is_na;
    return out;
  }
  inline ReverseDouble operator-(const ReverseDouble& r) const {
    if (is_na || r.is_na) return ReverseDouble::NA(*tape);
    assert_same_tape(r);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_binary(ROp::Sub, id, r.id);
    out.is_na = tape->nodes[out.id].is_na;
    return out;
  }
  inline ReverseDouble operator*(const ReverseDouble& r) const {
    if (is_na || r.is_na) return ReverseDouble::NA(*tape);
    assert_same_tape(r);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_binary(ROp::Mul, id, r.id);
    out.is_na = tape->nodes[out.id].is_na;
    return out;
  }
  inline ReverseDouble operator/(const ReverseDouble& r) const {
    if (is_na || r.is_na) return ReverseDouble::NA(*tape);
    assert_same_tape(r);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_binary(ROp::Div, id, r.id);
    out.is_na = tape->nodes[out.id].is_na;
    return out;
  }

  inline ReverseDouble& operator+=(const ReverseDouble& r) { return *this = (*this + r); }
  inline ReverseDouble& operator-=(const ReverseDouble& r) { return *this = (*this - r); }
  inline ReverseDouble& operator*=(const ReverseDouble& r) { return *this = (*this * r); }
  inline ReverseDouble& operator/=(const ReverseDouble& r) { return *this = (*this / r); }

  inline ReverseDouble operator-() const {
    if (is_na) return ReverseDouble::NA(*tape);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_unary(ROp::Neg, id);
    out.is_na = tape->nodes[out.id].is_na;
    return out;
  }

  inline ReverseDouble pow(const ReverseDouble& r) const {
    if (is_na || r.is_na) return ReverseDouble::NA(*tape);
    assert_same_tape(r);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_binary(ROp::Pow, id, r.id);
    out.is_na = tape->nodes[out.id].is_na;
    return out;
  }
  // Later CmpLogical is replaced by etr::Logical
  struct CmpLogical {
    bool val{false};
    bool is_na{false};
    inline operator bool() const { return val; }
    static inline CmpLogical NA() { CmpLogical x; x.is_na = true; return x; }
  };

  inline CmpLogical operator==(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() == r.val(), false};
  }
  inline CmpLogical operator<(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() < r.val(), false};
  }
  inline CmpLogical operator<=(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() <= r.val(), false};
  }
  inline CmpLogical operator>(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() > r.val(), false};
  }
  inline CmpLogical operator>=(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() >= r.val(), false};
  }
  inline CmpLogical operator!=(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() != r.val(), false};
  }

  inline ReverseDouble sin() const  { return unary(ROp::Sin); }
  inline ReverseDouble asin() const { return unary(ROp::Asin); }
  inline ReverseDouble sinh() const { return unary(ROp::Sinh); }
  inline ReverseDouble cos() const  { return unary(ROp::Cos); }
  inline ReverseDouble acos() const { return unary(ROp::Acos); }
  inline ReverseDouble cosh() const { return unary(ROp::Cosh); }
  inline ReverseDouble tan() const  { return unary(ROp::Tan); }
  inline ReverseDouble atan() const { return unary(ROp::Atan); }
  inline ReverseDouble tanh() const { return unary(ROp::Tanh); }
  inline ReverseDouble exp() const  { return unary(ROp::Exp); }
  inline ReverseDouble log() const  { return unary(ROp::Log); }
  inline ReverseDouble log10() const{ return unary(ROp::Log10); }
  inline ReverseDouble sqrt() const { return unary(ROp::Sqrt); }

  inline ReverseDouble unary(ROp op) const {
    if (is_na) return ReverseDouble::NA(*tape);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_unary(op, id);
    out.is_na = tape->nodes[out.id].is_na;
    return out;
  }

  inline friend std::ostream& operator<<(std::ostream& os, const ReverseDouble& x) {
    if (x.isNA()) return os << "NA";
    const double v = x.val();
    if (std::isnan(v)) return os << "NaN";
    if (!std::isfinite(v)) return os << (v > 0 ? "Inf" : "-Inf");
    return os << v;
  }
};


int main() {
  // Later forbid TAPE_INTERN as variable name
  ReverseTape TAPE_INTERN;
  auto x = ReverseDouble::Var(TAPE_INTERN, 2.0);
  auto y = ReverseDouble::Var(TAPE_INTERN, 3.0);
  auto res = x * y + x;

  if (res.val() > 10.0) {
    res = res + y;
  } else if (res.val() > 5.0) {
    res = res * ReverseDouble(TAPE_INTERN, 2.0);
  } else {
    res = res.sin();
  }
  TAPE_INTERN.reverse(res.id);
  std::cout << "res=" << res << "\n";
  std::cout << "dx=" << x.grad() << "\n";
  std::cout << "dy=" << y.grad() << "\n";
}
