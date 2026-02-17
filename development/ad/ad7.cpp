#define STANDALONE_ETR

#include <cmath>
#include <cstdint>
#include <limits>
#include <vector>
#include <iostream>

// replace throw with etr::ass
enum class ROp : uint8_t {
  Var,     // independent variable (value set on creation)
  Const,   // constant payload
  Add, Sub, Mul, Div, Neg,
  Sin, Cos, Tan, Asin, Acos, Atan, Sinh, Cosh, Tanh,
  Exp, Log, Log10, Sqrt, Pow
};

struct ReverseTape {
  std::vector<ROp>    op;
  std::vector<int>    a;
  std::vector<int>    b;
  std::vector<double> val;
  std::vector<double> adj;
  std::vector<double> c;
  std::vector<uint8_t> is_na; // avoid std::vector<bool> specialization

  inline std::size_t size() const noexcept { return op.size(); }

  void reserve(std::size_t n) {
    op.reserve(n); a.reserve(n); b.reserve(n);
    val.reserve(n); adj.reserve(n); c.reserve(n);
    is_na.reserve(n);
  }

  // internal unified push
  int push_node(ROp op_i, int a_i, int b_i, double val_i, double c_i, bool na_i) {
    op.push_back(op_i);
    a.push_back(a_i);
    b.push_back(b_i);
    val.push_back(val_i);
    adj.push_back(0.0);
    c.push_back(c_i);
    is_na.push_back(static_cast<uint8_t>(na_i));
    return static_cast<int>(op.size() - 1);
  }

  inline bool node_is_na(int i) const noexcept {
    return is_na[static_cast<std::size_t>(i)] != 0;
  }

  int push_const(double v, bool na=false) {
    if (na) v = std::numeric_limits<double>::quiet_NaN();
    return push_node(ROp::Const, -1, -1, v, v, na);
  }

  int push_var(double v, bool na=false) {
    if (na) v = std::numeric_limits<double>::quiet_NaN();
    return push_node(ROp::Var, -1, -1, v, 0.0, na);
  }

  int push_unary(ROp op_u, int ai) {
    const bool na = node_is_na(ai);

    if (na) {
      return push_node(op_u, ai, -1,
                       std::numeric_limits<double>::quiet_NaN(),
                       0.0,
                       true);
    }

    const double x = val[static_cast<std::size_t>(ai)];
    double outv = 0.0;

    switch (op_u) {
      case ROp::Neg:   outv = -x; break;
      case ROp::Sin:   outv = std::sin(x); break;
      case ROp::Cos:   outv = std::cos(x); break;
      case ROp::Tan:   outv = std::tan(x); break;
      case ROp::Asin:  outv = std::asin(x); break;
      case ROp::Acos:  outv = std::acos(x); break;
      case ROp::Atan:  outv = std::atan(x); break;
      case ROp::Sinh:  outv = std::sinh(x); break;
      case ROp::Cosh:  outv = std::cosh(x); break;
      case ROp::Tanh:  outv = std::tanh(x); break;
      case ROp::Exp:   outv = std::exp(x); break;
      case ROp::Log:   outv = std::log(x); break;
      case ROp::Log10: outv = std::log10(x); break;
      case ROp::Sqrt:  outv = std::sqrt(x); break;
      default:
        throw std::runtime_error("push_unary: unsupported op");
    }

    return push_node(op_u, ai, -1, outv, 0.0, false);
  }

  int push_binary(ROp op_b, int ai, int bi) {
    const bool na = node_is_na(ai) || node_is_na(bi);

    if (na) {
      return push_node(op_b, ai, bi,
                       std::numeric_limits<double>::quiet_NaN(),
                       0.0,
                       true);
    }

    const double x = val[static_cast<std::size_t>(ai)];
    const double y = val[static_cast<std::size_t>(bi)];
    double outv = 0.0;

    switch (op_b) {
      case ROp::Add: outv = x + y; break;
      case ROp::Sub: outv = x - y; break;
      case ROp::Mul: outv = x * y; break;
      case ROp::Div: outv = x / y; break;
      case ROp::Pow: outv = std::pow(x, y); break;
      default:
        throw std::runtime_error("push_binary: unsupported op");
    }

    return push_node(op_b, ai, bi, outv, 0.0, false);
  }

  void reverse(int out) {
    // reset adjoints
    for (std::size_t i = 0; i < adj.size(); ++i) adj[i] = 0.0;
    adj[static_cast<std::size_t>(out)] = 1.0;

    for (std::size_t ii = size(); ii-- > 0;) {
      const double w = adj[ii];
      if (is_na[ii]) continue;

      const ROp op_i = op[ii];
      const int ai = a[ii];
      const int bi = b[ii];

      switch (op_i) {
        case ROp::Add:
          adj[static_cast<std::size_t>(ai)] += w;
          adj[static_cast<std::size_t>(bi)] += w;
          break;

        case ROp::Sub:
          adj[static_cast<std::size_t>(ai)] += w;
          adj[static_cast<std::size_t>(bi)] -= w;
          break;

        case ROp::Mul:
          adj[static_cast<std::size_t>(ai)] += w * val[static_cast<std::size_t>(bi)];
          adj[static_cast<std::size_t>(bi)] += w * val[static_cast<std::size_t>(ai)];
          break;

        case ROp::Div: {
          const double x = val[static_cast<std::size_t>(ai)];
          const double y = val[static_cast<std::size_t>(bi)];
          adj[static_cast<std::size_t>(ai)] += w / y;
          adj[static_cast<std::size_t>(bi)] += w * (-x) / (y * y);
          break;
        }

        case ROp::Neg:
          adj[static_cast<std::size_t>(ai)] += -w;
          break;

        case ROp::Sin: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * std::cos(x);
          break;
        }
        case ROp::Cos: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * (-std::sin(x));
          break;
        }
        case ROp::Tan: {
          const double x = val[static_cast<std::size_t>(ai)];
          const double cc = std::cos(x);
          adj[static_cast<std::size_t>(ai)] += w * (1.0 / (cc * cc));
          break;
        }

        case ROp::Asin: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * (1.0 / std::sqrt(1.0 - x*x));
          break;
        }
        case ROp::Acos: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * (-1.0 / std::sqrt(1.0 - x*x));
          break;
        }
        case ROp::Atan: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * (1.0 / (1.0 + x*x));
          break;
        }

        case ROp::Sinh: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * std::cosh(x);
          break;
        }
        case ROp::Cosh: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * std::sinh(x);
          break;
        }
        case ROp::Tanh: {
          const double x = val[static_cast<std::size_t>(ai)];
          const double t = std::tanh(x);
          adj[static_cast<std::size_t>(ai)] += w * (1.0 - t*t);
          break;
        }

        case ROp::Exp:
          // d/dx exp(x) = exp(x) == val[ii]
          adj[static_cast<std::size_t>(ai)] += w * val[ii];
          break;

        case ROp::Log: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * (1.0 / x);
          break;
        }
        case ROp::Log10: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * (1.0 / (x * std::log(10.0)));
          break;
        }
        case ROp::Sqrt: {
          const double y = val[ii]; // sqrt(x)
          adj[static_cast<std::size_t>(ai)] += w * (0.5 / y);
          break;
        }

        case ROp::Pow: {
          const double x = val[static_cast<std::size_t>(ai)];
          const double y = val[static_cast<std::size_t>(bi)];
          const double f = val[ii];
          adj[static_cast<std::size_t>(ai)] += w * (y * f / x);
          adj[static_cast<std::size_t>(bi)] += w * (f * std::log(x));
          break;
        }

        default:
          // Var / Const: nothing to do
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
    return tape->val[static_cast<std::size_t>(id)];
  }

  inline double grad() const {
    return tape->adj[static_cast<std::size_t>(id)];
  }

  inline bool isNA() const noexcept { return is_na; }
  inline bool isNaN() const noexcept { return !is_na && std::isnan(val()); }
  inline bool isFinite() const noexcept { return !is_na && std::isfinite(val()); }
  inline bool isInfinite() const noexcept { return !is_na && !isNaN() && !std::isfinite(val()); }

  inline void assert_same_tape(const ReverseDouble& other) const {
#ifndef NDEBUG
    // ass<"ReverseDouble: operands are on different tapes">(tape == other.tape);
#endif
  }

  inline ReverseDouble operator+(const ReverseDouble& r) const {
    if (is_na || r.is_na) return ReverseDouble::NA(*tape);
    assert_same_tape(r);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_binary(ROp::Add, id, r.id);
    out.is_na = tape->is_na[static_cast<std::size_t>(out.id)] != 0;
    return out;
  }

  inline ReverseDouble operator-(const ReverseDouble& r) const {
    if (is_na || r.is_na) return ReverseDouble::NA(*tape);
    assert_same_tape(r);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_binary(ROp::Sub, id, r.id);
    out.is_na = tape->is_na[static_cast<std::size_t>(out.id)] != 0;
    return out;
  }

  inline ReverseDouble operator*(const ReverseDouble& r) const {
    if (is_na || r.is_na) return ReverseDouble::NA(*tape);
    assert_same_tape(r);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_binary(ROp::Mul, id, r.id);
    out.is_na = tape->is_na[static_cast<std::size_t>(out.id)] != 0;
    return out;
  }

  inline ReverseDouble operator/(const ReverseDouble& r) const {
    if (is_na || r.is_na) return ReverseDouble::NA(*tape);
    assert_same_tape(r);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_binary(ROp::Div, id, r.id);
    out.is_na = tape->is_na[static_cast<std::size_t>(out.id)] != 0;
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
    out.is_na = tape->is_na[static_cast<std::size_t>(out.id)] != 0;
    return out;
  }

  inline ReverseDouble pow(const ReverseDouble& r) const {
    if (is_na || r.is_na) return ReverseDouble::NA(*tape);
    assert_same_tape(r);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_binary(ROp::Pow, id, r.id);
    out.is_na = tape->is_na[static_cast<std::size_t>(out.id)] != 0;
    return out;
  }

  // Later: replace with etr::Logical
  // Missing: &&, &, ||, and |
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

  inline ReverseDouble sin() const   { return unary(ROp::Sin); }
  inline ReverseDouble asin() const  { return unary(ROp::Asin); }
  inline ReverseDouble sinh() const  { return unary(ROp::Sinh); }
  inline ReverseDouble cos() const   { return unary(ROp::Cos); }
  inline ReverseDouble acos() const  { return unary(ROp::Acos); }
  inline ReverseDouble cosh() const  { return unary(ROp::Cosh); }
  inline ReverseDouble tan() const   { return unary(ROp::Tan); }
  inline ReverseDouble atan() const  { return unary(ROp::Atan); }
  inline ReverseDouble tanh() const  { return unary(ROp::Tanh); }
  inline ReverseDouble exp() const   { return unary(ROp::Exp); }
  inline ReverseDouble log() const   { return unary(ROp::Log); }
  inline ReverseDouble log10() const { return unary(ROp::Log10); }
  inline ReverseDouble sqrt() const  { return unary(ROp::Sqrt); }

  inline ReverseDouble unary(ROp op) const {
    if (is_na) return ReverseDouble::NA(*tape);
    ReverseDouble out;
    out.tape = tape;
    out.id = tape->push_unary(op, id);
    out.is_na = tape->is_na[static_cast<std::size_t>(out.id)] != 0;
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

  if (res > ReverseDouble(TAPE_INTERN, 10.0)) {
    res = res + y;
  } else if (res > ReverseDouble(TAPE_INTERN, 5.0)) {
    res = res * ReverseDouble(TAPE_INTERN, 2.0);
  } else {
    res = res.sin();
  }

  TAPE_INTERN.reverse(res.id);

  std::cout << "res=" << res << "\n";
  std::cout << "dx=" << x.grad() << "\n";
  std::cout << "dy=" << y.grad() << "\n";
}
