#ifndef ETR_REVERSE_AD_HPP
#define ETR_REVERSE_AD_HPP

// Reverse-mode AD prototype for ast2ast.
//
// Design:
// - Single flat tape per thread, SoA layout. See ad7.cpp for the earlier
//   standalone walkthrough.
// - `ReverseDouble` is 8 bytes: { int id, bool is_na }. No tape pointer —
//   all scalar operations route through `etr::TAPE_INTERN`, which is
//   `thread_local`. Each thread owns its own tape; translated functions
//   call `etr::TAPE_INTERN.clear()` at entry to amortize allocation across
//   repeated invocations on that thread.
// - `CmpLogical` is a stub that stands in for `etr::Logical`. When this
//   file migrates into the tree, swap it out for `etr::Logical`.

#include <cmath>
#include <cstdint>
#include <limits>
#include <ostream>
#include <vector>

namespace etr {

//==============================================================================
// Tape
//==============================================================================

enum class ROp : uint8_t {
  Var, Const,
  Add, Sub, Mul, Div, Neg,
  Sin, Cos, Tan, Asin, Acos, Atan, Sinh, Cosh, Tanh,
  Exp, Log, Log10, Sqrt, Pow
};

struct ReverseTape {
  std::vector<ROp>     op;
  std::vector<int>     a;
  std::vector<int>     b;
  std::vector<double>  val;
  std::vector<double>  adj;
  std::vector<uint8_t> is_na; // uint8_t on purpose — avoids vector<bool>.

  inline std::size_t size() const noexcept { return op.size(); }

  // Amortizes allocation across sequential calls on the same thread:
  // vector::clear() keeps capacity, only resets size to zero.
  inline void clear() noexcept {
    op.clear(); a.clear(); b.clear();
    val.clear(); adj.clear(); is_na.clear();
  }

  inline void reserve(std::size_t n) {
    op.reserve(n); a.reserve(n); b.reserve(n);
    val.reserve(n); adj.reserve(n); is_na.reserve(n);
  }

  inline bool node_is_na(int i) const noexcept {
    return is_na[static_cast<std::size_t>(i)] != 0;
  }

  inline int push_node(ROp op_i, int a_i, int b_i, double v_i, bool na_i) {
    op.push_back(op_i);
    a.push_back(a_i);
    b.push_back(b_i);
    val.push_back(v_i);
    adj.push_back(0.0);
    is_na.push_back(static_cast<uint8_t>(na_i));
    return static_cast<int>(op.size() - 1);
  }

  inline int push_const(double v, bool na = false) {
    if (na) v = std::numeric_limits<double>::quiet_NaN();
    return push_node(ROp::Const, -1, -1, v, na);
  }

  inline int push_var(double v, bool na = false) {
    if (na) v = std::numeric_limits<double>::quiet_NaN();
    return push_node(ROp::Var, -1, -1, v, na);
  }

  int push_unary(ROp u, int ai) {
    if (node_is_na(ai)) {
      return push_node(u, ai, -1,
                       std::numeric_limits<double>::quiet_NaN(),
                       true);
    }
    const double x = val[static_cast<std::size_t>(ai)];
    double outv = 0.0;
    switch (u) {
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
    return push_node(u, ai, -1, outv, false);
  }

  int push_binary(ROp bo, int ai, int bi) {
    if (node_is_na(ai) || node_is_na(bi)) {
      return push_node(bo, ai, bi,
                       std::numeric_limits<double>::quiet_NaN(),
                       true);
    }
    const double x = val[static_cast<std::size_t>(ai)];
    const double y = val[static_cast<std::size_t>(bi)];
    double outv = 0.0;
    switch (bo) {
      case ROp::Add: outv = x + y; break;
      case ROp::Sub: outv = x - y; break;
      case ROp::Mul: outv = x * y; break;
      case ROp::Div: outv = x / y; break;
      case ROp::Pow: outv = std::pow(x, y); break;
      default:
        throw std::runtime_error("push_binary: unsupported op");
    }
    return push_node(bo, ai, bi, outv, false);
  }

  void reverse(int out) {
    std::fill(adj.begin(), adj.end(), 0.0);
    adj[static_cast<std::size_t>(out)] = 1.0;
    for (std::size_t ii = size(); ii-- > 0;) {
      if (is_na[ii]) continue;
      const double w = adj[ii];
      const ROp   op_i = op[ii];
      const int   ai   = a[ii];
      const int   bi   = b[ii];
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
          const double c = std::cos(x);
          adj[static_cast<std::size_t>(ai)] += w * (1.0 / (c * c));
          break;
        }
        case ROp::Asin: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * (1.0 / std::sqrt(1.0 - x * x));
          break;
        }
        case ROp::Acos: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * (-1.0 / std::sqrt(1.0 - x * x));
          break;
        }
        case ROp::Atan: {
          const double x = val[static_cast<std::size_t>(ai)];
          adj[static_cast<std::size_t>(ai)] += w * (1.0 / (1.0 + x * x));
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
          adj[static_cast<std::size_t>(ai)] += w * (1.0 - t * t);
          break;
        }
        case ROp::Exp:
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
          const double y = val[ii];
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
          // Var / Const: no adjoint propagation
          break;
      }
    }
  }
};

//==============================================================================
// Thread-local tape
//
// One tape per thread. Each translated function call should start with
// `etr::TAPE_INTERN.clear()`, which keeps the allocation and resets size.
//==============================================================================

inline thread_local ReverseTape TAPE_INTERN;

//==============================================================================
// Logical stub. Replace with etr::Logical at integration time.
//==============================================================================

struct CmpLogical {
  bool val{false};
  bool is_na{false};
  inline operator bool() const { return val; }
  inline static CmpLogical NA() { CmpLogical x; x.is_na = true; return x; }
};

//==============================================================================
// ReverseDouble
//==============================================================================

struct ReverseDouble {
  int  id{-1};
  bool is_na{false};

  //--- construction -----------------------------------------------------------

  inline ReverseDouble() {
    id = TAPE_INTERN.push_const(0.0, false);
  }
  inline ReverseDouble(double v) {
    id = TAPE_INTERN.push_const(v, false);
  }
  inline ReverseDouble(int v)  : ReverseDouble(static_cast<double>(v)) {}
  inline ReverseDouble(bool v) : ReverseDouble(v ? 1.0 : 0.0) {}

  // Copy / move: default is correct. An assignment `x = y` where both are
  // ReverseDouble should NOT push a new node — it just rebinds x.id to y.id,
  // which is the semantics of an R let-binding `x <- y`. A later
  // `x = y + z` will push normally.
  ReverseDouble(const ReverseDouble&)            = default;
  ReverseDouble(ReverseDouble&&) noexcept        = default;
  ReverseDouble& operator=(const ReverseDouble&) = default;
  ReverseDouble& operator=(ReverseDouble&&)      = default;

  //--- factories --------------------------------------------------------------

  static inline ReverseDouble Var(double v) {
    ReverseDouble x;
    x.is_na = false;
    x.id = TAPE_INTERN.push_var(v, false);
    return x;
  }
  static inline ReverseDouble NA() {
    ReverseDouble x;
    x.is_na = true;
    x.id = TAPE_INTERN.push_const(std::numeric_limits<double>::quiet_NaN(), true);
    return x;
  }
  static inline ReverseDouble NaN() {
    ReverseDouble x;
    x.is_na = false;
    x.id = TAPE_INTERN.push_const(std::numeric_limits<double>::quiet_NaN(), false);
    return x;
  }
  static inline ReverseDouble Inf() {
    ReverseDouble x;
    x.is_na = false;
    x.id = TAPE_INTERN.push_const(std::numeric_limits<double>::infinity(), false);
    return x;
  }

  //--- accessors --------------------------------------------------------------

  inline double val()  const { return TAPE_INTERN.val [static_cast<std::size_t>(id)]; }
  inline double grad() const { return TAPE_INTERN.adj [static_cast<std::size_t>(id)]; }

  inline bool isNA()       const noexcept { return is_na; }
  inline bool isNaN()      const noexcept { return !is_na && std::isnan(val()); }
  inline bool isFinite()   const noexcept { return !is_na && std::isfinite(val()); }
  inline bool isInfinite() const noexcept { return !is_na && !isNaN() && !std::isfinite(val()); }

  //--- unary / binary helpers -------------------------------------------------

  inline ReverseDouble unary_(ROp u) const {
    if (is_na) return NA();
    ReverseDouble out;
    out.id = TAPE_INTERN.push_unary(u, id);
    out.is_na = TAPE_INTERN.is_na[static_cast<std::size_t>(out.id)] != 0;
    return out;
  }
  inline ReverseDouble binary_(ROp bo, const ReverseDouble& r) const {
    if (is_na || r.is_na) return NA();
    ReverseDouble out;
    out.id = TAPE_INTERN.push_binary(bo, id, r.id);
    out.is_na = TAPE_INTERN.is_na[static_cast<std::size_t>(out.id)] != 0;
    return out;
  }

  //--- arithmetic -------------------------------------------------------------

  inline ReverseDouble operator+(const ReverseDouble& r) const { return binary_(ROp::Add, r); }
  inline ReverseDouble operator-(const ReverseDouble& r) const { return binary_(ROp::Sub, r); }
  inline ReverseDouble operator*(const ReverseDouble& r) const { return binary_(ROp::Mul, r); }
  inline ReverseDouble operator/(const ReverseDouble& r) const { return binary_(ROp::Div, r); }
  inline ReverseDouble pow       (const ReverseDouble& r) const { return binary_(ROp::Pow, r); }

  inline ReverseDouble& operator+=(const ReverseDouble& r) { return *this = (*this + r); }
  inline ReverseDouble& operator-=(const ReverseDouble& r) { return *this = (*this - r); }
  inline ReverseDouble& operator*=(const ReverseDouble& r) { return *this = (*this * r); }
  inline ReverseDouble& operator/=(const ReverseDouble& r) { return *this = (*this / r); }

  inline ReverseDouble operator-() const {
    if (is_na) return NA();
    ReverseDouble out;
    out.id = TAPE_INTERN.push_unary(ROp::Neg, id);
    out.is_na = TAPE_INTERN.is_na[static_cast<std::size_t>(out.id)] != 0;
    return out;
  }

  //--- comparisons (return CmpLogical stub) ----------------------------------

  inline CmpLogical operator==(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() == r.val(), false};
  }
  inline CmpLogical operator!=(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() != r.val(), false};
  }
  inline CmpLogical operator< (const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() <  r.val(), false};
  }
  inline CmpLogical operator<=(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() <= r.val(), false};
  }
  inline CmpLogical operator> (const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() >  r.val(), false};
  }
  inline CmpLogical operator>=(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() >= r.val(), false};
  }

  //--- logical --------------------------------------------------------------

  inline CmpLogical operator&&(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() != 0.0 && r.val() != 0.0, false};
  }
  inline CmpLogical operator||(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() != 0.0 || r.val() != 0.0, false};
  }
  inline CmpLogical operator&(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() != 0.0 && r.val() != 0.0, false};
  }
  inline CmpLogical operator|(const ReverseDouble& r) const {
    if (is_na || r.is_na) return CmpLogical::NA();
    return CmpLogical{val() != 0.0 || r.val() != 0.0, false};
  }

  //--- unary math -----------------------------------------------------------

  inline ReverseDouble sin()   const { return unary_(ROp::Sin);   }
  inline ReverseDouble cos()   const { return unary_(ROp::Cos);   }
  inline ReverseDouble tan()   const { return unary_(ROp::Tan);   }
  inline ReverseDouble asin()  const { return unary_(ROp::Asin);  }
  inline ReverseDouble acos()  const { return unary_(ROp::Acos);  }
  inline ReverseDouble atan()  const { return unary_(ROp::Atan);  }
  inline ReverseDouble sinh()  const { return unary_(ROp::Sinh);  }
  inline ReverseDouble cosh()  const { return unary_(ROp::Cosh);  }
  inline ReverseDouble tanh()  const { return unary_(ROp::Tanh);  }
  inline ReverseDouble exp()   const { return unary_(ROp::Exp);   }
  inline ReverseDouble log()   const { return unary_(ROp::Log);   }
  inline ReverseDouble log10() const { return unary_(ROp::Log10); }
  inline ReverseDouble sqrt()  const { return unary_(ROp::Sqrt);  }

  //--- stream output --------------------------------------------------------

  inline friend std::ostream& operator<<(std::ostream& os, const ReverseDouble& x) {
    if (x.isNA()) return os << "NA";
    const double v = x.val();
    if (std::isnan(v)) return os << "NaN";
    if (!std::isfinite(v)) return os << (v > 0 ? "Inf" : "-Inf");
    return os << v;
  }
};

//==============================================================================
// Free functions expected by etr's generic templates.
// Add more as integration proceeds (get_scalar_val variants for Expr/Variable
// compatibility, type traits registration, etc.).
//==============================================================================

inline double        get_val(const ReverseDouble& x)        { return x.val(); }
inline ReverseDouble get_scalar_val(const ReverseDouble& x) { return x; }

} // namespace etr

#endif
