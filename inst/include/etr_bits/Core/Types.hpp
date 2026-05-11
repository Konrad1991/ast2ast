#ifndef TYPES_ETR_H
#define TYPES_ETR_H

#include <cmath>
#include <limits>

#ifdef STANDALONE_ETR
#include "stddef.h"
#include <cxxabi.h>
// Standalone mode still uses BLAS for matmul; declare dgemm by hand and
// shim F77_CALL/FCONE so the call sites can be uniform with the R build.
extern "C" void dgemm_(const char* transa, const char* transb,
                       const int* m, const int* n, const int* k,
                       const double* alpha, const double* a, const int* lda,
                       const double* b, const int* ldb,
                       const double* beta, double* c, const int* ldc);
#define F77_CALL(x) x##_
#define FCONE
#else
#include <R_ext/BLAS.h>
#include <R_ext/RS.h>
#ifndef FCONE
#define FCONE
#endif
#endif

#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <array>
#include <ios>
#include <iostream>
#include <iterator>
#include <math.h>
#include <memory>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>
#include <functional>
#include <numeric>

namespace etr {

// Messages/Warnings and Errors
// -----------------------------------------------------------------------------------------------------------
inline void ass(bool inp, const std::string &message) {
#ifdef STANDALONE_ETR
  if (!inp)
    throw std::runtime_error(message);
#else
  if (!inp)
    Rcpp::stop(message);
#endif
}

// https://ctrpeach.io/posts/cpp20-string-literal-template-parameters/
template <std::size_t N> struct string_literal {
  constexpr string_literal(const char (&str)[N]) {
    std::copy_n(str, N, value.begin());
  }
  std::array<char, N> value;
};

template <string_literal msg> inline void ass(bool inp) {
#ifdef STANDALONE_ETR
  if (!inp) throw std::runtime_error(msg.value.data());
#else
  if (!inp) Rcpp::stop(msg.value.data());
#endif
}

inline void warn(bool inp, std::string message) {
#ifdef STANDALONE_ETR
  if (!inp) std::cerr << "Warning: " + message << std::endl;
#else
  if (!inp) Rcpp::warning("Warning: " + message);
#endif
}

template <string_literal msg> inline void warn(bool inp) {
#ifdef STANDALONE_ETR
  if (!inp) std::cerr << msg.value.data() << std::endl;
#else
  if (!inp) Rcpp::warning(msg.value.data());
#endif
}

// Handler which store reference to r for L values and otherwise copy the value to keep it alive
// -----------------------------------------------------------------------------------------------------------
template <class T>
class ConstHolder {
  std::shared_ptr<T> owned; // used only when constructed from rvalue
  const T* ptr = nullptr;

public:
  ConstHolder(const T& ref) : ptr(&ref) {}

  ConstHolder(T&& r) : owned(std::make_shared<T>(std::move(r))), ptr(owned.get()) {}

  ConstHolder(const ConstHolder&) = default;
  ConstHolder(ConstHolder&&) noexcept = default;
  ConstHolder& operator=(const ConstHolder&) = default;
  ConstHolder& operator=(ConstHolder&&) noexcept = default;

  const T& get() const { return *ptr; }
};


template <typename T>
class Holder {
  std::optional<T> val;
  T* ptr = nullptr;
public:
  // Construct from lvalue
  Holder(T& ref) : ptr(&ref) {}
  // Construct from rvalue
  Holder(T&& r) {
    val.emplace(std::move(r));
    ptr = &val.value();
  }
  // Copy constructor
  Holder(const Holder& other) {
    if (other.val.has_value()) {
      val.emplace(*other.val);
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }
  // Copy assignment
  Holder& operator=(const Holder& other) {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }
  // Move constructor
  Holder(Holder&& other) noexcept {
    if (other.val.has_value()) {
      val.emplace(std::move(*other.val));
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }
  // Move assignment
  Holder& operator=(Holder&& other) noexcept {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }
  T& get() const {
    return *ptr;
  }
};

// Helper
// -----------------------------------------------------------------------------------------------------------
template <typename T, typename U>
inline constexpr bool IS = std::is_same_v<T, U>;

// Reverse-mode AD tape (used by ReverseDouble, defined further down with the other scalar types).
// The tape is self-contained: only primitive types, std::vector, and <cmath>.
// One tape per thread; translated functions call TAPE_INTERN.clear() at entry, which resets size without freeing capacity.
enum class ROp : uint8_t {
  Var, Const,
  Add, Sub, Mul, Div, Neg,
  Sin, Cos, Tan, Asin, Acos, Atan, Sinh, Cosh, Tanh,
  Exp, Log, Log10, Sqrt, Pow,
  // A MatMul produces M*N output cells. We push M*N consecutive tape nodes
  // so downstream ops can reference any cell by tape id. The FIRST node is
  // tagged MatMul and carries the whole block's backward pass; the other
  // M*N-1 nodes are tagged BlockSlot and are no-ops in reverse(). Both
  // node kinds store block_idx (index into ReverseTape::blocks) in a[],
  // NOT a tape id.
  MatMul, BlockSlot
};

struct BlockOp {
  enum class Kind : uint8_t { MatMul };
  Kind        kind;
  std::size_t rows_a;
  std::size_t cols_a;     // == rows_b
  std::size_t cols_b;
  // Per side, exactly one of {ids, vals} is populated. ids = ReverseDouble
  // input (values pulled from tape at backward time). vals = r-value input
  // (Double/Integer/Logical), copied here so backward survives the original
  // memory going out of scope.
  std::vector<int>    a_ids;
  std::vector<double> a_vals;
  std::vector<int>    b_ids;
  std::vector<double> b_vals;
  int         out_tape_id_first{-1};   // tape id of the head node; outputs occupy [out_tape_id_first, out_tape_id_first + rows_a*cols_b)
};

struct ReverseTape {
  std::vector<ROp>     op;
  std::vector<int>     a;
  std::vector<int>     b;
  std::vector<double>  val;
  std::vector<double>  adj;
  std::vector<uint8_t> is_na; // uint8_t on purpose — avoids vector<bool>.
  std::vector<BlockOp> blocks;

  inline std::size_t size() const noexcept { return op.size(); }

  inline void clear() noexcept {
    op.clear(); a.clear(); b.clear();
    val.clear(); adj.clear(); is_na.clear();
    blocks.clear();
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

  // Block matrix multiply C = A * B, column-major (R convention).
  // A is rows_a x cols_a, B is cols_a x cols_b, C is rows_a x cols_b.
  // All three buffers laid out as A[m,k] = idx[k*rows_a + m], etc.
  // Per side, exactly one of {ids, vals} must be non-empty (the other empty).
  // Returns the head tape id; output cells occupy [head, head + rows_a*cols_b).
  inline int push_matmul(std::size_t rows_a, std::size_t cols_a, std::size_t cols_b,
                         std::vector<int>    a_ids,
                         std::vector<double> a_vals,
                         std::vector<int>    b_ids,
                         std::vector<double> b_vals) {
    ass<"push_matmul: A side requires exactly one of {ids, vals}">(a_ids.empty() != a_vals.empty());
    ass<"push_matmul: B side requires exactly one of {ids, vals}">(b_ids.empty() != b_vals.empty());
    const std::size_t a_size = a_ids.empty() ? a_vals.size() : a_ids.size();
    const std::size_t b_size = b_ids.empty() ? b_vals.size() : b_ids.size();
    ass<"push_matmul: A size mismatch">(a_size == rows_a * cols_a);
    ass<"push_matmul: B size mismatch">(b_size == cols_a * cols_b);

    const std::size_t out_size = rows_a * cols_b;
    const int         block_idx = static_cast<int>(blocks.size());

    // Pre-scan NA per row of A and per column of B. C[i,j] is NA iff
    // any A[i,*] is NA OR any B[*,j] is NA — same poisoning rule as R's %*%.
    // For ids inputs we read the authoritative is_na flag; for vals inputs
    // we treat NaN as NA (the distinction was already lost upstream).
    std::vector<uint8_t> a_row_na(rows_a, 0);
    std::vector<uint8_t> b_col_na(cols_b, 0);
    for (std::size_t i = 0; i < rows_a; ++i) {
      for (std::size_t k = 0; k < cols_a; ++k) {
        const std::size_t idx = k * rows_a + i;
        const bool na_here = a_ids.empty()
          ? std::isnan(a_vals[idx])
          : (is_na[static_cast<std::size_t>(a_ids[idx])] != 0);
        if (na_here) { a_row_na[i] = 1; break; }
      }
    }
    for (std::size_t j = 0; j < cols_b; ++j) {
      for (std::size_t k = 0; k < cols_a; ++k) {
        const std::size_t idx = j * cols_a + k;
        const bool na_here = b_ids.empty()
          ? std::isnan(b_vals[idx])
          : (is_na[static_cast<std::size_t>(b_ids[idx])] != 0);
        if (na_here) { b_col_na[j] = 1; break; }
      }
    }

    // Gather (or alias) inputs into contiguous double buffers for dgemm.
    std::vector<double> a_buf, b_buf;
    const double* a_ptr;
    if (a_ids.empty()) {
      a_ptr = a_vals.data();
    } else {
      a_buf.resize(rows_a * cols_a);
      for (std::size_t k = 0; k < a_buf.size(); ++k) {
        a_buf[k] = val[static_cast<std::size_t>(a_ids[k])];
      }
      a_ptr = a_buf.data();
    }
    const double* b_ptr;
    if (b_ids.empty()) {
      b_ptr = b_vals.data();
    } else {
      b_buf.resize(cols_a * cols_b);
      for (std::size_t k = 0; k < b_buf.size(); ++k) {
        b_buf[k] = val[static_cast<std::size_t>(b_ids[k])];
      }
      b_ptr = b_buf.data();
    }

    std::vector<double>  out_vals(out_size, 0.0);
    std::vector<uint8_t> out_na(out_size, 0);
    const int    Mi = static_cast<int>(rows_a);
    const int    Ni = static_cast<int>(cols_b);
    const int    Ki = static_cast<int>(cols_a);
    const double alpha = 1.0, beta = 0.0;
    F77_CALL(dgemm)("N", "N", &Mi, &Ni, &Ki, &alpha,
                    a_ptr, &Mi, b_ptr, &Ki, &beta,
                    out_vals.data(), &Mi FCONE FCONE);

    // Apply NA mask: overwrite poisoned cells (BLAS may have produced finite
    // garbage if the source NA was tracked only by the is_na flag).
    for (std::size_t j = 0; j < cols_b; ++j) {
      for (std::size_t i = 0; i < rows_a; ++i) {
        if (a_row_na[i] || b_col_na[j]) {
          const std::size_t out_idx = j * rows_a + i;
          out_vals[out_idx] = std::numeric_limits<double>::quiet_NaN();
          out_na[out_idx]   = 1;
        }
      }
    }

    // Push the M*N output tape nodes. The first is the MatMul head (carries
    // the backward pass for the whole block); the rest are inert BlockSlot
    // markers. Both store block_idx in a[] (NOT a tape id — reverse() looks
    // it up in `blocks`).
    const int head_id = push_node(ROp::MatMul, block_idx, -1, out_vals[0], out_na[0] != 0);
    for (std::size_t k = 1; k < out_size; ++k) {
      push_node(ROp::BlockSlot, block_idx, -1, out_vals[k], out_na[k] != 0);
    }

    BlockOp blk;
    blk.kind         = BlockOp::Kind::MatMul;
    blk.rows_a       = rows_a;
    blk.cols_a       = cols_a;
    blk.cols_b       = cols_b;
    blk.a_ids        = std::move(a_ids);
    blk.a_vals       = std::move(a_vals);
    blk.b_ids        = std::move(b_ids);
    blk.b_vals       = std::move(b_vals);
    blk.out_tape_id_first = head_id;
    blocks.push_back(std::move(blk));

    return head_id;
  }

  void reverse(int out) {
    std::fill(adj.begin(), adj.end(), 0.0);
    adj[static_cast<std::size_t>(out)] = 1.0;
    for (std::size_t ii = size(); ii-- > 0;) {
      // MatMul head nodes must always fire — the head propagates adjoints
      // for ALL output cells of the block, not just its own cell.
      // Per-cell NA is handled inside the case by reading is_na per output id.
      if (is_na[ii] && op[ii] != ROp::MatMul) continue;
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
        case ROp::MatMul: {
          // ai is the index into `blocks`, NOT a tape id.
          const BlockOp& blk = blocks[static_cast<std::size_t>(ai)];
          const std::size_t M = blk.rows_a;
          const std::size_t K = blk.cols_a;
          const std::size_t N = blk.cols_b;
          const std::size_t out0 = static_cast<std::size_t>(blk.out_tape_id_first);

          // dC: copy output adjoints, zeroing NA cells (NA outputs have no
          // sensitivity to inputs).
          std::vector<double> dC(M * N);
          for (std::size_t k = 0; k < M * N; ++k) {
            dC[k] = is_na[out0 + k] ? 0.0 : adj[out0 + k];
          }

          // Materialize A and B as contiguous column-major double buffers.
          std::vector<double> a_buf, b_buf;
          const double* A_ptr;
          if (blk.a_ids.empty()) A_ptr = blk.a_vals.data();
          else {
            a_buf.resize(M * K);
            for (std::size_t k = 0; k < a_buf.size(); ++k)
              a_buf[k] = val[static_cast<std::size_t>(blk.a_ids[k])];
            A_ptr = a_buf.data();
          }
          const double* B_ptr;
          if (blk.b_ids.empty()) B_ptr = blk.b_vals.data();
          else {
            b_buf.resize(K * N);
            for (std::size_t k = 0; k < b_buf.size(); ++k)
              b_buf[k] = val[static_cast<std::size_t>(blk.b_ids[k])];
            B_ptr = b_buf.data();
          }

          const int    Mi = static_cast<int>(M);
          const int    Ni = static_cast<int>(N);
          const int    Ki = static_cast<int>(K);
          const double one = 1.0, zero = 0.0;

          // dA = dC * Bᵀ ;   shape (M x K) = (M x N) * (N x K stored as K x N transposed)
          if (!blk.a_ids.empty()) {
            std::vector<double> dA(M * K, 0.0);
            F77_CALL(dgemm)("N", "T", &Mi, &Ki, &Ni, &one,
                            dC.data(), &Mi, B_ptr, &Ki, &zero,
                            dA.data(), &Mi FCONE FCONE);
            for (std::size_t k = 0; k < M * K; ++k) {
              adj[static_cast<std::size_t>(blk.a_ids[k])] += dA[k];
            }
          }

          // dB = Aᵀ * dC ;   shape (K x N) = (K x M stored as M x K transposed) * (M x N)
          if (!blk.b_ids.empty()) {
            std::vector<double> dB(K * N, 0.0);
            F77_CALL(dgemm)("T", "N", &Ki, &Ni, &Mi, &one,
                            A_ptr, &Mi, dC.data(), &Mi, &zero,
                            dB.data(), &Ki FCONE FCONE);
            for (std::size_t k = 0; k < K * N; ++k) {
              adj[static_cast<std::size_t>(blk.b_ids[k])] += dB[k];
            }
          }
          break;
        }
        case ROp::BlockSlot:
          // No-op — the head handles all input adjoints in one pass.
          break;
        default:
          // Var / Const: no adjoint propagation
          break;
      }
    }
  }
};

inline thread_local ReverseTape TAPE_INTERN;

template <typename T> using IsCppArith = std::is_arithmetic<T>;
template <typename T> constexpr bool IsCppArithV = std::is_arithmetic_v<T>;

template <typename T> using Decayed = std::decay_t<T>;

template <typename T> using ReRef = std::remove_reference<T>;
template <typename T> constexpr bool IsRvalueV = std::is_rvalue_reference_v<T>;

template <typename T> using IsClass = std::is_class<T>;
template <typename T> constexpr bool IsClassV = std::is_class_v<T>;

template <typename T> struct It {
  T *p;
  T &operator*() const { return *p; }
  bool operator!=(const It &rhs) const { return p != rhs.p; }
  bool operator==(const It& rhs) const { return p == rhs.p; }
  It& operator++() { ++p; return *this; }
};

struct SI {
  std::size_t sz{0};

  SI() = default;
  explicit SI(std::size_t n) : sz(n) {
    ass<"Size has to be larger than 0!">(sz >= 1);
  }
  // signed integers (e.g., int, long long)
  template <std::signed_integral I>
  explicit SI(I n) {
    ass<"Size has to be larger than 0!">(n >= 1);
    using U = std::make_unsigned_t<I>;
    const U un = static_cast<U>(n);
    ass<"Requested size too large">(un <= static_cast<U>(std::numeric_limits<std::size_t>::max()));
    sz = static_cast<std::size_t>(un);
  }
  // floating point (floor, then validate)
  template <std::floating_point F>
  explicit SI(F n) {
    ass<"Size is not finite">(std::isfinite(n));
    const F f = std::floor(n);
    ass<"Size has to be larger than 0!">(f >= static_cast<F>(1));
    const long double ld = static_cast<long double>(f);
    ass<"Requested size too large">(ld <= static_cast<long double>(std::numeric_limits<std::size_t>::max()));
    sz = static_cast<std::size_t>(ld);
  }
};

inline std::size_t safe_modulo(std::size_t idx, std::size_t sz) {
  return sz ? (idx % sz) : 0;  // no UB if n==0
}
template <class F, class... Args> inline F forEachArg(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...); return f;
}
template<typename Dim>
auto&& dim_view(Dim& d) {
  if constexpr (requires { d.get(); }) {
    return d.get();
  } else {
    return d;
  }
}

inline std::size_t safe_index_from_double(double x) {
  ass<"invalid index argument">(std::isfinite(x));
  double t = std::trunc(x);
  ass<"Negative indices are not supported">(t >= 1);
  return static_cast<std::size_t>(t);
}

struct Dim {
  std::size_t rank = 1;
  std::size_t nrow = 1;
  std::size_t ncol = 0;
  std::unique_ptr<std::size_t[]> other_dims;

  Dim() = default;
  Dim(const Dim& other) { *this = other; }
  Dim(Dim&& other) noexcept { *this = std::move(other); }
  Dim& operator=(const Dim& other) {
    if (this == &other) return *this;
    rank = other.rank;
    nrow = other.nrow;
    ncol = (other.rank >= 2) ? other.ncol : 0;
    if (rank > 2) {
      other_dims = std::make_unique<std::size_t[]>(rank - 2);
      std::copy_n(other.other_dims.get(), rank - 2, other_dims.get());
    } else {
      other_dims.reset();
    }
    return *this;
  }

  Dim& operator=(Dim&& other) noexcept {
    if (this == &other) return *this;
    rank = other.rank;
    nrow = other.nrow;
    ncol = other.ncol;
    other_dims = std::move(other.other_dims);
    other.rank = 1;
    other.nrow = 1;
    other.ncol = 0;
    return *this;
  }
  std::size_t size() const noexcept { return rank; }

  std::size_t dim(std::size_t k) const noexcept {
    ass<"Invalid dim (out of bounds)">(k < rank);
    if (k == 0) return nrow;
    if (k == 1) return ncol;
    return other_dims[k - 2];
  }

  void set_rank1(std::size_t n) {
    rank = 1;
    nrow = n;
    ncol = 0;
    other_dims.reset();
  }
  void set_rank2(std::size_t r, std::size_t c) {
    rank = 2;
    nrow = r;
    ncol = c;
    other_dims.reset();
  }
  void set_rankN(std::size_t rk, std::size_t r, std::size_t c, const std::size_t* tail) {
    rank = rk;
    nrow = r;
    ncol = c;
    if (rank > 2) {
      other_dims = std::make_unique<std::size_t[]>(rank - 2);
      std::copy_n(tail, rank - 2, other_dims.get());
    } else {
      other_dims.reset();
    }
  }
};

// Traits (third dispatch layer)
// --------------------------------------------------------------------------------------------------
struct ComparisonTrait { using value_type = bool; };

struct LBufferTrait {};
struct RBufferTrait {};
struct SubsetViewTrait {};
struct ConstSubsetViewTrait {};
struct BorrowTrait {};
struct BinaryTrait {};
struct UnaryTrait {};

struct PlusTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l + r;
  }
};
struct MinusTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l - r;
  }
};
struct TimesTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l * r;
  }
};
struct DivideTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l / r;
  }
};
struct PowTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return pow(l, r);
  }
};
struct EqualTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l == r;
  }
};
struct SmallerTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l < r;
  }
};
struct SmallerEqualTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l <= r;
  }
};
struct LargerTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l > r;
  }
};
struct LargerEqualTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l >= r;
  }
};
struct UnEqualTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l != r;
  }
};
struct AndTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l && r;
  }
};
struct OrTrait {
  template <typename L, typename R>
  static inline auto f(const L& l, const R& r) {
    return l || r;
  }
};

struct SinusTrait {
  template <typename L> static inline auto f(L a) { return sin(a); }
};
struct ASinusTrait {
  template <typename L> static inline auto f(L a) { return asin(a); }
};
struct SinusHTrait {
  template <typename L> static inline auto f(L a) { return sinh(a); }
};
struct CosinusTrait {
  template <typename L> static inline auto f(L a) { return cos(a); }
};
struct ACosinusTrait {
  template <typename L> static inline auto f(L a) { return acos(a); }
};
struct CosinusHTrait {
  template <typename L> static inline auto f(L a) { return cosh(a); }
};
struct TangensTrait {
  template <typename L> static inline auto f(L a) { return tan(a); }
};
struct ATangensTrait {
  template <typename L> static inline auto f(L a) { return atan(a); }
};
struct TangensHTrait {
  template <typename L> static inline auto f(L a) { return tanh(a); }
};
struct ExpTrait {
  template <typename L> static inline auto f(L a) { return exp(a); }
};
struct LogTrait {
  template <typename L> static inline auto f(L a) { return log(a); }
};
struct SquareRootTrait {
  template <typename L> static inline auto f(L a) { return sqrt(a); }
};
struct MinusUnaryTrait {
  template <typename L> static inline auto f(L a) { return -a; }
};


// Inner data structs
// -----------------------------------------------------------------------------------------------------------
/*
Each inner data struct requires:
- copy constructor
- copy assignment
- move constructor
- move assignment
- size
- const auto& operator[]
- auto& operator[]
- begin and end
- Trait
- value_type

Classes Buffer, Borrow, UnaryOperation, BinaryOperation, SubsetView
*/
template <typename T, typename Trait = LBufferTrait> struct Buffer;
template <typename T, typename Trait = BorrowTrait> struct Borrow;
template <typename L, typename Trait = UnaryTrait> struct UnaryOperation;
template <typename L, typename R, typename Trait = BinaryTrait> struct BinaryOperation;
template<typename O, std::size_t N, typename Trait = SubsetViewTrait> struct SubsetView;
template<typename O, std::size_t N, typename Trait = ConstSubsetViewTrait> struct ConstSubsetView;

// Outer data structs
// -----------------------------------------------------------------------------------------------------------
template<typename T, typename R>
struct Array { static_assert(sizeof(R) == 0, "No generic Array<T,R> implementation"); };
template <typename T, typename Trait> struct Array<T, Buffer<T, Trait>>;
template <typename T, typename Trait> struct Array<T, Borrow<T, Trait>>;
template <typename T, typename I, typename Trait> struct Array<T, UnaryOperation<I, Trait>>;
template <typename T, typename L, typename R, typename Trait> struct Array<T, BinaryOperation<L, R, Trait>>;
template <typename T, typename O, std::size_t N, typename Trait> struct Array<T, SubsetView<O, N, Trait>>;
template <typename T, typename O, std::size_t N, typename Trait> struct Array<T, ConstSubsetView<O, N, Trait>>;

// Extract data type from outer data structs
// -----------------------------------------------------------------------------------------------------------
template <typename T> struct ExtractDataType;

template <typename T, typename R>
struct ExtractDataType<Array<T, R>> {
  using value_type = T;
};
template <typename T, typename R>
struct ExtractDataType<const Array<T, R>> {
  using value_type = T const;
};
template <typename T> using ExtractedTypeData = typename ExtractDataType<T>::value_type;

// Concepts
// -----------------------------------------------------------------------------------------------------------
// Determine type for literal bools, ints or doubles
// -----------------------------------------------------------------------------------------------------------
// Float
template <typename T> struct is_float_type : std::is_floating_point<T> {};
template <typename T> struct is_float_type_with_type : is_float_type<typename T::Type> {};
template <typename T, typename = void> struct is_float_dispatch : is_float_type<T> {};
template <typename T> struct is_float_dispatch<T, std::void_t<typename T::Type>> : is_float_type_with_type<T> {};
template <typename T> inline constexpr bool IsCppDouble = is_float_dispatch<T>::value;
// Integer
template <typename T> struct is_integer_type : std::is_integral<T> {};
template <typename T> struct is_integer_type_with_type : is_integer_type<typename T::Type> {};
template <typename T, typename = void> struct is_integer_dispatch : is_integer_type<T> {};
template <typename T> struct is_integer_dispatch<T, std::void_t<typename T::Type>> : is_integer_type_with_type<T> {};
template <typename T> inline constexpr bool IsCppInteger = is_integer_dispatch<T>::value;
// Bool (exactly bool, not all integrals)
template <typename T> struct is_bool_type : std::is_same<T, bool> {};
template <typename T> struct is_bool_type_with_type : is_bool_type<typename T::Type> {};
template <typename T, typename = void> struct is_bool_dispatch : is_bool_type<T> {};
template <typename T> struct is_bool_dispatch<T, std::void_t<typename T::Type>> : is_bool_type_with_type<T> {};
template <typename T> inline constexpr bool IsCppLogical = is_bool_dispatch<T>::value;

// Calculation & Inner data structures
// -----------------------------------------------------------------------------------------------------------
template <typename T>
concept IsUnary = requires {
typename ReRef<T>::type::Trait;
requires IS<typename ReRef<T>::type::Trait, SinusTrait> ||
IS<typename ReRef<T>::type::Trait, ASinusTrait> ||
IS<typename ReRef<T>::type::Trait, SinusHTrait> ||
IS<typename ReRef<T>::type::Trait, CosinusTrait> ||
IS<typename ReRef<T>::type::Trait, ACosinusTrait> ||
IS<typename ReRef<T>::type::Trait, CosinusHTrait> ||
IS<typename ReRef<T>::type::Trait, TangensTrait> ||
IS<typename ReRef<T>::type::Trait, ATangensTrait> ||
IS<typename ReRef<T>::type::Trait, TangensHTrait> ||
IS<typename ReRef<T>::type::Trait, ExpTrait> ||
IS<typename ReRef<T>::type::Trait, LogTrait> ||
IS<typename ReRef<T>::type::Trait, SquareRootTrait> ||
IS<typename ReRef<T>::type::Trait, MinusUnaryTrait>;
};
template <typename T>
concept IsBinary = requires {
typename ReRef<T>::type::Trait;
requires IS<typename ReRef<T>::type::Trait, PlusTrait> ||
IS<typename ReRef<T>::type::Trait, MinusTrait> ||
IS<typename ReRef<T>::type::Trait, TimesTrait> ||
IS<typename ReRef<T>::type::Trait, DivideTrait> ||
IS<typename ReRef<T>::type::Trait, PowTrait> ||
IS<typename ReRef<T>::type::Trait, EqualTrait> ||
IS<typename ReRef<T>::type::Trait, SmallerTrait> ||
IS<typename ReRef<T>::type::Trait, SmallerEqualTrait> ||
IS<typename ReRef<T>::type::Trait, LargerTrait> ||
IS<typename ReRef<T>::type::Trait, LargerEqualTrait> ||
IS<typename ReRef<T>::type::Trait, UnEqualTrait>;
};
template <typename T>
concept IsComparison = requires {
typename ReRef<T>::type::Trait;
requires IS<typename ReRef<T>::type::Trait, EqualTrait> ||
IS<typename ReRef<T>::type::Trait, SmallerTrait> ||
IS<typename ReRef<T>::type::Trait, SmallerEqualTrait> ||
IS<typename ReRef<T>::type::Trait, LargerTrait> ||
IS<typename ReRef<T>::type::Trait, LargerEqualTrait> ||
IS<typename ReRef<T>::type::Trait, UnEqualTrait> ||
IS<typename ReRef<T>::type::Trait, AndTrait> ||
IS<typename ReRef<T>::type::Trait, OrTrait>;
};
template <typename T>
concept IsComparisonTrait = requires(T t) { // required as in binary operation the trait is directly tested
typename T;
requires IS<T, EqualTrait> ||
IS<T, SmallerTrait> ||
IS<T, SmallerEqualTrait> ||
IS<T, LargerTrait> ||
IS<T, LargerEqualTrait> ||
IS<T, UnEqualTrait> ||
IS<T, AndTrait> ||
IS<T, OrTrait>;
};

// Mutable subset view
// -------------------------------------------------------------------
template <typename T>
concept IsSubsetView = requires {
typename ReRef<T>::type::TypeTrait;
requires IS<typename ReRef<T>::type::TypeTrait, SubsetViewTrait>;
};
// extract N from SubsetView
template<typename T>
struct subsetview_traits;

template<typename O, std::size_t N, typename Trait>
struct subsetview_traits<SubsetView<O, N, Trait>> {
  static constexpr std::size_t value = N;
};

// Const subset view
// -------------------------------------------------------------------
template <typename T>
concept IsConstSubsetView = requires {
typename ReRef<T>::type::TypeTrait;
requires IS<typename ReRef<T>::type::TypeTrait, ConstSubsetViewTrait>;
};
// extract N from ConstSubsetView
template<typename T>
struct const_subsetview_traits;

template <typename T>
concept IsLBuffer = requires {
typename ReRef<T>::type::Trait;
requires IS<typename ReRef<T>::type::Trait, LBufferTrait>;
};
template <typename T>
concept IsRBuffer = requires {
typename ReRef<T>::type::Trait;
requires IS<typename ReRef<T>::type::Trait, RBufferTrait>;
};
template <typename T>
concept IsBorrow = requires {
typename ReRef<T>::type::Trait;
requires IS<typename ReRef<T>::type::Trait, BorrowTrait>;
};

// Input class (outer data structures)
// -----------------------------------------------------------------------------------------------------------
// Array
template <typename T> struct is_any_array : std::false_type {};
template <typename T, typename R> struct is_any_array<Array<T, R>> : std::true_type {};
template <typename T> inline constexpr bool is_any_array_v = is_any_array<T>::value;
template <typename T> concept IsArray = is_any_array_v<T>;

template <typename T> struct is_array_l : std::false_type {};
template <typename T> struct is_array_l<Array<T, Buffer<T, LBufferTrait>>> : std::true_type {};
template <typename T> inline constexpr bool is_array_l_v = is_array_l<T>::value;
template <typename T> concept IsLBufferArray = is_array_l_v<T>;

template <typename T> struct is_array_r : std::false_type {};
template <typename T> struct is_array_r<Array<T, Buffer<T, RBufferTrait>>> : std::true_type {};
template <typename T> inline constexpr bool is_array_r_v = is_array_r<T>::value;
template <typename T> concept IsRArray = is_array_r_v<T>;

template <typename T> struct is_array_b : std::false_type {};
template <typename T> struct is_array_b<Array<T, Borrow<T, BorrowTrait>>> : std::true_type {};
template <typename T> inline constexpr bool is_array_b_v = is_array_b<T>::value;
template <typename T> concept IsBorrowArray = is_array_b_v<T>;

template <typename T> struct is_array_s : std::false_type {};
template <typename T, typename O, std::size_t N, typename Trait>
struct is_array_s<Array<T, SubsetView<O, N, Trait>>> : std::bool_constant<std::is_same_v<Trait, SubsetViewTrait>> {};
template <typename T> inline constexpr bool is_array_s_v = is_array_s<T>::value;
template <typename T> concept IsSubsetArray = is_array_s_v<T>;

template <typename T> struct is_array_const_s : std::false_type {};
template <typename T, typename O, std::size_t N, typename Trait>
struct is_array_const_s<Array<T, ConstSubsetView<O, N, Trait>>> : std::bool_constant<std::is_same_v<Trait, ConstSubsetViewTrait>> {};
template <typename T> inline constexpr bool is_array_const_s_v = is_array_const_s<T>::value;
template <typename T> concept IsConstSubsetArray = is_array_const_s_v<T>;

template <typename T> concept IsUnaryArray = IsArray<T> && IsUnary<typename T::DType>;
template <typename T> concept IsBinaryArray = IsArray<T> && IsBinary<typename T::DType>;
template <typename T> concept IsComparisonArray = IsArray<T> && IsComparison<typename T::DType>;

template <typename T>
concept IsOperationArray =
IsArray<T> && (
IsUnary<typename T::DType> ||
IsBinary<typename T::DType> ||
IsComparison<typename T::DType> ||
IsSubsetView<typename T::DType> ||
IsConstSubsetView<typename T::DType>
    );
template <typename T>
concept IsROrCalculationArray = requires(T t) {
typename T::DType;
requires IsOperationArray<T> || IsRArray<T>;
};

// Scalar types (First dispatch layer)
// --------------------------------------------------------------------------------------------------
struct Logical;
struct Integer;
struct Double;
struct Dual;
struct LogicalRef;
struct IntegerRef;
struct DoubleRef;
struct DualRef;
struct ReverseDouble;

struct Logical {
  bool val;
  bool is_na{false};
  inline operator bool() const { return val; }
  inline Logical();
  inline Logical(bool v);
  inline Logical(Integer v);
  inline Logical(Double v);
  inline Logical(Dual v);
  inline Logical(ReverseDouble v);
  inline Integer operator+(const Logical&) const;
  inline Integer operator-(const Logical&) const;
  inline Integer operator*(const Logical&) const;
  inline Double operator/(const Logical&) const;
  inline Double pow(const Logical&) const;
  inline Logical operator==(const Logical&) const;
  inline Logical operator<(const Logical&) const;
  inline Logical operator<=(const Logical&) const;
  inline Logical operator>(const Logical&) const;
  inline Logical operator>=(const Logical&) const;
  inline Logical operator!=(const Logical&) const;
  inline Logical operator&&(const Logical&) const;
  inline Logical operator||(const Logical&) const;
  inline Logical operator&(const Logical&) const;
  inline Logical operator|(const Logical&) const;
  inline Double sin() const;
  inline Double asin() const;
  inline Double sinh() const;
  inline Double cos() const;
  inline Double acos() const;
  inline Double cosh() const;
  inline Double tan() const;
  inline Double atan() const;
  inline Double tanh() const;
  inline Double exp() const;
  inline Double log() const;
  inline Double log10() const;
  inline Double sqrt() const;
  inline Integer operator-() const;
  inline friend std::ostream& operator<<(std::ostream&, const Logical&);
  inline static Logical NA() { Logical x; x.is_na = true; return x; }
  inline bool isNA() const noexcept {
    return is_na;
  }
  template<typename T> requires IsArray<Decayed<T>> inline Logical& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    const auto arr_val = get_scalar_val(arr.get(0));
    if constexpr(IS<inner, Logical>) {
      val = arr_val.val;
      is_na = arr_val.is_na;
    } else if constexpr(IS<inner, ReverseDouble>) {
      val = static_cast<bool>(arr_val.get_val_from_tape());
      is_na = arr_val.is_na;
    } else {
      val = static_cast<bool>(arr_val.val);
      is_na = arr_val.is_na;
    }
    return *this;
  }
};

struct Integer {
  int val;
  bool is_na{false};
  inline Integer();
  inline Integer(int v);
  inline Integer(Logical v);
  inline Integer(Double v);
  inline Integer(Dual v);
  inline Integer(ReverseDouble v);
  inline Integer operator+(const Integer&) const;
  inline Integer& operator+=(const Integer& r);
  inline Integer& operator-=(const Integer& r);
  inline Integer& operator*=(const Integer& r);
  inline Integer operator-(const Integer&) const;
  inline Integer operator*(const Integer&) const;
  inline Double operator/(const Integer&) const;
  inline Double pow(const Integer&) const;
  inline Logical operator==(const Integer&) const;
  inline Logical operator<(const Integer&) const;
  inline Logical operator<=(const Integer&) const;
  inline Logical operator>(const Integer&) const;
  inline Logical operator>=(const Integer&) const;
  inline Logical operator!=(const Integer&) const;
  inline Logical operator&&(const Integer&) const;
  inline Logical operator||(const Integer&) const;
  inline Logical operator&(const Integer&) const;
  inline Logical operator|(const Integer&) const;
  inline Double sin() const;
  inline Double asin() const;
  inline Double sinh() const;
  inline Double cos() const;
  inline Double acos() const;
  inline Double cosh() const;
  inline Double tan() const;
  inline Double atan() const;
  inline Double tanh() const;
  inline Double exp() const;
  inline Double log() const;
  inline Double log10() const;
  inline Double sqrt() const;
  inline Integer operator-() const;
  inline friend std::ostream& operator<<(std::ostream&, const Integer&);
  inline static Integer NA() { Integer x; x.is_na = true; return x; }
  inline bool isNA() const noexcept {
    return is_na;
  }
  template<typename T> requires IsArray<Decayed<T>> inline Integer& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    const auto arr_val = get_scalar_val(arr.get(0));
    if constexpr(IS<inner, Integer>) {
      val = arr_val.val;
      is_na = arr_val.is_na;
    } else if constexpr(IS<inner, ReverseDouble>) {
      val = static_cast<int>(arr_val.get_val_from_tape());
      is_na = arr_val.is_na;
    } else {
      val = static_cast<int>(arr_val.val);
      is_na = arr_val.is_na;
    }
    return *this;
  }
};

struct Double {
  double val;
  bool is_na;
  inline Double();
  inline Double(double v);
  inline Double(Logical v);
  inline Double(Integer v);
  inline Double(Dual v);
  inline Double(ReverseDouble v);
  inline Double operator+(const Double&) const;
  inline Double& operator+=(const Double& r);
  inline Double& operator-=(const Double& r);
  inline Double& operator*=(const Double& r);
  inline Double& operator/=(const Double& r);
  inline Double operator-(const Double&) const;
  inline Double operator*(const Double&) const;
  inline Double operator/(const Double&) const;
  inline Double pow(const Double&) const;
  inline Logical operator==(const Double&) const;
  inline Logical operator<(const Double&) const;
  inline Logical operator<=(const Double&) const;
  inline Logical operator>(const Double&) const;
  inline Logical operator>=(const Double&) const;
  inline Logical operator!=(const Double&) const;
  inline Logical operator&&(const Double&) const;
  inline Logical operator||(const Double&) const;
  inline Logical operator&(const Double&) const;
  inline Logical operator|(const Double&) const;
  inline Double sin() const;
  inline Double asin() const;
  inline Double sinh() const;
  inline Double cos() const;
  inline Double acos() const;
  inline Double cosh() const;
  inline Double tan() const;
  inline Double atan() const;
  inline Double tanh() const;
  inline Double exp() const;
  inline Double log() const;
  inline Double log10() const;
  inline Double sqrt() const;
  inline Double operator-() const;
  inline friend std::ostream& operator<<(std::ostream&, const Double&);
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
  inline bool isNA() const noexcept {
    return is_na;
  }
  inline bool isNaN() const noexcept {
    return !is_na && std::isnan(val);
  }
  inline bool isFinite() const noexcept {
    return !is_na && std::isfinite(val);
  }
  inline bool isInfinite() const noexcept {
    return !is_na && !isNaN() && !std::isfinite(val);
  }
  template<typename T> requires IsArray<Decayed<T>> inline Double& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    const auto arr_val = get_scalar_val(arr.get(0));
    if constexpr(IS<inner, Double>) {
      val = arr_val.val;
      is_na = arr_val.is_na;
    } else if constexpr(IS<inner, ReverseDouble>) {
      val = arr_val.get_val_from_tape();
      is_na = arr_val.is_na;
    } else {
      val = static_cast<double>(arr_val.val);
      is_na = arr_val.is_na;
    }
    return *this;
  }
};

struct Dual {
  double val, dot;
  bool is_na;
  bool is_na_dot;
  inline Dual();
  inline Dual(double v, double d);
  inline Dual(Logical v);
  inline Dual(Integer v);
  inline Dual(Double v);
  inline Dual(ReverseDouble v);
  // Does not require a ctr for Variable. Forward and reverse mode AD cannot be mixed!
  template<typename T> requires std::is_arithmetic_v<T>
  Dual(T v);
  inline Dual operator+(const Dual&) const;
  inline Dual& operator+=(const Dual& r);
  inline Dual& operator-=(const Dual& r);
  inline Dual& operator*=(const Dual& r);
  inline Dual& operator/=(const Dual& r);
  inline Dual operator-(const Dual&) const;
  inline Dual operator*(const Dual&) const;
  inline Dual operator/(const Dual&) const;
  inline Dual pow(const Dual&) const;
  inline Logical operator==(const Dual&) const;
  inline Logical operator<(const Dual&) const;
  inline Logical operator<=(const Dual&) const;
  inline Logical operator>(const Dual&) const;
  inline Logical operator>=(const Dual&) const;
  inline Logical operator!=(const Dual&) const;
  inline Logical operator&&(const Dual&) const;
  inline Logical operator||(const Dual&) const;
  inline Logical operator&(const Dual&) const;
  inline Logical operator|(const Dual&) const;
  inline Dual sin() const;
  inline Dual asin() const;
  inline Dual sinh() const;
  inline Dual cos() const;
  inline Dual acos() const;
  inline Dual cosh() const;
  inline Dual tan() const;
  inline Dual atan() const;
  inline Dual tanh() const;
  inline Dual exp() const;
  inline Dual log() const;
  inline Dual log10() const;
  inline Dual sqrt() const;
  inline Dual operator-() const;
  inline friend std::ostream& operator<<(std::ostream&, const Dual&);
  inline static Dual NA() {
    Dual x(std::numeric_limits<double>::quiet_NaN(),
           std::numeric_limits<double>::quiet_NaN());
    x.is_na = true;
    x.is_na_dot = true;
    return x;
  }
  inline static Dual NaN() {
    return Dual (std::numeric_limits<double>::quiet_NaN(),
                 std::numeric_limits<double>::quiet_NaN());
  }
  inline static Dual Inf() {
    return Dual(std::numeric_limits<double>::infinity(),
                std::numeric_limits<double>::infinity());
  }
  inline bool isNA() const noexcept {
    return is_na;
  }
  inline bool isNADot() const noexcept {
    return is_na_dot;
  }
  inline bool isNaN() const noexcept {
    return !is_na && std::isnan(val);
  }
  inline bool isNaNDot() const noexcept {
    return !is_na_dot && std::isnan(dot);
  }
  inline bool isFinite() const noexcept {
    return !is_na && std::isfinite(val);
  }
  inline bool isInfinite() const noexcept {
    return !is_na && !isNaN() && !std::isfinite(val);
  }
  inline bool isFiniteDot() const noexcept {
    return !is_na_dot && std::isfinite(dot);
  }
  inline bool isInfiniteDot() const noexcept {
    return !is_na_dot && !isNaNDot() && !std::isfinite(dot);
  }
  template<typename T> requires IsArray<Decayed<T>> inline Dual& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    const auto arr_val = get_scalar_val(arr.get(0));
    if constexpr(IS<inner, Dual>) {
      val = arr_val.val;
      is_na = arr_val.is_na;
      dot = arr_val.dot;
      is_na_dot = arr_val.is_na_dot;
    } else if constexpr(IS<inner, ReverseDouble>) {
      val = arr_val.get_val_from_tape();
      is_na = arr_val.is_na;
      dot = 0;
      is_na_dot = false;
    } else {
      val = static_cast<double>(arr_val.val);
      is_na = arr_val.is_na;
      dot = 0;
      is_na_dot = false;
    }
    return *this;
  }
};

// Reverse-mode AD scalar. 8-byte handle into etr::TAPE_INTERN; the actual
// value/adjoint live on the tape (val[id] / adj[id]).
struct ReverseDouble {
  int  id{-1};
  bool is_na{false};

  inline ReverseDouble() {
    id = TAPE_INTERN.push_const(0.0, false);
  }
  inline ReverseDouble(double v) {
    id = TAPE_INTERN.push_const(v, false);
  }
  // Tag-based ctor for wrapping an existing tape id without pushing a new
  // node (used by mat_mul and other block ops to materialize result handles).
  struct from_tape_id_t {};
  inline ReverseDouble(from_tape_id_t, int id_, bool is_na_) noexcept
  : id(id_), is_na(is_na_) {}
  inline ReverseDouble(int v)  : ReverseDouble(static_cast<double>(v)) {}
  inline ReverseDouble(bool v) : ReverseDouble(v ? 1.0 : 0.0) {}

  inline ReverseDouble(Logical v) { 
    id = TAPE_INTERN.push_const(static_cast<double>(v.val), v.is_na);
    is_na = v.is_na;
  }
  inline ReverseDouble(Integer v) {
    id = TAPE_INTERN.push_const(static_cast<double>(v.val), v.is_na);
    is_na = v.is_na;
  }
  inline ReverseDouble(Double v) {
    id = TAPE_INTERN.push_const(static_cast<double>(v.val), v.is_na);
    is_na = v.is_na;
  }
  inline ReverseDouble(Dual v) {
    id = TAPE_INTERN.push_const(v.val, v.is_na);
    is_na = v.is_na;
  }

  // An assignment `x = y` between two ReverseDouble values rebinds x.id to
  // y.id (same R let-binding semantics). A subsequent `x = y + z` pushes a
  // fresh node normally.
  ReverseDouble(const ReverseDouble&)            = default;
  ReverseDouble(ReverseDouble&&) noexcept        = default;
  ReverseDouble& operator=(const ReverseDouble&) = default;
  ReverseDouble& operator=(ReverseDouble&&)      = default;

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

  inline double get_val_from_tape()  const { return TAPE_INTERN.val [static_cast<std::size_t>(id)]; }
  inline double get_grad_from_tape() const { return TAPE_INTERN.adj [static_cast<std::size_t>(id)]; }

  inline bool isNA()       const noexcept { return is_na; }
  inline bool isNaN()      const noexcept { return !is_na && std::isnan(get_val_from_tape()); }
  inline bool isFinite()   const noexcept { return !is_na && std::isfinite(get_val_from_tape()); }
  inline bool isInfinite() const noexcept { return !is_na && !isNaN() && !std::isfinite(get_val_from_tape()); }

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

  inline Logical operator==(const ReverseDouble& r) const {
    if (is_na || r.is_na) return Logical::NA();
    return Logical{get_val_from_tape() == r.get_val_from_tape()};
  }
  inline Logical operator!=(const ReverseDouble& r) const {
    if (is_na || r.is_na) return Logical::NA();
    return Logical{get_val_from_tape() != r.get_val_from_tape()};
  }
  inline Logical operator< (const ReverseDouble& r) const {
    if (is_na || r.is_na) return Logical::NA();
    return Logical{get_val_from_tape() <  r.get_val_from_tape()};
  }
  inline Logical operator<=(const ReverseDouble& r) const {
    if (is_na || r.is_na) return Logical::NA();
    return Logical{get_val_from_tape() <= r.get_val_from_tape()};
  }
  inline Logical operator> (const ReverseDouble& r) const {
    if (is_na || r.is_na) return Logical::NA();
    return Logical{get_val_from_tape() >  r.get_val_from_tape()};
  }
  inline Logical operator>=(const ReverseDouble& r) const {
    if (is_na || r.is_na) return Logical::NA();
    return Logical{get_val_from_tape() >= r.get_val_from_tape()};
  }

  inline Logical operator&&(const ReverseDouble& r) const {
    if (is_na || r.is_na) return Logical::NA();
    return Logical{get_val_from_tape() != 0.0 && r.get_val_from_tape() != 0.0};
  }
  inline Logical operator||(const ReverseDouble& r) const {
    if (is_na || r.is_na) return Logical::NA();
    return Logical{get_val_from_tape() != 0.0 || r.get_val_from_tape() != 0.0};
  }
  inline Logical operator&(const ReverseDouble& r) const {
    if (is_na || r.is_na) return Logical::NA();
    return Logical{get_val_from_tape() != 0.0 && r.get_val_from_tape() != 0.0};
  }
  inline Logical operator|(const ReverseDouble& r) const {
    if (is_na || r.is_na) return Logical::NA();
    return Logical{get_val_from_tape() != 0.0 || r.get_val_from_tape() != 0.0};
  }

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

  inline friend std::ostream& operator<<(std::ostream& os, const ReverseDouble& x) {
    if (x.isNA()) return os << "NA";
    const double v = x.get_val_from_tape();
    if (std::isnan(v)) return os << "NaN";
    if (!std::isfinite(v)) return os << (v > 0 ? "Inf" : "-Inf");
    return os << v;
  }
  template<typename T> requires IsArray<Decayed<T>> inline ReverseDouble& operator=(const T& arr) {
    using inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    const auto arr_val = get_scalar_val(arr.get(0));
    if constexpr (IS<inner, ReverseDouble>) {
      // Rebind to the existing tape node — same semantics as the
      // defaulted scalar operator=
      id    = arr_val.id;
      is_na = arr_val.is_na;
    } else {
      // Push a fresh constant for the converted value, then rebind.
      ReverseDouble tmp(arr_val);
      id    = tmp.id;
      is_na = tmp.is_na;
    }
    return *this;
  }
};

inline Logical::Logical() : val(false), is_na(false) {}
inline Logical::Logical(bool v) : val(v), is_na(false) {}
inline Logical::Logical(Integer v) : val(static_cast<bool>(v.val)), is_na(v.is_na) {}
inline Logical::Logical(Double v)  : val(static_cast<bool>(v.val)), is_na(v.is_na) {}
inline Logical::Logical(Dual v)    : val(static_cast<bool>(v.val)), is_na(v.is_na) {}
inline Logical::Logical(ReverseDouble v) : val(static_cast<bool>(v.get_val_from_tape())), is_na(v.is_na) {}

inline Integer::Integer() : val(0), is_na(false) {}
inline Integer::Integer(int v) : val(v), is_na(false) {}
inline Integer::Integer(Logical v) : val(static_cast<int>(v.val)), is_na(v.is_na) {}
inline Integer::Integer(Double v)  : val(static_cast<int>(v.val)), is_na(v.is_na) {}
inline Integer::Integer(Dual v)    : val(static_cast<int>(v.val)), is_na(v.is_na) {}
inline Integer::Integer(ReverseDouble v) : val(static_cast<int>(v.get_val_from_tape())), is_na(v.is_na) {}

inline Double::Double() : val(0.0), is_na(false) {}
inline Double::Double(double v) : val(v), is_na(false) {}
inline Double::Double(Logical v) : val(static_cast<double>(v.val)), is_na(v.is_na) {}
inline Double::Double(Integer v) : val(static_cast<double>(v.val)), is_na(v.is_na) {}
inline Double::Double(Dual v)    : val(v.val), is_na(v.is_na) {}
inline Double::Double(ReverseDouble v) : val(v.get_val_from_tape()), is_na(v.is_na) {}

inline Dual::Dual() : val(0.0), dot(0.0), is_na(false), is_na_dot(false) {}
inline Dual::Dual(double v, double d) : val(v), dot(d), is_na(false), is_na_dot(false) {}
inline Dual::Dual(Logical v) : val(static_cast<double>(v.val)), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
inline Dual::Dual(Integer v) : val(static_cast<double>(v.val)), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
inline Dual::Dual(Double v)    : val(v.val), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
inline Dual::Dual(ReverseDouble v) : val(v.get_val_from_tape()), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
template<typename T> requires std::is_arithmetic_v<T>
inline Dual::Dual(T v)    : val(static_cast<double>(v)), dot(0.0), is_na(std::isnan(v)), is_na_dot(false) {}

inline std::ostream& operator<<(std::ostream& os, const Logical& x) {
  if (x.isNA()) return os << "NA";
  if (x.val) return os << "TRUE"; else return os << "FALSE";
}
inline std::ostream& operator<<(std::ostream& os, const Integer& x) {
  if (x.isNA()) return os << "NA";
  return os << x.val;
}
inline std::ostream& operator<<(std::ostream& os, const Double& x) {
  if (x.isNA()) return os << "NA";
  if (x.isNaN()) return os << "NaN";
  if (!x.isFinite())   return os << (x.val > 0 ? "Inf" : "-Inf");
  return os << x.val;
}
inline std::ostream& operator<<(std::ostream& os, const Dual& x) {
  auto print_scalar = [&](double v, bool is_na) -> std::ostream& {
    if (is_na) return os << "NA";
    if (std::isnan(v)) return os << "NaN";
    if (!std::isfinite(v)) return os << (v > 0 ? "Inf" : "-Inf");
    return os << v;
  };
  os << "(";
  print_scalar(x.val, x.isNA());
  os << ", ";
  print_scalar(x.dot, x.isNADot());
  os << ")";
  return os;
}

inline Integer Logical::operator+(const Logical& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val + other.val);
}
inline Integer Integer::operator+(const Integer& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val + other.val);
}
inline Double Double::operator+(const Double& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(val + other.val);
}
inline Dual Dual::operator+(const Dual& other) const {
  if (is_na || other.is_na) return Dual::NA();
  const double v = val + other.val;
  if (is_na_dot || other.is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, dot + other.dot);
}

inline Integer& Integer::operator+=(const Integer& r) {
  if (is_na || r.is_na) return *this = Integer::NA();
  val += r.val;
  return *this;
}
inline Integer& Integer::operator-=(const Integer& r) {
  if (is_na || r.is_na) return *this = Integer::NA();
  val -= r.val;
  return *this;
}
inline Integer& Integer::operator*=(const Integer& r) {
  if (is_na || r.is_na) return *this = Integer::NA();
  val *= r.val;
  return *this;
}
inline Double& Double::operator+=(const Double& r) {
  if (is_na || r.is_na) return *this = Double::NA();
  val += r.val;
  return *this;
}
inline Double& Double::operator-=(const Double& r) {
  if (is_na || r.is_na) return *this = Double::NA();
  val -= r.val;
  return *this;
}
inline Double& Double::operator*=(const Double& r) {
  if (is_na || r.is_na) return *this = Double::NA();
  val *= r.val;
  return *this;
}
inline Double& Double::operator/=(const Double& r) {
  if (is_na || r.is_na) return *this = Double::NA();
  val /= r.val;
  return *this;
}
inline Dual& Dual::operator+=(const Dual& r) {
  if (is_na || r.is_na) return *this = Dual::NA();
  val += r.val;
  if (is_na_dot || r.is_na_dot) return *this = Dual::NA();
  dot += r.dot;
  return *this;
}
inline Dual& Dual::operator-=(const Dual& r) {
  if (is_na || r.is_na) return *this = Dual::NA();
  val -= r.val;
  if (is_na_dot || r.is_na_dot) return *this = Dual::NA();
  dot -= r.dot;
  return *this;
}
inline Dual& Dual::operator*=(const Dual& r) {
  if (is_na || r.is_na) return *this = Dual::NA();
  val *= r.val;
  if (is_na_dot || r.is_na_dot) return *this = Dual::NA();
  dot *= r.dot;
  return *this;
}
inline Dual& Dual::operator/=(const Dual& r) {
  if (is_na || r.is_na) return *this = Dual::NA();
  val /= r.val;
  if (is_na_dot || r.is_na_dot) return *this = Dual::NA();
  dot /= r.dot;
  return *this;
}

inline Integer Logical::operator-(const Logical& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val - other.val);
}
inline Integer Integer::operator-(const Integer& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val - other.val);
}
inline Double Double::operator-(const Double& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(val - other.val);
}
inline Dual Dual::operator-(const Dual& other) const {
  if (is_na || other.is_na) return Dual::NA();
  const double v = val - other.val;
  if (is_na_dot || other.is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, dot - other.dot);
}

inline Integer Logical::operator*(const Logical& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val * other.val);
}
inline Integer Integer::operator*(const Integer& other) const {
  if (is_na || other.is_na) return Integer::NA();
  return Integer(val * other.val);
}
inline Double Double::operator*(const Double& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(val * other.val);
}
inline Dual Dual::operator*(const Dual& other) const {
  if (is_na || other.is_na) return Dual::NA();
  const double v = val * other.val;
  if (is_na_dot || other.is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, dot * other.val + val * other.dot);
}

inline Double Logical::operator/(const Logical& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(static_cast<double>(val) / static_cast<double>(other.val));
}
inline Double Integer::operator/(const Integer& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(static_cast<double>(val) / static_cast<double>(other.val));
}
inline Double Double::operator/(const Double& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(val / other.val);
}
inline Dual Dual::operator/(const Dual& other) const {
  if (is_na || other.is_na) return Dual::NA();
  const double v = val / other.val;
  if (is_na_dot || other.is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, (dot * other.val - val * other.dot) / (other.val * other.val));
}

inline Double Logical::pow(const Logical& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(std::pow(static_cast<double>(val), static_cast<double>(other.val)));
}
inline Double Integer::pow(const Integer& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(std::pow(static_cast<double>(val), static_cast<double>(other.val)));
}
inline Double Double::pow(const Double& other) const {
  if (is_na || other.is_na) return Double::NA();
  return Double(std::pow(val, other.val));
}
inline Dual Dual::pow(const Dual& other) const {
  if (is_na || other.is_na) return Dual::NA();
  const double f = std::pow(val, other.val);
  if (!std::isfinite(f) || is_na_dot || other.is_na_dot) return Dual(f, std::numeric_limits<double>::quiet_NaN());
  const double d = f * (other.dot * std::log(val) + other.val * dot / val);
  return Dual(f, d);
}

inline Logical Logical::operator==(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val == other.val);
}
inline Logical Integer::operator==(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val == other.val);
}
inline Logical Double::operator==(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val == other.val);
}
inline Logical Dual::operator==(const Dual& other) const {
  if ( is_na || other.is_na) return Logical::NA();
  return Logical(val == other.val);
}

inline Logical Logical::operator<(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val < other.val);
}
inline Logical Integer::operator<(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val < other.val);
}
inline Logical Double::operator<(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val < other.val);
}
inline Logical Dual::operator<(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val < other.val);
}

inline Logical Logical::operator<=(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val <= other.val);
}
inline Logical Integer::operator<=(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val <= other.val);
}
inline Logical Double::operator<=(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val <= other.val);
}
inline Logical Dual::operator<=(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val <= other.val);
}

inline Logical Logical::operator>(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val > other.val);
}
inline Logical Integer::operator>(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val > other.val);
}
inline Logical Double::operator>(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val > other.val);
}
inline Logical Dual::operator>(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val > other.val);
}

inline Logical Logical::operator>=(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val >= other.val);
}
inline Logical Integer::operator>=(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val >= other.val);
}
inline Logical Double::operator>=(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val >= other.val);
}
inline Logical Dual::operator>=(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val >= other.val);
}

inline Logical Logical::operator!=(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val != other.val);
}
inline Logical Integer::operator!=(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val != other.val);
}
inline Logical Double::operator!=(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val != other.val);
}
inline Logical Dual::operator!=(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val != other.val);
}

inline Logical Logical::operator&&(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}
inline Logical Integer::operator&&(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}
inline Logical Double::operator&&(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}
inline Logical Dual::operator&&(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}

inline Logical Logical::operator||(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}
inline Logical Integer::operator||(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}
inline Logical Double::operator||(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}
inline Logical Dual::operator||(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}

inline Logical Logical::operator&(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}
inline Logical Integer::operator&(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}
inline Logical Double::operator&(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}
inline Logical Dual::operator&(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val && other.val);
}

inline Logical Logical::operator|(const Logical& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}
inline Logical Integer::operator|(const Integer& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}
inline Logical Double::operator|(const Double& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}
inline Logical Dual::operator|(const Dual& other) const {
  if (is_na || other.is_na) return Logical::NA();
  return Logical(val || other.val);
}

inline Double Logical::sin() const {
  if (is_na) return Double::NA();
  return Double(std::sin(static_cast<double>(val)));
}
inline Double Integer::sin() const {
  if (is_na) return Double::NA();
  return Double(std::sin(static_cast<double>(val)));
}
inline Double Double::sin() const {
  if (is_na) return Double::NA();
  return Double(std::sin(val));
}
inline Dual Dual::sin() const {
  if (is_na) return Dual::NA();
  const double v = std::sin(val);
  if (is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, std::cos(val));
}

inline Double Logical::sinh() const {
  if (is_na) return Double::NA();
  return Double(std::sinh(static_cast<double>(val)));
}
inline Double Integer::sinh() const {
  if (is_na) return Double::NA();
  return Double(std::sinh(static_cast<double>(val)));
}
inline Double Double::sinh() const {
  if (is_na) return Double::NA();
  return Double(std::sinh(val));
}
inline Dual Dual::sinh() const {
  if (is_na) return Dual::NA();
  const double v = std::sinh(val);
  if (is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, std::cosh(val));
}

inline Double Logical::asin() const {
  if (is_na) return Double::NA();
  return Double(std::asin(static_cast<double>(val)));
}
inline Double Integer::asin() const {
  if (is_na) return Double::NA();
  return Double(std::asin(static_cast<double>(val)));
}
inline Double Double::asin() const {
  if (is_na) return Double::NA();
  return Double(std::asin(val));
}
inline Dual Dual::asin() const {
  if (is_na) return Dual::NA();
  const double v = std::asin(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 1.0 / std::sqrt(1.0 - val * val));
}

inline Double Logical::cos() const {
  if (is_na) return Double::NA();
  return Double(std::cos(static_cast<double>(val)));
}
inline Double Integer::cos() const {
  if (is_na) return Double::NA();
  return Double(std::cos(static_cast<double>(val)));
}
inline Double Double::cos() const {
  if (is_na) return Double::NA();
  return Double(std::cos(val));
}
inline Dual Dual::cos() const {
  if (is_na) return Dual::NA();
  const double v = std::cos(val);
  if (is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, -std::sin(val));
}

inline Double Logical::cosh() const {
  if (is_na) return Double::NA();
  return Double(std::cosh(static_cast<double>(val)));
}
inline Double Integer::cosh() const {
  if (is_na) return Double::NA();
  return Double(std::cosh(static_cast<double>(val)));
}
inline Double Double::cosh() const {
  if (is_na) return Double::NA();
  return Double(std::cosh(val));
}
inline Dual Dual::cosh() const {
  if (is_na) return Dual::NA();
  const double v = std::cosh(val);
  if (is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, std::sinh(val));
}

inline Double Logical::acos() const {
  if (is_na) return Double::NA();
  return Double(std::acos(static_cast<double>(val)));
}
inline Double Integer::acos() const {
  if (is_na) return Double::NA();
  return Double(std::acos(static_cast<double>(val)));
}
inline Double Double::acos() const {
  if (is_na) return Double::NA();
  return Double(std::acos(val));
}
inline Dual Dual::acos() const {
  if (is_na) return Dual::NA();
  const double v = std::acos(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, -1.0 / std::sqrt(1.0 - val * val));
}

inline Double Logical::tan() const {
  if (is_na) return Double::NA();
  return Double(std::tan(static_cast<double>(val)));
}
inline Double Integer::tan() const {
  if (is_na) return Double::NA();
  return Double(std::tan(static_cast<double>(val)));
}
inline Double Double::tan() const {
  if (is_na) return Double::NA();
  return Double(std::tan(val));
}
inline Dual Dual::tan() const {
  if (is_na) return Dual::NA();
  const double v = std::tan(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 1.0 / (std::cos(val) * std::cos(val)));
}

inline Double Logical::tanh() const {
  if (is_na) return Double::NA();
  return Double(std::tanh(static_cast<double>(val)));
}
inline Double Integer::tanh() const {
  if (is_na) return Double::NA();
  return Double(std::tanh(static_cast<double>(val)));
}
inline Double Double::tanh() const {
  if (is_na) return Double::NA();
  return Double(std::tanh(val));
}
inline Dual Dual::tanh() const {
  if (is_na) return Dual::NA();
  const double t = std::tanh(val);
  if (is_na_dot) return Dual(t, std::numeric_limits<double>::quiet_NaN());
  return Dual(t, 1.0 - t * t);
}

inline Double Logical::atan() const {
  if (is_na) return Double::NA();
  return Double(std::atan(static_cast<double>(val)));
}
inline Double Integer::atan() const {
  if (is_na) return Double::NA();
  return Double(std::atan(static_cast<double>(val)));
}
inline Double Double::atan() const {
  if (is_na) return Double::NA();
  return Double(std::atan(val));
}
inline Dual Dual::atan() const {
  if (is_na) return Dual::NA();
  const double v = std::atan(val);
  if (is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 1.0 / (1.0 + val * val));
}

inline Double Logical::exp() const {
  if (is_na) return Double::NA();
  return Double(std::exp(static_cast<double>(val)));
}
inline Double Integer::exp() const {
  if (is_na) return Double::NA();
  return Double(std::exp(static_cast<double>(val)));
}
inline Double Double::exp() const {
  if (is_na) return Double::NA();
  return Double(std::exp(val));
}
inline Dual Dual::exp() const {
  if (is_na) return Dual::NA();
  const double v = std::exp(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, v);
}

inline Double Logical::log() const {
  if (is_na) return Double::NA();
  return Double(std::log(static_cast<double>(val)));
}
inline Double Integer::log() const {
  if (is_na) return Double::NA();
  return Double(std::log(static_cast<double>(val)));
}
inline Double Double::log() const {
  if (is_na) return Double::NA();
  return Double(std::log(val));
}
inline Dual Dual::log() const {
  if (is_na) return Dual::NA();
  const double v = std::log(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 1.0 / val);
}

inline Double Logical::log10() const {
  if (is_na) return Double::NA();
  return Double(std::log10(static_cast<double>(val)));
}
inline Double Integer::log10() const {
  if (is_na) return Double::NA();
  return Double(std::log10(static_cast<double>(val)));
}
inline Double Double::log10() const {
  if (is_na) return Double::NA();
  return Double(std::log10(val));
}
inline Dual Dual::log10() const {
  if (is_na) return Dual::NA();
  const double v = std::log10(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 1.0 / val);
}

inline Double Logical::sqrt() const {
  if (is_na) return Double::NA();
  return Double(std::sqrt(static_cast<double>(val)));
}
inline Double Integer::sqrt() const {
  if (is_na) return Double::NA();
  return Double(std::sqrt(static_cast<double>(val)));
}
inline Double Double::sqrt() const {
  if (is_na) return Double::NA();
  return Double(std::sqrt(val));
}
inline Dual Dual::sqrt() const {
  if (is_na) return Dual::NA();
  const double v = std::sqrt(val);
  if (!std::isfinite(v) || is_na_dot) return Dual(v, std::numeric_limits<double>::quiet_NaN());
  return Dual(v, 0.5 / v);
}

inline Integer Logical::operator-() const {
  if (is_na) return Integer::NA();
  return Integer(-static_cast<int>(val));
}
inline Integer Integer::operator-() const {
  if (is_na) return Integer::NA();
  return Integer(-val);
}
inline Double Double::operator-() const {
  if (is_na) return Double::NA();
  return Double(-val);
}
inline Dual Dual::operator-() const {
  if (is_na) return Dual::NA();
  if (is_na_dot) return Dual(-val, std::numeric_limits<double>::quiet_NaN());
  return Dual(-val, -dot);
}

// Concepts for scalar
// --------------------------------------------------------------------------------------------------
template<typename T>
struct from_ast_scalar {
  using type = T;
};
template<> struct from_ast_scalar<Double>  { using type = double;  };
template<> struct from_ast_scalar<Integer>     { using type = int; };
template<> struct from_ast_scalar<Logical>    { using type = bool; };
template<> struct from_ast_scalar<ReverseDouble> { using type = double; };
template<typename T>
using from_ast_scalar_t = typename from_ast_scalar<T>::type;

template<typename T>
struct to_ast_scalar {
  using type = T;
};
template<> struct to_ast_scalar<double>  { using type = Double;  };
template<> struct to_ast_scalar<int>     { using type = Integer; };
template<> struct to_ast_scalar<bool>    { using type = Logical; };

template<> struct to_ast_scalar<LogicalRef>  { using type = Logical; };
template<> struct to_ast_scalar<IntegerRef>  { using type = Integer; };
template<> struct to_ast_scalar<DoubleRef>   { using type = Double;  };
template<> struct to_ast_scalar<DualRef>     { using type = Dual;    };
template<> struct to_ast_scalar<ReverseDouble> { using type = ReverseDouble; };

template<typename T>
using to_ast_scalar_t = typename to_ast_scalar<T>::type;

// Common type of scalar types
// --------------------------------------------------------------------------------------------------
template<typename T>
using bare_t = std::remove_cv_t<std::remove_reference_t<T>>;

enum class ScalarRank { Logical, Integer, Double, Dual, ReverseDouble };

// template<typename T> struct scalar_rank;
template <class T>
struct scalar_rank {
  static constexpr int value = -1;
};

template<> struct scalar_rank<Logical> { static constexpr auto value = ScalarRank::Logical; };
template<> struct scalar_rank<Integer> { static constexpr auto value = ScalarRank::Integer; };
template<> struct scalar_rank<Double>  { static constexpr auto value = ScalarRank::Double; };
template<> struct scalar_rank<Dual>    { static constexpr auto value = ScalarRank::Dual; };
template<> struct scalar_rank<ReverseDouble> { static constexpr auto value = ScalarRank::ReverseDouble; }; // highest

template<typename L, typename R>
using common_scalar = std::conditional_t<
(static_cast<int>(scalar_rank<L>::value) >=
static_cast<int>(scalar_rank<R>::value)),
L, R
>;

template<typename L, typename R>
struct common_type {
  using type = common_scalar<
  bare_t<to_ast_scalar_t<L>>,
  bare_t<to_ast_scalar_t<R>>
  >;
};

template<typename L, typename R>
using common_type_t = typename common_type<L, R>::type;

// Concept to detect scalars
template<typename T> concept IsArith =
std::same_as<bare_t<T>, Logical> ||
std::same_as<bare_t<T>, Integer> ||
std::same_as<bare_t<T>, Double> ||
std::same_as<bare_t<T>, Dual>;
template <typename T> constexpr bool IsArithV = IsArith<T>;

// Concept to detect refs to scalars
template<typename T> concept IsArithRef =
std::same_as<bare_t<T>, LogicalRef> ||
std::same_as<bare_t<T>, IntegerRef> ||
std::same_as<bare_t<T>, DoubleRef> ||
std::same_as<bare_t<T>, DualRef>;
template <typename T> constexpr bool IsArithRefV = IsArithRef<T>;

template<typename T> concept IsDouble = std::same_as<T, Double>;
template<typename T> concept IsInteger = std::same_as<T, Integer>;
template<typename T> concept IsLogical = std::same_as<T, Logical>;
template<typename T> concept IsDual = std::same_as<T, Dual>;

template<typename T> concept IsDoubleRef = std::same_as<T, DoubleRef>;
template<typename T> concept IsIntegerRef = std::same_as<T, IntegerRef>;
template<typename T> concept IsLogicalRef = std::same_as<T, LogicalRef>;
template<typename T> concept IsDualRef = std::same_as<T, DualRef>;

// reverse ad
template <typename T> concept IsReverseDouble = IS<T, ReverseDouble>;
template <typename T> concept IsADType = IS<T, ReverseDouble>;

template <typename T> concept IsScalarLike = requires(T t) {
typename T;
requires IsArithV<T> || IsArithRefV<T> || IsADType<T>;
};
template <typename T> concept IsScalarOrScalarRef = requires(T t) {
typename T;
requires IsArithV<T> || IsArithRefV<T> || IsReverseDouble<T>;
};

template <typename I>
concept ScalarIndex =
IsScalarLike<I> &&
!IsLogical<I> &&
!IsLogicalRef<I>;

template <typename... Args>
concept NonEmpty = (sizeof...(Args) > 0);

template <typename... Args>
concept AllScalarIndices =
NonEmpty<Args...> &&
(ScalarIndex<Args> && ...);

template <typename... Args>
concept HasNonScalarIndex = (!ScalarIndex<Args> || ...);

// Second dispatch layer
// --------------------------------------------------------------------------------------------------
// unary - ================================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto operator-(const O& o) -> decltype(o.operator-()) {
  return o.operator-();
}
// sqrt ===================================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto sqrt(const O& o) -> decltype(o.sqrt()) {
  return o.sqrt();
}
// exp ===================================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto exp(const O& o) -> decltype(o.exp()) {
  return o.exp();
}
// log ===================================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto log(const O& o) -> decltype(o.log()) {
  return o.log();
}
// log10 ===================================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto log10(const O& o) -> decltype(o.log10()) {
  return o.log10();
}
// tan/atan/tanh =========================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto tan(const O& o) -> decltype(o.tan()) {
  return o.tan();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto atan(const O& o) -> decltype(o.atan()) {
  return o.atan();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto tanh(const O& o) -> decltype(o.tanh()) {
  return o.tanh();
}
// cos/acos/cosh =========================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto cos(const O& o) -> decltype(o.cos()) {
  return o.cos();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto acos(const O& o) -> decltype(o.acos()) {
  return o.acos();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto cosh(const O& o) -> decltype(o.cosh()) {
  return o.cosh();
}
// sin/asin/sinh =========================================
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto sin(const O& o) -> decltype(o.sin()) {
  return o.sin();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto asin(const O& o) -> decltype(o.asin()) {
  return o.asin();
}
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline auto sinh(const O& o) -> decltype(o.sinh()) {
  return o.sinh();
}

// abs ===================================================
// This is required for Eigen
template<typename O>
requires (!IsArray<O> && IsScalarOrScalarRef<O>)
inline double abs(const O& o) {
  return std::abs(static_cast<double>(get_val(o)));
}

// && ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator&&(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator&&( common_type_t<L,R>(r) ) ) {
  using CT = common_type_t<L, R>;
  return CT(l).operator&&( CT(r) );
}

// || ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator||(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator||( common_type_t<L,R>(r) ) ) {
  using CT = common_type_t<L, R>;
  return CT(l).operator||( CT(r) );
}

// != ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && (IsScalarOrScalarRef<L> || IsCppArithV<L>) && (IsScalarOrScalarRef<R> || IsCppArithV<R>) )
inline auto operator!=(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator!=( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator!=( CT(r) );
}
// >= ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator>=(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator>=( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator>=( CT(r) );
}
// > ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator>(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator>( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator>( CT(r) );
}
// <= ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator<=(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator<=( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator<=( CT(r) );
}
// < =====================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator<(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator<( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator<( CT(r) );
}
// == ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator==(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator==( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator==( CT(r) );
}
// pow ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto pow(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).pow( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).pow( CT(r) );
}
// / ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator/(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator/( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator/( CT(r) );
}
// + ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator+(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator+( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator+( CT(r) );
}
// - ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator-(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator-( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator-( CT(r) );
}
// * ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R> && IsScalarOrScalarRef<L> && IsScalarOrScalarRef<R>)
inline auto operator*(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator*( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator*( CT(r) );
}

// Retrieve scalar value
// --------------------------------------------------------------------------------------------------
template<typename T> requires IsArithV<T>    inline auto get_scalar_val(const T& t) { return t; }
template<typename T> requires IsArithRefV<T> inline auto get_scalar_val(const T& t) {
  using DecayedT = Decayed<T>;
  using RetType  = to_ast_scalar_t<DecayedT>;
  return RetType(t);
}
inline ReverseDouble get_scalar_val(const ReverseDouble& x) { return x; }

template<typename T> requires std::is_arithmetic_v<T> inline T    get_val(const T& t) { return t; }
template<typename T> requires IsArithV<T>             inline auto get_val(const T& t) { return t.val; }
template<typename T> requires IsArithRefV<T>          inline auto get_val(const T& t) { return *(t.p_val); }
inline double get_val(const ReverseDouble& x) { return x.get_val_from_tape(); }

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

} // namespace etr

#endif
