#ifndef SCALARS_ETR_H
#define SCALARS_ETR_H

namespace etr {

/*
-----------------------------------------------------------------------------------------------------------
Tape definition for reverse automatic differentiation
- Reverse-mode AD tape (used by ReverseDouble, defined further down with the other scalar types).
- The tape is self-contained: only primitive types, std::vector, and <cmath>.
- One tape per thread; translated functions call TAPE_INTERN.clear() at entry,
  which resets size without freeing capacity.
-----------------------------------------------------------------------------------------------------------
*/
struct from_tape_id_t {};

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
  Kind kind;
  std::size_t rows_a;
  std::size_t cols_a; // == rows_b
  std::size_t cols_b;
  // Per side, exactly one of {ids, vals} is populated. ids = ReverseDouble
  // input (values pulled from tape at backward time). vals = r-value input
  // (Double/Integer/Logical), copied here so backward survives the original
  // memory going out of scope.
  std::vector<int> a_ids;
  std::vector<double> a_vals;
  std::vector<int> b_ids;
  std::vector<double> b_vals;
  int out_tape_id_first{-1}; // tape id of the head node; outputs occupy [out_tape_id_first, out_tape_id_first + rows_a*cols_b)
};

struct ReverseTape {
  std::vector<ROp> op;
  std::vector<int> a;
  std::vector<int> b;
  std::vector<double> val;
  std::vector<double> adj;
  std::vector<uint8_t> is_na; // uint8_t on purpose — avoids vector<bool>.
  std::vector<BlockOp> blocks;

  inline std::size_t size() const noexcept { return op.size(); }

  inline void clear() noexcept {
    op.clear();
    a.clear();
    b.clear();
    val.clear();
    adj.clear();
    is_na.clear();
    blocks.clear();
  }

  inline void reserve(std::size_t n) {
    op.reserve(n);
    a.reserve(n);
    b.reserve(n);
    val.reserve(n);
    adj.reserve(n);
    is_na.reserve(n);
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
    if (na) {
      v = std::numeric_limits<double>::quiet_NaN();
    }
    return push_node(ROp::Const, -1, -1, v, na);
  }

  inline int push_var(double v, bool na = false) {
    if (na) {
      v = std::numeric_limits<double>::quiet_NaN();
    }
    return push_node(ROp::Var, -1, -1, v, na);
  }

  int push_unary(ROp u, int ai) {
    if (node_is_na(ai)) {
      return push_node(u, ai, -1, std::numeric_limits<double>::quiet_NaN(), true);
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
        ass<"push_unary: unsupported op">(false);
    }
    return push_node(u, ai, -1, outv, false);
  }

  int push_binary(ROp bo, int ai, int bi) {
    if (node_is_na(ai) || node_is_na(bi)) {
      return push_node(bo, ai, bi, std::numeric_limits<double>::quiet_NaN(), true);
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
        ass<"push_binary: unsupported op">(false);
    }
    return push_node(bo, ai, bi, outv, false);
  }

  // Block matrix multiply C = A * B, column-major (R convention).
  // A is rows_a x cols_a, B is cols_a x cols_b, C is rows_a x cols_b.
  // All three buffers laid out as A[m,k] = idx[k*rows_a + m], etc.
  // Per side, exactly one of {ids, vals} must be non-empty (the other empty).
  // Returns the head tape id; output cells occupy [head, head + rows_a*cols_b).
  inline int push_matmul(std::size_t rows_a, std::size_t cols_a, std::size_t cols_b,
                         std::vector<int> a_ids, std::vector<double> a_vals,
                         std::vector<int> b_ids, std::vector<double> b_vals) {
    ass<"push_matmul: A side requires exactly one of {ids, vals}">(a_ids.empty() != a_vals.empty());
    ass<"push_matmul: B side requires exactly one of {ids, vals}">(b_ids.empty() != b_vals.empty());
    const std::size_t a_size = a_ids.empty() ? a_vals.size() : a_ids.size();
    const std::size_t b_size = b_ids.empty() ? b_vals.size() : b_ids.size();
    ass<"push_matmul: A size mismatch">(a_size == rows_a * cols_a);
    ass<"push_matmul: B size mismatch">(b_size == cols_a * cols_b);
    const std::size_t out_size = rows_a * cols_b;
    const int block_idx = static_cast<int>(blocks.size());
    // Pre-scan NA per row of A and per column of B. C[i,j] is NA iff
    // any A[i,*] is NA OR any B[*,j] is NA — same poisoning rule as R's %*%.
    // For ids inputs we read the authoritative is_na flag; for vals inputs
    // we treat NaN as NA (the distinction was already lost upstream).
    std::vector<uint8_t> a_row_na(rows_a, 0);
    std::vector<uint8_t> b_col_na(cols_b, 0);
    for (std::size_t i = 0; i < rows_a; ++i) {
      for (std::size_t k = 0; k < cols_a; ++k) {
        const std::size_t idx = k * rows_a + i;
        const bool na_here = a_ids.empty() ? std::isnan(a_vals[idx]) : (is_na[static_cast<std::size_t>(a_ids[idx])] != 0);
        if (na_here) {
          a_row_na[i] = 1; break;
        }
      }
    }
    for (std::size_t j = 0; j < cols_b; ++j) {
      for (std::size_t k = 0; k < cols_a; ++k) {
        const std::size_t idx = j * cols_a + k;
        const bool na_here = b_ids.empty() ? std::isnan(b_vals[idx]) : (is_na[static_cast<std::size_t>(b_ids[idx])] != 0);
        if (na_here) {
          b_col_na[j] = 1; break;
        }
      }
    }

    // Gather (or alias) inputs into contiguous double buffers for dgemm.
    std::vector<double> a_buf;
    std::vector<double> b_buf;
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
    std::vector<double> out_vals(out_size, 0.0);
    std::vector<uint8_t> out_na(out_size, 0);
    const int Mi = static_cast<int>(rows_a);
    const int Ni = static_cast<int>(cols_b);
    const int Ki = static_cast<int>(cols_a);
    const double alpha = 1.0;
    const double beta = 0.0;
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
          out_na[out_idx] = 1;
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
    blk.kind = BlockOp::Kind::MatMul;
    blk.rows_a = rows_a;
    blk.cols_a = cols_a;
    blk.cols_b = cols_b;
    blk.a_ids = std::move(a_ids);
    blk.a_vals = std::move(a_vals);
    blk.b_ids = std::move(b_ids);
    blk.b_vals = std::move(b_vals);
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
          std::vector<double> a_buf;
          std::vector<double> b_buf;
          const double* A_ptr;
          if (blk.a_ids.empty()) {
            A_ptr = blk.a_vals.data();
          } else {
            a_buf.resize(M * K);
            for (std::size_t k = 0; k < a_buf.size(); ++k) {
              a_buf[k] = val[static_cast<std::size_t>(blk.a_ids[k])];
            }
            A_ptr = a_buf.data();
          }
          const double* B_ptr;
          if (blk.b_ids.empty()) {
            B_ptr = blk.b_vals.data();
          } else {
            b_buf.resize(K * N);
            for (std::size_t k = 0; k < b_buf.size(); ++k) {
              b_buf[k] = val[static_cast<std::size_t>(blk.b_ids[k])];
            }
            B_ptr = b_buf.data();
          }
          const int Mi = static_cast<int>(M);
          const int Ni = static_cast<int>(N);
          const int Ki = static_cast<int>(K);
          const double one = 1.0;
          const double zero = 0.0;
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

// MinGW: namespace-scope `inline thread_local` collides across TUs
// (TLS init wrapper not weak). Hide in inline fn; macro keeps `.` syntax.
inline ReverseTape& TAPE_INTERN_ref() {
  thread_local ReverseTape instance;
  return instance;
}
#define TAPE_INTERN TAPE_INTERN_ref()

/*
--------------------------------------------------------------------------------------------------
Scalar types (First dispatch layer)
--------------------------------------------------------------------------------------------------
*/
struct Logical;
struct Integer;
struct Double;
struct Dual;
struct ReverseDouble;
struct LogicalRef;
struct IntegerRef;
struct DoubleRef;
struct DualRef;

/*
--------------------------------------------------------------------------------------------------
Casting scalar types
- from_ast_scalar converts etr::Scalars to raw C++ scalars
- to_ast_scalar converts double, int, and bool to Double, Integer, and Logical respectivly
- to_ast_scalar converts LogicalRef, IntegerRef, DoubleRef, and DualRef to
  Logical, Integer, Double, and Dual. ReverseDouble has no reference wrapper:
  a ReverseDouble is already an 8-byte handle into the tape.
--------------------------------------------------------------------------------------------------
*/
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

template<typename T>
using to_ast_scalar_t = typename to_ast_scalar<T>::type;

/*
--------------------------------------------------------------------------------------------------
Common type of scalar types
determine the common type of two etr::Scalars
--------------------------------------------------------------------------------------------------
*/
template<typename T>
using bare_t = std::remove_cv_t<std::remove_reference_t<T>>;

enum class ScalarRank { LogicalRef, IntegerRef, DoubleRef, DualRef, Logical, Integer, Double, Dual, ReverseDouble };

template <class T> struct scalar_rank {
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
static_cast<int>(scalar_rank<R>::value)), L, R>;

template<typename L, typename R>
struct common_type {
  using type = common_scalar<
  bare_t<to_ast_scalar_t<L>>,
  bare_t<to_ast_scalar_t<R>>
  >;
};

template<typename L, typename R>
using common_type_t = typename common_type<L, R>::type;

/*
--------------------------------------------------------------------------------------------------
Concepts for sclars
- is it a scalar --> IsArith
- is it a scalar reference --> IsArithRef
- IsXY where XY can any scalar or scalar reference
--------------------------------------------------------------------------------------------------
*/
template<typename T> concept IsArith =
std::same_as<bare_t<T>, Logical> ||
std::same_as<bare_t<T>, Integer> ||
std::same_as<bare_t<T>, Double> ||
std::same_as<bare_t<T>, ReverseDouble> ||
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
template<typename T> concept IsReverseDouble = std::same_as<T, ReverseDouble>;

template<typename T> concept IsDoubleRef = std::same_as<T, DoubleRef>;
template<typename T> concept IsIntegerRef = std::same_as<T, IntegerRef>;
template<typename T> concept IsLogicalRef = std::same_as<T, LogicalRef>;
template<typename T> concept IsDualRef = std::same_as<T, DualRef>;

/*
--------------------------------------------------------------------------------------------------
Check for is scalars
- is scalar like either scalar or a scalar reference
--> basically AllScalarIndices and HasNonScalarIndex are required in preserved subsetting
--------------------------------------------------------------------------------------------------
*/
template <typename T> concept IsScalarLike = requires(T t) {
typename T;
requires IsArithV<T> || IsArithRefV<T>;
};
template <typename T> concept IsScalarOrScalarRef = requires(T t) {
typename T;
requires IsArithV<T> || IsArithRefV<T>;
};
template <typename I>
concept ScalarIndex =
IsScalarLike<I> &&
!IsLogical<I> &&
!IsLogicalRef<I>;

template <typename... Args>
concept NonEmpty = (sizeof...(Args) > 0);

template <typename... Args>
concept AllScalarIndices = NonEmpty<Args...> && (ScalarIndex<Args> && ...);

template <typename... Args>
concept HasNonScalarIndex = (!ScalarIndex<Args> || ...);


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
  uint8_t is_na{0};

  inline ReverseDouble();
  // Tag-based ctor for wrapping an existing tape id without pushing a new
  // node (used by mat_mul and other block ops to materialize result handles).
  inline ReverseDouble(from_tape_id_t, int id_, uint8_t is_na_) noexcept;
  inline ReverseDouble(bool v);
  inline ReverseDouble(int v);
  inline ReverseDouble(double v);
  inline ReverseDouble(Logical v);
  inline ReverseDouble(Integer v);
  inline ReverseDouble(Double v);
  inline ReverseDouble(Dual v);

  // An assignment `x = y` between two ReverseDouble values rebinds x.id to
  // y.id (same R let-binding semantics). A subsequent `x = y + z` pushes a
  // fresh node normally.
  ReverseDouble(const ReverseDouble&) = default;
  ReverseDouble(ReverseDouble&&) noexcept = default;
  ReverseDouble& operator=(const ReverseDouble&) = default;
  ReverseDouble& operator=(ReverseDouble&&) = default;

  static inline ReverseDouble Var(double v) {
    ReverseDouble x;
    x.is_na = 0;
    x.id = TAPE_INTERN.push_var(v, 0);
    return x;
  }
  static inline ReverseDouble NA() {
    ReverseDouble x;
    x.is_na = 1;
    x.id = TAPE_INTERN.push_const(std::numeric_limits<double>::quiet_NaN(), 1);
    return x;
  }
  static inline ReverseDouble NaN() {
    ReverseDouble x;
    x.is_na = 0;
    x.id = TAPE_INTERN.push_const(std::numeric_limits<double>::quiet_NaN(), 0);
    return x;
  }
  static inline ReverseDouble Inf() {
    ReverseDouble x;
    x.is_na = 0;
    x.id = TAPE_INTERN.push_const(std::numeric_limits<double>::infinity(), 0);
    return x;
  }

  inline double get_val_from_tape() const { return TAPE_INTERN.val [static_cast<std::size_t>(id)]; }
  inline double get_grad_from_tape() const { return TAPE_INTERN.adj [static_cast<std::size_t>(id)]; }

  inline bool isNA() const noexcept { return static_cast<bool>(is_na); }
  inline bool isNaN() const noexcept { return !static_cast<bool>(is_na) && std::isnan(get_val_from_tape()); }
  inline bool isFinite() const noexcept { return !static_cast<bool>(is_na) && std::isfinite(get_val_from_tape()); }
  inline bool isInfinite() const noexcept { return !static_cast<bool>(is_na) && !isNaN() && !std::isfinite(get_val_from_tape()); }

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


  inline ReverseDouble operator+(const ReverseDouble& r) const;
  inline ReverseDouble operator-(const ReverseDouble& r) const;
  inline ReverseDouble operator*(const ReverseDouble& r) const;
  inline ReverseDouble operator/(const ReverseDouble& r) const;
  inline ReverseDouble pow (const ReverseDouble& r) const;

  inline ReverseDouble& operator+=(const ReverseDouble& r);
  inline ReverseDouble& operator-=(const ReverseDouble& r);
  inline ReverseDouble& operator*=(const ReverseDouble& r);
  inline ReverseDouble& operator/=(const ReverseDouble& r);

  inline ReverseDouble operator-() const;

  inline Logical operator==(const ReverseDouble& r) const;
  inline Logical operator<(const ReverseDouble& r) const;
  inline Logical operator<=(const ReverseDouble& r) const;
  inline Logical operator>(const ReverseDouble& r) const;
  inline Logical operator>=(const ReverseDouble& r) const;
  inline Logical operator!=(const ReverseDouble& r) const;

  inline Logical operator&&(const ReverseDouble& r) const;
  inline Logical operator||(const ReverseDouble& r) const;
  inline Logical operator&(const ReverseDouble& r) const;
  inline Logical operator|(const ReverseDouble& r) const;

  inline ReverseDouble sin() const;
  inline ReverseDouble sinh() const;
  inline ReverseDouble asin() const;
  inline ReverseDouble cos() const;
  inline ReverseDouble cosh() const;
  inline ReverseDouble acos() const;
  inline ReverseDouble tan() const;
  inline ReverseDouble tanh() const;
  inline ReverseDouble atan() const;

  inline ReverseDouble exp() const;
  inline ReverseDouble log() const;
  inline ReverseDouble log10() const;
  inline ReverseDouble sqrt() const;

  template<typename T> requires IsArray<Decayed<T>> inline ReverseDouble& operator=(const T& arr) {
    using inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    const auto arr_val = get_scalar_val(arr.get(0));
    if constexpr (IS<inner, ReverseDouble>) {
      // Rebind to the existing tape node — same semantics as the
      // defaulted scalar operator=
      id = arr_val.id;
      is_na = arr_val.is_na;
    } else {
      // Push a fresh constant for the converted value, then rebind.
      ReverseDouble tmp(arr_val);
      id = tmp.id;
      is_na = tmp.is_na;
    }
    return *this;
  }
};

/*
Constructors for scalar values
*/
inline Logical::Logical() : val(false), is_na(false) {}
inline Logical::Logical(bool v) : val(v), is_na(false) {}
inline Logical::Logical(Integer v) : val(static_cast<bool>(v.val)), is_na(v.is_na) {}
inline Logical::Logical(Double v) : val(static_cast<bool>(v.val)), is_na(v.is_na) {}
inline Logical::Logical(Dual v) : val(static_cast<bool>(v.val)), is_na(v.is_na) {}
inline Logical::Logical(ReverseDouble v) : val(static_cast<bool>(v.get_val_from_tape())), is_na(v.is_na) {}

inline Integer::Integer() : val(0), is_na(false) {}
inline Integer::Integer(int v) : val(v), is_na(false) {}
inline Integer::Integer(Logical v) : val(static_cast<int>(v.val)), is_na(v.is_na) {}
inline Integer::Integer(Double v) : val(static_cast<int>(v.val)), is_na(v.is_na) {}
inline Integer::Integer(Dual v) : val(static_cast<int>(v.val)), is_na(v.is_na) {}
inline Integer::Integer(ReverseDouble v) : val(static_cast<int>(v.get_val_from_tape())), is_na(v.is_na) {}

inline Double::Double() : val(0.0), is_na(false) {}
inline Double::Double(double v) : val(v), is_na(false) {}
inline Double::Double(Logical v) : val(static_cast<double>(v.val)), is_na(v.is_na) {}
inline Double::Double(Integer v) : val(static_cast<double>(v.val)), is_na(v.is_na) {}
inline Double::Double(Dual v) : val(v.val), is_na(v.is_na) {}
inline Double::Double(ReverseDouble v) : val(v.get_val_from_tape()), is_na(v.is_na) {}

inline Dual::Dual() : val(0.0), dot(0.0), is_na(false), is_na_dot(false) {}
inline Dual::Dual(double v, double d) : val(v), dot(d), is_na(false), is_na_dot(false) {}
inline Dual::Dual(Logical v) : val(static_cast<double>(v.val)), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
inline Dual::Dual(Integer v) : val(static_cast<double>(v.val)), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
inline Dual::Dual(Double v) : val(v.val), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
inline Dual::Dual(ReverseDouble v) : val(v.get_val_from_tape()), dot(0.0), is_na(v.is_na), is_na_dot(false) {}
template<typename T> requires std::is_arithmetic_v<T>
inline Dual::Dual(T v) : val(static_cast<double>(v)), dot(0.0), is_na(std::isnan(v)), is_na_dot(false) {}

inline ReverseDouble::ReverseDouble() { id = TAPE_INTERN.push_const(0.0, 0); }
inline ReverseDouble::ReverseDouble(double v) { id = TAPE_INTERN.push_const(v, 0); }
inline ReverseDouble::ReverseDouble(from_tape_id_t, int id_, uint8_t is_na_) noexcept : id(id_), is_na(is_na_) {}
inline ReverseDouble::ReverseDouble(int v)  : ReverseDouble(static_cast<double>(v)) {}
inline ReverseDouble::ReverseDouble(bool v) : ReverseDouble(v ? 1.0 : 0.0) {}
inline ReverseDouble::ReverseDouble(Logical v) { 
  is_na = static_cast<uint8_t>(v.is_na);
  id = TAPE_INTERN.push_const(static_cast<double>(v.val), is_na);
}
inline ReverseDouble::ReverseDouble(Integer v) {
  is_na = static_cast<uint8_t>(v.is_na);
  id = TAPE_INTERN.push_const(static_cast<double>(v.val), is_na);
}
inline ReverseDouble::ReverseDouble(Double v) {
  is_na = static_cast<uint8_t>(v.is_na);
  id = TAPE_INTERN.push_const(static_cast<double>(v.val), is_na);
}
inline ReverseDouble::ReverseDouble(Dual v) {
  is_na = static_cast<uint8_t>(v.is_na);
  id = TAPE_INTERN.push_const(v.val, is_na);
}

/*
printing scalar values
*/
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
inline std::ostream& operator<<(std::ostream& os, const ReverseDouble& x) {
  if (x.isNA()) return os << "NA";
  const double v = x.get_val_from_tape();
  if (std::isnan(v)) return os << "NaN";
  if (!std::isfinite(v)) return os << (v > 0 ? "Inf" : "-Inf");
  return os << v;
}

/*
Apply arithmetic and assign
*/
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
inline ReverseDouble& ReverseDouble::operator+=(const ReverseDouble& r) {
  return *this = (*this + r);
}
inline ReverseDouble& ReverseDouble::operator-=(const ReverseDouble& r) {
  return *this = (*this - r);
}
inline ReverseDouble& ReverseDouble::operator*=(const ReverseDouble& r) {
  return *this = (*this * r);
}
inline ReverseDouble& ReverseDouble::operator/=(const ReverseDouble& r) {
  return *this = (*this / r);
}

/*
Arithmetic operations; binary operations
*/
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
inline ReverseDouble ReverseDouble::operator+(const ReverseDouble& r) const {
  return binary_(ROp::Add, r);
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
inline ReverseDouble ReverseDouble::operator-(const ReverseDouble& r) const {
  return binary_(ROp::Sub, r);
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
inline ReverseDouble ReverseDouble::operator*(const ReverseDouble& r) const {
  return binary_(ROp::Mul, r);
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
inline ReverseDouble ReverseDouble::operator/(const ReverseDouble& r) const {
  return binary_(ROp::Div, r);
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
inline ReverseDouble ReverseDouble::pow (const ReverseDouble& r) const {
  return binary_(ROp::Pow, r);
}

/*
Comparison operators
*/
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
inline Logical ReverseDouble::operator==(const ReverseDouble& r) const {
  if (is_na || r.is_na) return Logical::NA();
  return Logical{get_val_from_tape() == r.get_val_from_tape()};
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
inline Logical ReverseDouble::operator<(const ReverseDouble& r) const {
  if (is_na || r.is_na) return Logical::NA();
  return Logical{get_val_from_tape() <  r.get_val_from_tape()};
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
inline Logical ReverseDouble::operator<=(const ReverseDouble& r) const {
  if (is_na || r.is_na) return Logical::NA();
  return Logical{get_val_from_tape() <= r.get_val_from_tape()};
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
inline Logical ReverseDouble::operator>(const ReverseDouble& r) const {
  if (is_na || r.is_na) return Logical::NA();
  return Logical{get_val_from_tape() >  r.get_val_from_tape()};
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
inline Logical ReverseDouble::operator>=(const ReverseDouble& r) const {
  if (is_na || r.is_na) return Logical::NA();
  return Logical{get_val_from_tape() >= r.get_val_from_tape()};
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
inline Logical ReverseDouble::operator!=(const ReverseDouble& r) const {
  if (is_na || r.is_na) return Logical::NA();
  return Logical{get_val_from_tape() != r.get_val_from_tape()};
}

/*
Logical operators
*/
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
inline Logical ReverseDouble::operator&&(const ReverseDouble& r) const {
  if (is_na || r.is_na) return Logical::NA();
  return Logical{get_val_from_tape() != 0.0 && r.get_val_from_tape() != 0.0};
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
inline Logical ReverseDouble::operator||(const ReverseDouble& r) const {
  if (is_na || r.is_na) return Logical::NA();
  return Logical{get_val_from_tape() != 0.0 || r.get_val_from_tape() != 0.0};
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
inline Logical ReverseDouble::operator&(const ReverseDouble& r) const {
  if (is_na || r.is_na) return Logical::NA();
  return Logical{get_val_from_tape() != 0.0 && r.get_val_from_tape() != 0.0};
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
inline Logical ReverseDouble::operator|(const ReverseDouble& r) const {
  if (is_na || r.is_na) return Logical::NA();
  return Logical{get_val_from_tape() != 0.0 || r.get_val_from_tape() != 0.0};
}

/*
 trigonometric functions
 */
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
inline ReverseDouble ReverseDouble::sin() const {
  return unary_(ROp::Sin);
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
inline ReverseDouble ReverseDouble::sinh() const {
  return unary_(ROp::Sinh);
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
inline ReverseDouble ReverseDouble::asin() const {
  return unary_(ROp::Asin);
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
inline ReverseDouble ReverseDouble::cos() const {
  return unary_(ROp::Cos);
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
inline ReverseDouble ReverseDouble::cosh() const {
  return unary_(ROp::Cosh);
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
inline ReverseDouble ReverseDouble::acos() const {
  return unary_(ROp::Acos);
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
inline ReverseDouble ReverseDouble::tan() const {
  return unary_(ROp::Tan);
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
inline ReverseDouble ReverseDouble::tanh() const {
  return unary_(ROp::Tanh);
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
inline ReverseDouble ReverseDouble::atan() const {
  return unary_(ROp::Atan);
}

/*
 other unary math functions
*/
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
inline ReverseDouble ReverseDouble::exp() const {
  return unary_(ROp::Exp);
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
inline ReverseDouble ReverseDouble::log() const {
  return unary_(ROp::Log);
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
inline ReverseDouble ReverseDouble::log10() const {
  return unary_(ROp::Log10);
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
inline ReverseDouble ReverseDouble::sqrt() const {
  return unary_(ROp::Sqrt);
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
inline ReverseDouble ReverseDouble::operator-() const {
  if (is_na) return NA();
  ReverseDouble out;
  out.id = TAPE_INTERN.push_unary(ROp::Neg, id);
  out.is_na = TAPE_INTERN.is_na[static_cast<std::size_t>(out.id)] != 0;
  return out;
}

/*
--------------------------------------------------------------------------------------------------
Second dispatch layer
- these function overloads are used when no Arrays are involved in an operation
--------------------------------------------------------------------------------------------------
*/
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

/*
--------------------------------------------------------------------------------------------------
Retrieve scalar value
--------------------------------------------------------------------------------------------------
*/
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

/*
--------------------------------------------------------------------------------------------------
Reference scalars
basically the same as the normal scalars but storing a pointer to a value
all the operations are not defined for these classes as they are passed through the second
dispatch layer, as the second dispatch layer requires only that the objects are not arrays
and that they are either sclars or scalar references.
--------------------------------------------------------------------------------------------------
*/
struct LogicalRef {
  bool* p_val;
  bool* p_na;
  operator Logical() const {
    Logical out(*p_val);
    if (p_na) out.is_na = *p_na;
    return out;
  }
  // Scalar operations, delegated to the referenced value via operator Logical().
  inline Integer operator+(const Logical& o) const { return Logical(*this) + o; }
  inline Integer operator-(const Logical& o) const { return Logical(*this) - o; }
  inline Integer operator*(const Logical& o) const { return Logical(*this) * o; }
  inline Double  operator/(const Logical& o) const { return Logical(*this) / o; }
  inline Double  pow(const Logical& o) const { return Logical(*this).pow(o); }
  inline Logical operator==(const Logical& o) const { return Logical(*this) == o; }
  inline Logical operator< (const Logical& o) const { return Logical(*this) <  o; }
  inline Logical operator<=(const Logical& o) const { return Logical(*this) <= o; }
  inline Logical operator> (const Logical& o) const { return Logical(*this) >  o; }
  inline Logical operator>=(const Logical& o) const { return Logical(*this) >= o; }
  inline Logical operator!=(const Logical& o) const { return Logical(*this) != o; }
  inline Logical operator&&(const Logical& o) const { return Logical(*this) && o; }
  inline Logical operator||(const Logical& o) const { return Logical(*this) || o; }
  inline Logical operator& (const Logical& o) const { return Logical(*this) &  o; }
  inline Logical operator| (const Logical& o) const { return Logical(*this) |  o; }
  inline Double sin()   const { return Logical(*this).sin(); }
  inline Double asin()  const { return Logical(*this).asin(); }
  inline Double sinh()  const { return Logical(*this).sinh(); }
  inline Double cos()   const { return Logical(*this).cos(); }
  inline Double acos()  const { return Logical(*this).acos(); }
  inline Double cosh()  const { return Logical(*this).cosh(); }
  inline Double tan()   const { return Logical(*this).tan(); }
  inline Double atan()  const { return Logical(*this).atan(); }
  inline Double tanh()  const { return Logical(*this).tanh(); }
  inline Double exp()   const { return Logical(*this).exp(); }
  inline Double log()   const { return Logical(*this).log(); }
  inline Double log10() const { return Logical(*this).log10(); }
  inline Double sqrt()  const { return Logical(*this).sqrt(); }
  inline Integer operator-() const { return -Logical(*this); }
  inline bool isNA() const noexcept { return Logical(*this).isNA(); }
  // Explicit copy-assign: write through the referenced slot, do not rebind.
  // Without this the compiler-generated copy-assign copies pointers and
  // statements like `at(a, i) = at(a, j)` silently become no-ops.
  LogicalRef& operator=(const LogicalRef& other) {
    if (this == &other) return *this;
    *p_val = *other.p_val;
    if (p_na) *p_na = other.p_na ? *other.p_na : false;
    return *this;
  }
  LogicalRef& operator=(const Logical& x) {
    *p_val = get_val(x);
    if (p_na) *p_na = x.is_na;
    return *this;
  }
  template<typename T> requires (IsArithV<T> || IsReverseDouble<T>) LogicalRef& operator=(const T& x) {
    *p_val = static_cast<bool>(get_val(x));
    if (p_na) *p_na = get_scalar_val(x).is_na;
    return *this;
  }
  explicit inline operator bool() const { return *p_val; }
  explicit inline LogicalRef(bool* v, bool* n = nullptr);
  inline LogicalRef(Logical) = delete;
  inline LogicalRef(bool) = delete;

  template<typename T> requires IsArray<Decayed<T>> inline LogicalRef& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    *this = arr.get(0);
    return *this;
  }
};

struct IntegerRef {
  int* p_val;
  bool* p_na;
  operator Integer() const {
    Integer out(*p_val);
    if (p_na) out.is_na = *p_na;
    return out;
  }
  // Scalar operations, delegated to the referenced value via operator Integer().
  inline Integer operator+(const Integer& o) const { return Integer(*this) + o; }
  inline Integer operator-(const Integer& o) const { return Integer(*this) - o; }
  inline Integer operator*(const Integer& o) const { return Integer(*this) * o; }
  inline Double  operator/(const Integer& o) const { return Integer(*this) / o; }
  inline Double  pow(const Integer& o) const { return Integer(*this).pow(o); }
  inline IntegerRef& operator+=(const Integer& o) { *this = (Integer(*this) + o); return *this; }
  inline IntegerRef& operator-=(const Integer& o) { *this = (Integer(*this) - o); return *this; }
  inline IntegerRef& operator*=(const Integer& o) { *this = (Integer(*this) * o); return *this; }
  inline Logical operator==(const Integer& o) const { return Integer(*this) == o; }
  inline Logical operator< (const Integer& o) const { return Integer(*this) <  o; }
  inline Logical operator<=(const Integer& o) const { return Integer(*this) <= o; }
  inline Logical operator> (const Integer& o) const { return Integer(*this) >  o; }
  inline Logical operator>=(const Integer& o) const { return Integer(*this) >= o; }
  inline Logical operator!=(const Integer& o) const { return Integer(*this) != o; }
  inline Logical operator&&(const Integer& o) const { return Integer(*this) && o; }
  inline Logical operator||(const Integer& o) const { return Integer(*this) || o; }
  inline Logical operator& (const Integer& o) const { return Integer(*this) &  o; }
  inline Logical operator| (const Integer& o) const { return Integer(*this) |  o; }
  inline Double sin()   const { return Integer(*this).sin(); }
  inline Double asin()  const { return Integer(*this).asin(); }
  inline Double sinh()  const { return Integer(*this).sinh(); }
  inline Double cos()   const { return Integer(*this).cos(); }
  inline Double acos()  const { return Integer(*this).acos(); }
  inline Double cosh()  const { return Integer(*this).cosh(); }
  inline Double tan()   const { return Integer(*this).tan(); }
  inline Double atan()  const { return Integer(*this).atan(); }
  inline Double tanh()  const { return Integer(*this).tanh(); }
  inline Double exp()   const { return Integer(*this).exp(); }
  inline Double log()   const { return Integer(*this).log(); }
  inline Double log10() const { return Integer(*this).log10(); }
  inline Double sqrt()  const { return Integer(*this).sqrt(); }
  inline Integer operator-() const { return -Integer(*this); }
  inline bool isNA() const noexcept { return Integer(*this).isNA(); }
  IntegerRef& operator=(const IntegerRef& other) {
    if (this == &other) return *this;
    *p_val = *other.p_val;
    if (p_na) *p_na = other.p_na ? *other.p_na : false;
    return *this;
  }
  IntegerRef& operator=(const Integer& x) {
    *p_val = get_val(x);
    if (p_na) *p_na = x.is_na;
    return *this;
  }
  template<typename T> requires (IsArithV<T> || IsReverseDouble<T>) IntegerRef& operator=(const T& x) {
    *p_val = static_cast<int>(get_val(x));
    if (p_na) *p_na = get_scalar_val(x).is_na;
    return *this;
  }
  explicit inline IntegerRef(int* v, bool* n = nullptr);
  inline IntegerRef(Integer) = delete;
  inline IntegerRef(int) = delete;

  template<typename T> requires IsArray<Decayed<T>> inline IntegerRef& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    *this = arr.get(0);
    return *this;
  }
};
struct DoubleRef {
  double* p_val;
  bool*   p_na;
  operator Double() const {
    Double out(*p_val);
    if (p_na) out.is_na = *p_na;
    return out;
  }
  // Scalar operations, delegated to the referenced value via operator Double().
  inline Double operator+(const Double& o) const { return Double(*this) + o; }
  inline Double operator-(const Double& o) const { return Double(*this) - o; }
  inline Double operator*(const Double& o) const { return Double(*this) * o; }
  inline Double operator/(const Double& o) const { return Double(*this) / o; }
  inline Double pow(const Double& o) const { return Double(*this).pow(o); }
  inline DoubleRef& operator+=(const Double& o) { *this = (Double(*this) + o); return *this; }
  inline DoubleRef& operator-=(const Double& o) { *this = (Double(*this) - o); return *this; }
  inline DoubleRef& operator*=(const Double& o) { *this = (Double(*this) * o); return *this; }
  inline DoubleRef& operator/=(const Double& o) { *this = (Double(*this) / o); return *this; }
  inline Logical operator==(const Double& o) const { return Double(*this) == o; }
  inline Logical operator< (const Double& o) const { return Double(*this) <  o; }
  inline Logical operator<=(const Double& o) const { return Double(*this) <= o; }
  inline Logical operator> (const Double& o) const { return Double(*this) >  o; }
  inline Logical operator>=(const Double& o) const { return Double(*this) >= o; }
  inline Logical operator!=(const Double& o) const { return Double(*this) != o; }
  inline Logical operator&&(const Double& o) const { return Double(*this) && o; }
  inline Logical operator||(const Double& o) const { return Double(*this) || o; }
  inline Logical operator& (const Double& o) const { return Double(*this) &  o; }
  inline Logical operator| (const Double& o) const { return Double(*this) |  o; }
  inline Double sin()   const { return Double(*this).sin(); }
  inline Double asin()  const { return Double(*this).asin(); }
  inline Double sinh()  const { return Double(*this).sinh(); }
  inline Double cos()   const { return Double(*this).cos(); }
  inline Double acos()  const { return Double(*this).acos(); }
  inline Double cosh()  const { return Double(*this).cosh(); }
  inline Double tan()   const { return Double(*this).tan(); }
  inline Double atan()  const { return Double(*this).atan(); }
  inline Double tanh()  const { return Double(*this).tanh(); }
  inline Double exp()   const { return Double(*this).exp(); }
  inline Double log()   const { return Double(*this).log(); }
  inline Double log10() const { return Double(*this).log10(); }
  inline Double sqrt()  const { return Double(*this).sqrt(); }
  inline Double operator-() const { return -Double(*this); }
  inline bool isNA()       const noexcept { return Double(*this).isNA(); }
  inline bool isNaN()      const noexcept { return Double(*this).isNaN(); }
  inline bool isFinite()   const noexcept { return Double(*this).isFinite(); }
  inline bool isInfinite() const noexcept { return Double(*this).isInfinite(); }
  DoubleRef& operator=(const DoubleRef& other) {
    if (this == &other) return *this;
    *p_val = *other.p_val;
    if (p_na) *p_na = other.p_na ? *other.p_na : false;
    return *this;
  }
  DoubleRef& operator=(const Double& x) {
    *p_val = get_val(x);
    if (p_na) *p_na = x.is_na;
    return *this;
  }
  template<typename T> requires (IsArithV<T> || IsReverseDouble<T>) DoubleRef& operator=(const T& x) {
    *p_val = static_cast<double>(get_val(x));
    if (p_na) *p_na = get_scalar_val(x).is_na;
    return *this;
  }
  explicit inline DoubleRef(double* v, bool* n = nullptr);
  inline DoubleRef(Double) = delete;
  inline DoubleRef(double) = delete;
  template<typename T> requires IsArray<Decayed<T>> inline DoubleRef& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    *this = arr.get(0);
    return *this;
  }
};

struct DualRef {
  double* p_val;
  double* p_dot;
  bool*   p_na;
  bool*   p_na_dot;
  operator Dual() const {
    Dual out(*p_val, *p_dot);
    if (p_na)     out.is_na     = *p_na;
    if (p_na_dot) out.is_na_dot = *p_na_dot;
    return out;
  }
  // Scalar operations, delegated to the referenced value via operator Dual().
  inline Dual operator+(const Dual& o) const { return Dual(*this) + o; }
  inline Dual operator-(const Dual& o) const { return Dual(*this) - o; }
  inline Dual operator*(const Dual& o) const { return Dual(*this) * o; }
  inline Dual operator/(const Dual& o) const { return Dual(*this) / o; }
  inline Dual pow(const Dual& o) const { return Dual(*this).pow(o); }
  inline DualRef& operator+=(const Dual& o) { *this = (Dual(*this) + o); return *this; }
  inline DualRef& operator-=(const Dual& o) { *this = (Dual(*this) - o); return *this; }
  inline DualRef& operator*=(const Dual& o) { *this = (Dual(*this) * o); return *this; }
  inline DualRef& operator/=(const Dual& o) { *this = (Dual(*this) / o); return *this; }
  inline Logical operator==(const Dual& o) const { return Dual(*this) == o; }
  inline Logical operator< (const Dual& o) const { return Dual(*this) <  o; }
  inline Logical operator<=(const Dual& o) const { return Dual(*this) <= o; }
  inline Logical operator> (const Dual& o) const { return Dual(*this) >  o; }
  inline Logical operator>=(const Dual& o) const { return Dual(*this) >= o; }
  inline Logical operator!=(const Dual& o) const { return Dual(*this) != o; }
  inline Logical operator&&(const Dual& o) const { return Dual(*this) && o; }
  inline Logical operator||(const Dual& o) const { return Dual(*this) || o; }
  inline Logical operator& (const Dual& o) const { return Dual(*this) &  o; }
  inline Logical operator| (const Dual& o) const { return Dual(*this) |  o; }
  inline Dual sin()   const { return Dual(*this).sin(); }
  inline Dual asin()  const { return Dual(*this).asin(); }
  inline Dual sinh()  const { return Dual(*this).sinh(); }
  inline Dual cos()   const { return Dual(*this).cos(); }
  inline Dual acos()  const { return Dual(*this).acos(); }
  inline Dual cosh()  const { return Dual(*this).cosh(); }
  inline Dual tan()   const { return Dual(*this).tan(); }
  inline Dual atan()  const { return Dual(*this).atan(); }
  inline Dual tanh()  const { return Dual(*this).tanh(); }
  inline Dual exp()   const { return Dual(*this).exp(); }
  inline Dual log()   const { return Dual(*this).log(); }
  inline Dual log10() const { return Dual(*this).log10(); }
  inline Dual sqrt()  const { return Dual(*this).sqrt(); }
  inline Dual operator-() const { return -Dual(*this); }
  inline bool isNA()          const noexcept { return Dual(*this).isNA(); }
  inline bool isNADot()       const noexcept { return Dual(*this).isNADot(); }
  inline bool isNaN()         const noexcept { return Dual(*this).isNaN(); }
  inline bool isNaNDot()      const noexcept { return Dual(*this).isNaNDot(); }
  inline bool isFinite()      const noexcept { return Dual(*this).isFinite(); }
  inline bool isInfinite()    const noexcept { return Dual(*this).isInfinite(); }
  inline bool isFiniteDot()   const noexcept { return Dual(*this).isFiniteDot(); }
  inline bool isInfiniteDot() const noexcept { return Dual(*this).isInfiniteDot(); }
  DualRef& operator=(const DualRef& other) {
    if (this == &other) return *this;
    *p_val = *other.p_val;
    *p_dot = *other.p_dot;
    if (p_na)     *p_na     = other.p_na     ? *other.p_na     : false;
    if (p_na_dot) *p_na_dot = other.p_na_dot ? *other.p_na_dot : false;
    return *this;
  }
  DualRef& operator=(const Dual& x) {
    *p_val = get_val(x);
    *p_dot = x.dot;
    if (p_na)     *p_na     = x.is_na;
    if (p_na_dot) *p_na_dot = x.is_na_dot;
    return *this;
  }
  template<typename T> requires (IsArithV<T>) DualRef& operator=(const T& x) {
    *p_val    = static_cast<double>(get_val(x));
    *p_dot    = 0.0;
    if (p_na)     *p_na     = x.is_na;
    if (p_na_dot) *p_na_dot = false;
    return *this;
  }
  explicit inline DualRef(double* v, double* d, bool* n = nullptr, bool* nd = nullptr);
  inline DualRef(Dual) = delete;
  inline DualRef(double, double) = delete;
  template<typename T> requires IsArray<Decayed<T>> inline DualRef& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    *this = arr.get(0);
    return *this;
  }
};

inline LogicalRef::LogicalRef(bool*   v, bool* n) : p_val(v), p_na(n) {}
inline IntegerRef::IntegerRef(int*    v, bool* n) : p_val(v), p_na(n) {}
inline DoubleRef ::DoubleRef (double* v, bool* n) : p_val(v), p_na(n) {}
inline DualRef   ::DualRef   (double* v, double* d, bool* n, bool* nd) : p_val(v), p_dot(d), p_na(n), p_na_dot(nd) {}


} // namespace etr

#endif
