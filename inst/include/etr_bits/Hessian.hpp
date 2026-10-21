#ifndef HESSIAN_ETR_HPP
#define HESSIAN_ETR_HPP

namespace etr {

// Forward-over-reverse on the tape (development/Plans/hessian.md).
// f is recorded once; per direction v: tangent forward sweep (dot = J v),
// then a second-order reverse sweep (adj_dot = H v at the inputs).
// Only tape[start, end) is swept; older nodes (outer translation, constants)
// have dot = 0 and collect no adjoints.
struct HessianSweep {
  const ReverseTape& tape;
  std::size_t start;
  std::size_t end;
  std::vector<double> dot;
  std::vector<double> adj;
  std::vector<double> adj_dot;

  HessianSweep(const ReverseTape& t, std::size_t s)
    : tape(t), start(s), end(t.size()), dot(end - s), adj(end - s), adj_dot(end - s) {}

  inline bool local(int id) const { return id >= 0 && static_cast<std::size_t>(id) >= start; }
  inline double value(int id) const { return tape.val[static_cast<std::size_t>(id)]; }
  inline double tangent(int id) const { return local(id) ? dot[static_cast<std::size_t>(id) - start] : 0.0; }
  inline void add(int id, double g, double g_dot) {
    if (!local(id)) return;
    adj[static_cast<std::size_t>(id) - start] += g;
    adj_dot[static_cast<std::size_t>(id) - start] += g_dot;
  }

  // first and second derivative of the unary op at x (y = op(x))
  static inline void unary_derivatives(ROp op, double x, double y, double& d1, double& d2) {
    switch (op) {
      case ROp::Neg:   d1 = -1.0; d2 = 0.0; break;
      case ROp::Sin:   d1 = std::cos(x); d2 = -std::sin(x); break;
      case ROp::Cos:   d1 = -std::sin(x); d2 = -std::cos(x); break;
      case ROp::Tan: {
        const double c = std::cos(x);
        d1 = 1.0 / (c * c);
        d2 = 2.0 * y * d1;
        break;
      }
      case ROp::Asin: {
        const double s = 1.0 - x * x;
        d1 = 1.0 / std::sqrt(s);
        d2 = x / (s * std::sqrt(s));
        break;
      }
      case ROp::Acos: {
        const double s = 1.0 - x * x;
        d1 = -1.0 / std::sqrt(s);
        d2 = -x / (s * std::sqrt(s));
        break;
      }
      case ROp::Atan: {
        const double s = 1.0 + x * x;
        d1 = 1.0 / s;
        d2 = -2.0 * x / (s * s);
        break;
      }
      case ROp::Sinh:  d1 = std::cosh(x); d2 = y; break;
      case ROp::Cosh:  d1 = std::sinh(x); d2 = y; break;
      case ROp::Tanh:  d1 = 1.0 - y * y; d2 = -2.0 * y * d1; break;
      case ROp::Exp:   d1 = y; d2 = y; break;
      case ROp::Log:   d1 = 1.0 / x; d2 = -1.0 / (x * x); break;
      case ROp::Log10: d1 = 1.0 / (x * std::log(10.0)); d2 = -d1 / x; break;
      case ROp::Sqrt:  d1 = 0.5 / y; d2 = -0.25 / (y * y * y); break;
      case ROp::Abs:   d1 = x >= 0.0 ? 1.0 : -1.0; d2 = 0.0; break;
      default:         d1 = 0.0; d2 = 0.0;
    }
  }

  // partial derivatives of the binary op: first (da, db), second (daa, dab, dbb)
  static inline void binary_derivatives(ROp op, double a, double b, double c,
                                        double& da, double& db, double& daa, double& dab, double& dbb) {
    switch (op) {
      case ROp::Add: da = 1.0; db = 1.0; daa = dab = dbb = 0.0; break;
      case ROp::Sub: da = 1.0; db = -1.0; daa = dab = dbb = 0.0; break;
      case ROp::Mul: da = b; db = a; daa = 0.0; dab = 1.0; dbb = 0.0; break;
      case ROp::Div:
        da = 1.0 / b;
        db = -a / (b * b);
        daa = 0.0;
        dab = -1.0 / (b * b);
        dbb = 2.0 * a / (b * b * b);
        break;
      case ROp::Pow: {
        // as reverse(): base rule y * x^(y-1) (finite at x = 0), log terms only for x > 0
        da = b * std::pow(a, b - 1.0);
        daa = b * (b - 1.0) * std::pow(a, b - 2.0);
        if (a > 0.0) {
          const double l = std::log(a);
          db = c * l;
          dab = std::pow(a, b - 1.0) * (1.0 + b * l);
          dbb = c * l * l;
        } else {
          db = dab = dbb = 0.0;
        }
        break;
      }
      default: da = db = daa = dab = dbb = 0.0;
    }
  }

  // B = alpha op(A)^-1 B (side L) or B op(A)^-1 (side R), A upper triangular n x n, B n x n
  static inline void trsm_upper(const char* side, const char* trans, int n,
                                const double* A, double* B) {
    if (n == 0) return;
    const double one = 1.0;
    F77_CALL(dtrsm)(side, "U", trans, "N", &n, &n, &one, A, &n, B, &n FCONE FCONE FCONE FCONE);
  }

  // Phi_U: strictly lower -> 0, diagonal halved
  static inline void phi_upper(std::vector<double>& M, std::size_t n) {
    for (std::size_t c = 0; c < n; c++) {
      M[c * n + c] *= 0.5;
      for (std::size_t r = c + 1; r < n; r++) M[c * n + r] = 0.0;
    }
  }

  // input adjoint of chol: strictly-upper(X + X^T) + diag(X), lower stays 0
  static inline std::vector<double> fold_upper(const std::vector<double>& X, std::size_t n) {
    std::vector<double> g(n * n, 0.0);
    for (std::size_t c = 0; c < n; c++) {
      for (std::size_t r = 0; r <= c; r++) {
        g[c * n + r] = (r == c) ? X[c * n + r] : X[c * n + r] + X[r * n + c];
      }
    }
    return g;
  }

  // R = chol(A), A = R^T R; dpotrf reads only the upper triangle of A:
  // S = R^-T sym(A_dot) R^-1, R_dot = Phi_U(S) R
  inline void chol_forward(const BlockOp& blk) {
    const std::size_t out0 = static_cast<std::size_t>(blk.out_tape_id_first);
    if (blk.a_ids.empty() || tape.is_na[out0]) return;
    const std::size_t n = blk.rows_a;
    const int ni = static_cast<int>(n);
    std::vector<double> A, A_dot, R, R_dot;
    gather(blk.a_ids, blk.a_vals, A, A_dot);
    block_outputs(out0, n * n, R, R_dot);
    std::vector<double> S(n * n);
    for (std::size_t c = 0; c < n; c++) {
      for (std::size_t r = 0; r < n; r++) S[c * n + r] = r <= c ? A_dot[c * n + r] : A_dot[r * n + c];
    }
    trsm_upper("L", "T", ni, R.data(), S.data());
    trsm_upper("R", "N", ni, R.data(), S.data());
    phi_upper(S, n);
    std::vector<double> out_dot(n * n, 0.0);
    gemm("N", "N", ni, ni, ni, S.data(), ni, R.data(), ni, 0.0, out_dot.data(), ni);
    write_block_tangent(out0, out_dot);
  }

  // first order:  M = Phi_U(R_bar R^T), X = R^-1 M R^-T, A_bar = fold(X)
  // second order: M_dot = Phi_U(R_bar_dot R^T + R_bar R_dot^T),
  //               X_dot = R^-1 M_dot R^-T - R^-1 R_dot X - X R_dot^T R^-T, A_bar_dot = fold(X_dot)
  inline void chol_reverse(const BlockOp& blk) {
    const std::size_t out0 = static_cast<std::size_t>(blk.out_tape_id_first);
    if (blk.a_ids.empty() || tape.is_na[out0]) return;
    const std::size_t n = blk.rows_a;
    const int ni = static_cast<int>(n);
    std::vector<double> R_bar, R_bar_dot;
    if (!block_adjoints(out0, n * n, R_bar, R_bar_dot)) return;
    // lower R is a structural zero
    for (std::size_t c = 0; c < n; c++) {
      for (std::size_t r = c + 1; r < n; r++) R_bar[c * n + r] = R_bar_dot[c * n + r] = 0.0;
    }
    std::vector<double> R, R_dot;
    block_outputs(out0, n * n, R, R_dot);

    std::vector<double> X(n * n, 0.0);
    gemm("N", "T", ni, ni, ni, R_bar.data(), ni, R.data(), ni, 0.0, X.data(), ni);
    phi_upper(X, n);
    trsm_upper("L", "N", ni, R.data(), X.data());
    trsm_upper("R", "T", ni, R.data(), X.data());

    std::vector<double> X_dot(n * n, 0.0);
    gemm("N", "T", ni, ni, ni, R_bar_dot.data(), ni, R.data(), ni, 0.0, X_dot.data(), ni);
    gemm("N", "T", ni, ni, ni, R_bar.data(), ni, R_dot.data(), ni, 1.0, X_dot.data(), ni);
    phi_upper(X_dot, n);
    trsm_upper("L", "N", ni, R.data(), X_dot.data());
    trsm_upper("R", "T", ni, R.data(), X_dot.data());
    std::vector<double> T1(n * n, 0.0), T2(n * n, 0.0);
    gemm("N", "N", ni, ni, ni, R_dot.data(), ni, X.data(), ni, 0.0, T1.data(), ni);
    trsm_upper("L", "N", ni, R.data(), T1.data());
    gemm("N", "T", ni, ni, ni, X.data(), ni, R_dot.data(), ni, 0.0, T2.data(), ni);
    trsm_upper("R", "T", ni, R.data(), T2.data());
    for (std::size_t k = 0; k < n * n; k++) X_dot[k] -= T1[k] + T2[k];

    add_side(blk.a_ids, fold_upper(X, n), fold_upper(X_dot, n));
  }

  // W = A^-T from the cached LU (empty for a singular A: no derivatives, as reverse())
  inline std::vector<double> inverse_transposed(const BlockOp& blk) const {
    const std::size_t n = blk.rows_a;
    std::vector<double> W(n * n, 0.0);
    for (std::size_t d = 0; d < n; d++) W[d * n + d] = 1.0;
    int ni = static_cast<int>(n);
    int info = 0;
    F77_CALL(dgetrs)("T", &ni, &ni, blk.lu.data(), &ni, blk.ipiv.data(), W.data(), &ni, &info FCONE);
    ass<"hessian: triangular solve failed">(info == 0);
    return W;
  }

  // d = det(A): d_dot = d <W, A_dot>, W = A^-T
  inline void det_forward(const BlockOp& blk, std::size_t i) {
    if (blk.lu.empty() || blk.rows_a == 0) return;
    std::vector<double> A, A_dot;
    gather(blk.a_ids, blk.a_vals, A, A_dot);
    const std::vector<double> W = inverse_transposed(blk);
    double s = 0.0;
    for (std::size_t k = 0; k < W.size(); k++) s += W[k] * A_dot[k];
    dot[i - start] = tape.val[i] * s;
  }

  // A_bar = w d W, A_bar_dot = (w_dot d + w d_dot) W + w d W_dot, W_dot = -W A_dot^T W
  inline void det_reverse(const BlockOp& blk, std::size_t i) {
    if (blk.lu.empty() || blk.rows_a == 0) return;
    const double w = adj[i - start];
    const double w_dot = adj_dot[i - start];
    if (w == 0.0 && w_dot == 0.0) return;
    const int ni = static_cast<int>(blk.rows_a);
    const double d = tape.val[i];
    const double d_dot = dot[i - start];
    std::vector<double> A, A_dot;
    gather(blk.a_ids, blk.a_vals, A, A_dot);
    const std::vector<double> W = inverse_transposed(blk);
    std::vector<double> T(W.size(), 0.0), W_dot(W.size(), 0.0);
    gemm("N", "T", ni, ni, ni, W.data(), ni, A_dot.data(), ni, 0.0, T.data(), ni);
    gemm("N", "N", ni, ni, ni, T.data(), ni, W.data(), ni, 0.0, W_dot.data(), ni, -1.0);
    std::vector<double> g(W.size()), g_dot(W.size());
    for (std::size_t k = 0; k < W.size(); k++) {
      g[k] = w * d * W[k];
      g_dot[k] = (w_dot * d + w * d_dot) * W[k] + w * d * W_dot[k];
    }
    add_side(blk.a_ids, g, g_dot);
  }

  // C = alpha op(A) op(B) + beta C, column-major
  static inline void gemm(const char* ta, const char* tb, int M, int N, int K,
                          const double* A, int lda, const double* B, int ldb,
                          double beta, double* C, int ldc, double alpha = 1.0) {
    if (M == 0 || N == 0) return;
    F77_CALL(dgemm)(ta, tb, &M, &N, &K, &alpha, A, &lda, B, &ldb, &beta, C, &ldc FCONE FCONE);
  }

  // solution X and its tangent from the block's output cells
  inline void block_outputs(std::size_t out0, std::size_t size,
                            std::vector<double>& X, std::vector<double>& X_dot) const {
    X.resize(size);
    X_dot.resize(size);
    for (std::size_t k = 0; k < size; k++) {
      X[k] = tape.val[out0 + k];
      X_dot[k] = dot[out0 + k - start];
    }
  }

  // solves op(A) Y = Y in place: LU of solve() or the triangle of backsolve / forwardsolve
  static inline void solve_in_place(const BlockOp& blk, const std::vector<double>& A,
                                    bool transposed, std::vector<double>& Y) {
    int n = static_cast<int>(blk.rows_a);
    int k = static_cast<int>(blk.cols_b);
    if (n == 0 || k == 0) return;
    if (blk.kind == BlockOp::Kind::Solve) {
      int info = 0;
      F77_CALL(dgetrs)(transposed ? "T" : "N", &n, &k, blk.lu.data(), &n, blk.ipiv.data(),
                       Y.data(), &n, &info FCONE);
      ass<"hessian: triangular solve failed">(info == 0);
    } else {
      const double one = 1.0;
      const bool upper = blk.kind == BlockOp::Kind::BackSolve;
      F77_CALL(dtrsm)("L", upper ? "U" : "L", transposed ? "T" : "N", "N", &n, &k, &one,
                      A.data(), &n, Y.data(), &n FCONE FCONE FCONE FCONE);
    }
  }

  // backsolve / forwardsolve read only one triangle of A
  static inline void mask_triangle(const BlockOp& blk, std::vector<double>& M) {
    if (blk.kind == BlockOp::Kind::Solve) return;
    const bool upper = blk.kind == BlockOp::Kind::BackSolve;
    const std::size_t n = blk.rows_a;
    for (std::size_t c = 0; c < n; c++) {
      for (std::size_t r = 0; r < n; r++) {
        if (upper ? (r > c) : (r < c)) M[c * n + r] = 0.0;
      }
    }
  }

  // NA in A or B poisons the whole solution: no tangent, no adjoints
  inline bool solve_is_na(const BlockOp& blk) const {
    return tape.is_na[static_cast<std::size_t>(blk.out_tape_id_first)] != 0;
  }

  // X = A^-1 B: X_dot = A^-1 (B_dot - A_dot X)
  inline void solve_forward(const BlockOp& blk) {
    if (solve_is_na(blk)) return;
    const int n = static_cast<int>(blk.rows_a);
    const int k = static_cast<int>(blk.cols_b);
    const std::size_t out0 = static_cast<std::size_t>(blk.out_tape_id_first);
    std::vector<double> A, A_dot, B, B_dot, X, X_dot;
    gather(blk.a_ids, blk.a_vals, A, A_dot);
    gather(blk.b_ids, blk.b_vals, B, B_dot);
    mask_triangle(blk, A_dot);
    block_outputs(out0, blk.rows_a * blk.cols_b, X, X_dot);
    std::vector<double> R = B_dot;
    gemm("N", "N", n, k, n, A_dot.data(), n, X.data(), n, 1.0, R.data(), n, -1.0);
    solve_in_place(blk, A, false, R);
    write_block_tangent(out0, R);
  }

  // G = A^-T X_bar,                    G_dot = A^-T (X_bar_dot - A_dot^T G)
  // B_bar += G,                        B_bar_dot += G_dot
  // A_bar += -G X^T,                   A_bar_dot += -(G_dot X^T + G X_dot^T)
  inline void solve_reverse(const BlockOp& blk) {
    if (solve_is_na(blk)) return;
    if (blk.a_ids.empty() && blk.b_ids.empty()) return;
    const int n = static_cast<int>(blk.rows_a);
    const int k = static_cast<int>(blk.cols_b);
    const std::size_t out0 = static_cast<std::size_t>(blk.out_tape_id_first);
    std::vector<double> G, G_dot;
    if (!block_adjoints(out0, blk.rows_a * blk.cols_b, G, G_dot)) return;
    std::vector<double> A, A_dot, X, X_dot;
    gather(blk.a_ids, blk.a_vals, A, A_dot);
    mask_triangle(blk, A_dot);
    block_outputs(out0, blk.rows_a * blk.cols_b, X, X_dot);
    solve_in_place(blk, A, true, G);
    gemm("T", "N", n, k, n, A_dot.data(), n, G.data(), n, 1.0, G_dot.data(), n, -1.0);
    solve_in_place(blk, A, true, G_dot);
    if (!blk.b_ids.empty()) add_side(blk.b_ids, G, G_dot);
    if (!blk.a_ids.empty()) {
      std::vector<double> g(blk.rows_a * blk.rows_a, 0.0), g_dot(g.size(), 0.0);
      gemm("N", "T", n, n, k, G.data(), n, X.data(), n, 0.0, g.data(), n, -1.0);
      gemm("N", "T", n, n, k, G_dot.data(), n, X.data(), n, 0.0, g_dot.data(), n, -1.0);
      gemm("N", "T", n, n, k, G.data(), n, X_dot.data(), n, 1.0, g_dot.data(), n, -1.0);
      mask_triangle(blk, g);
      mask_triangle(blk, g_dot);
      add_side(blk.a_ids, g, g_dot);
    }
  }

  // values and tangents of one block side; constants (vals) have tangent 0
  inline void gather(const std::vector<int>& ids, const std::vector<double>& vals,
                     std::vector<double>& v, std::vector<double>& v_dot) const {
    if (ids.empty()) {
      v = vals;
      v_dot.assign(vals.size(), 0.0);
      return;
    }
    v.resize(ids.size());
    v_dot.resize(ids.size());
    for (std::size_t k = 0; k < ids.size(); k++) {
      v[k] = value(ids[k]);
      v_dot[k] = tangent(ids[k]);
    }
  }

  // output adjoints (and their tangents) of a block; NA cells have no sensitivity
  inline bool block_adjoints(std::size_t out0, std::size_t size,
                             std::vector<double>& bar, std::vector<double>& bar_dot) const {
    bar.assign(size, 0.0);
    bar_dot.assign(size, 0.0);
    bool any = false;
    for (std::size_t k = 0; k < size; k++) {
      if (tape.is_na[out0 + k]) continue;
      bar[k] = adj[out0 + k - start];
      bar_dot[k] = adj_dot[out0 + k - start];
      any = any || bar[k] != 0.0 || bar_dot[k] != 0.0;
    }
    return any;
  }

  inline void add_side(const std::vector<int>& ids, const std::vector<double>& g,
                       const std::vector<double>& g_dot) {
    for (std::size_t k = 0; k < ids.size(); k++) add(ids[k], g[k], g_dot[k]);
  }

  // C = A B: C_dot = A_dot B + A B_dot
  inline void matmul_forward(const BlockOp& blk) {
    const int M = static_cast<int>(blk.rows_a);
    const int K = static_cast<int>(blk.cols_a);
    const int N = static_cast<int>(blk.cols_b);
    std::vector<double> A, A_dot, B, B_dot;
    gather(blk.a_ids, blk.a_vals, A, A_dot);
    gather(blk.b_ids, blk.b_vals, B, B_dot);
    std::vector<double> C_dot(blk.rows_a * blk.cols_b, 0.0);
    gemm("N", "N", M, N, K, A_dot.data(), M, B.data(), K, 0.0, C_dot.data(), M);
    gemm("N", "N", M, N, K, A.data(), M, B_dot.data(), K, 1.0, C_dot.data(), M);
    write_block_tangent(static_cast<std::size_t>(blk.out_tape_id_first), C_dot);
  }

  // A_bar = C_bar B^T,  A_bar_dot = C_bar_dot B^T + C_bar B_dot^T
  // B_bar = A^T C_bar,  B_bar_dot = A^T C_bar_dot + A_dot^T C_bar
  inline void matmul_reverse(const BlockOp& blk) {
    const int M = static_cast<int>(blk.rows_a);
    const int K = static_cast<int>(blk.cols_a);
    const int N = static_cast<int>(blk.cols_b);
    std::vector<double> C_bar, C_bar_dot;
    if (!block_adjoints(static_cast<std::size_t>(blk.out_tape_id_first), blk.rows_a * blk.cols_b, C_bar, C_bar_dot)) return;
    std::vector<double> A, A_dot, B, B_dot;
    gather(blk.a_ids, blk.a_vals, A, A_dot);
    gather(blk.b_ids, blk.b_vals, B, B_dot);
    if (!blk.a_ids.empty()) {
      std::vector<double> g(blk.rows_a * blk.cols_a, 0.0), g_dot(g.size(), 0.0);
      gemm("N", "T", M, K, N, C_bar.data(), M, B.data(), K, 0.0, g.data(), M);
      gemm("N", "T", M, K, N, C_bar_dot.data(), M, B.data(), K, 0.0, g_dot.data(), M);
      gemm("N", "T", M, K, N, C_bar.data(), M, B_dot.data(), K, 1.0, g_dot.data(), M);
      add_side(blk.a_ids, g, g_dot);
    }
    if (!blk.b_ids.empty()) {
      std::vector<double> g(blk.cols_a * blk.cols_b, 0.0), g_dot(g.size(), 0.0);
      gemm("T", "N", K, N, M, A.data(), M, C_bar.data(), M, 0.0, g.data(), K);
      gemm("T", "N", K, N, M, A.data(), M, C_bar_dot.data(), M, 0.0, g_dot.data(), K);
      gemm("T", "N", K, N, M, A_dot.data(), M, C_bar.data(), M, 1.0, g_dot.data(), K);
      add_side(blk.b_ids, g, g_dot);
    }
  }

  // crossprod S = X^T X: S_dot = X_dot^T X + X^T X_dot
  // tcrossprod S = X X^T: S_dot = X_dot X^T + X X_dot^T
  inline void crossprod_forward(const BlockOp& blk) {
    if (blk.a_ids.empty()) return;
    const bool tr = blk.kind == BlockOp::Kind::TCrossProd;
    const int m = static_cast<int>(blk.rows_a);
    const int n = static_cast<int>(blk.cols_a);
    const int d = tr ? m : n;
    std::vector<double> X, X_dot;
    gather(blk.a_ids, blk.a_vals, X, X_dot);
    std::vector<double> S_dot(static_cast<std::size_t>(d) * d, 0.0);
    if (tr) {
      gemm("N", "T", m, m, n, X_dot.data(), m, X.data(), m, 0.0, S_dot.data(), m);
      gemm("N", "T", m, m, n, X.data(), m, X_dot.data(), m, 1.0, S_dot.data(), m);
    } else {
      gemm("T", "N", n, n, m, X_dot.data(), m, X.data(), m, 0.0, S_dot.data(), n);
      gemm("T", "N", n, n, m, X.data(), m, X_dot.data(), m, 1.0, S_dot.data(), n);
    }
    write_block_tangent(static_cast<std::size_t>(blk.out_tape_id_first), S_dot);
  }

  // G = S_bar + S_bar^T, G_dot = S_bar_dot + S_bar_dot^T
  // crossprod:  X_bar = X G,  X_bar_dot = X_dot G + X G_dot
  // tcrossprod: X_bar = G X,  X_bar_dot = G X_dot + G_dot X
  inline void crossprod_reverse(const BlockOp& blk) {
    if (blk.a_ids.empty()) return;
    const bool tr = blk.kind == BlockOp::Kind::TCrossProd;
    const int m = static_cast<int>(blk.rows_a);
    const int n = static_cast<int>(blk.cols_a);
    const int d = tr ? m : n;
    const std::size_t dd = static_cast<std::size_t>(d) * d;
    std::vector<double> S_bar, S_bar_dot;
    if (!block_adjoints(static_cast<std::size_t>(blk.out_tape_id_first), dd, S_bar, S_bar_dot)) return;
    std::vector<double> G(dd), G_dot(dd);
    for (int j = 0; j < d; j++) {
      for (int i = 0; i < d; i++) {
        G[j * d + i] = S_bar[j * d + i] + S_bar[i * d + j];
        G_dot[j * d + i] = S_bar_dot[j * d + i] + S_bar_dot[i * d + j];
      }
    }
    std::vector<double> X, X_dot;
    gather(blk.a_ids, blk.a_vals, X, X_dot);
    std::vector<double> g(X.size(), 0.0), g_dot(X.size(), 0.0);
    if (tr) {
      gemm("N", "N", m, n, m, G.data(), m, X.data(), m, 0.0, g.data(), m);
      gemm("N", "N", m, n, m, G.data(), m, X_dot.data(), m, 0.0, g_dot.data(), m);
      gemm("N", "N", m, n, m, G_dot.data(), m, X.data(), m, 1.0, g_dot.data(), m);
    } else {
      gemm("N", "N", m, n, n, X.data(), m, G.data(), n, 0.0, g.data(), m);
      gemm("N", "N", m, n, n, X_dot.data(), m, G.data(), n, 0.0, g_dot.data(), m);
      gemm("N", "N", m, n, n, X.data(), m, G_dot.data(), n, 1.0, g_dot.data(), m);
    }
    add_side(blk.a_ids, g, g_dot);
  }

  inline void write_block_tangent(std::size_t out0, const std::vector<double>& out_dot) {
    for (std::size_t k = 0; k < out_dot.size(); k++) {
      dot[out0 + k - start] = tape.is_na[out0 + k] ? 0.0 : out_dot[k];
    }
  }

  // block heads carry the whole block (all output cells); slots are inert
  static inline bool is_block(ROp op) {
    return op == ROp::MatMul || op == ROp::BlockSlot || op == ROp::Chol || op == ROp::Solve ||
           op == ROp::CrossProd || op == ROp::TriSolve || op == ROp::Det;
  }

  inline bool block_forward(std::size_t i) {
    const ROp op = tape.op[i];
    if (!is_block(op)) return false;
    if (op == ROp::BlockSlot) return true;
    const BlockOp& blk = tape.blocks[static_cast<std::size_t>(tape.a[i])];
    switch (op) {
      case ROp::MatMul: matmul_forward(blk); break;
      case ROp::CrossProd: crossprod_forward(blk); break;
      case ROp::Chol: chol_forward(blk); break;
      case ROp::Det: det_forward(blk, i); break;
      default: solve_forward(blk);
    }
    return true;
  }

  inline bool block_reverse(std::size_t i) {
    const ROp op = tape.op[i];
    if (!is_block(op)) return false;
    if (op == ROp::BlockSlot) return true;
    const BlockOp& blk = tape.blocks[static_cast<std::size_t>(tape.a[i])];
    switch (op) {
      case ROp::MatMul: matmul_reverse(blk); break;
      case ROp::CrossProd: crossprod_reverse(blk); break;
      case ROp::Chol: chol_reverse(blk); break;
      case ROp::Det: det_reverse(blk, i); break;
      default: solve_reverse(blk);
    }
    return true;
  }

  inline void forward(const std::vector<int>& x_ids, const double* v) {
    std::fill(dot.begin(), dot.end(), 0.0);
    for (std::size_t k = 0; k < x_ids.size(); k++) dot[static_cast<std::size_t>(x_ids[k]) - start] = v[k];
    for (std::size_t i = start; i < end; i++) {
      if (block_forward(i)) continue;
      const ROp op = tape.op[i];
      if (op == ROp::Var || op == ROp::Const || tape.is_na[i]) continue;
      const int ai = tape.a[i];
      const int bi = tape.b[i];
      if (bi < 0) {
        double d1, d2;
        unary_derivatives(op, value(ai), tape.val[i], d1, d2);
        dot[i - start] = d1 * tangent(ai);
      } else {
        double da, db, daa, dab, dbb;
        binary_derivatives(op, value(ai), value(bi), tape.val[i], da, db, daa, dab, dbb);
        dot[i - start] = da * tangent(ai) + db * tangent(bi);
      }
    }
  }

  inline void reverse(int out) {
    std::fill(adj.begin(), adj.end(), 0.0);
    std::fill(adj_dot.begin(), adj_dot.end(), 0.0);
    add(out, 1.0, 0.0);
    for (std::size_t i = end; i-- > start;) {
      if (block_reverse(i)) continue;
      const ROp op = tape.op[i];
      if (op == ROp::Var || op == ROp::Const || tape.is_na[i]) continue;
      const double w = adj[i - start];
      const double w_dot = adj_dot[i - start];
      // NOTE: as reverse(): a node not reaching the output would add 0 * Inf = NaN
      if (w == 0.0 && w_dot == 0.0) continue;
      const int ai = tape.a[i];
      const int bi = tape.b[i];
      if (bi < 0) {
        double d1, d2;
        unary_derivatives(op, value(ai), tape.val[i], d1, d2);
        add(ai, w * d1, w_dot * d1 + w * d2 * tangent(ai));
      } else {
        double da, db, daa, dab, dbb;
        binary_derivatives(op, value(ai), value(bi), tape.val[i], da, db, daa, dab, dbb);
        const double ta = tangent(ai);
        const double tb = tangent(bi);
        add(ai, w * da, w_dot * da + w * (daa * ta + dab * tb));
        add(bi, w * db, w_dot * db + w * (dab * ta + dbb * tb));
      }
    }
  }
};

// records f(x) with ReverseDouble at the end of the tape, hands the sweep to body, rewinds
template<typename X, typename Call, typename Body>
inline auto with_recorded_scalar_f(const X& x, Call&& call_f, Body&& body) {
  auto& tape = TAPE_INTERN;
  const std::size_t tape_start = tape.size();
  const std::size_t blocks_start = tape.blocks.size();
  const std::size_t n = x.size();
  Array<ReverseDouble, Buffer<ReverseDouble>> xr(SI{n});
  for (std::size_t j = 0; j < n; j++) xr.set(j, ReverseDouble::Var(get_val(x.get(j))));
  const ReverseDouble y = call_f(xr);
  std::vector<int> x_ids(n);
  for (std::size_t j = 0; j < n; j++) x_ids[j] = xr.get(j).id;
  HessianSweep sweep(tape, tape_start);
  auto res = body(sweep, x_ids, y);
  tape.rewind(tape_start, blocks_start);
  return res;
}

template<typename X, typename Call>
inline auto hessian_impl(const X& x, Call&& call_f) {
  const std::size_t n = x.size();
  return with_recorded_scalar_f(x, call_f, [n](HessianSweep& sweep, const std::vector<int>& x_ids, const ReverseDouble& y) {
    auto H = matrix(Double(0.0), Integer(static_cast<int>(n)), Integer(static_cast<int>(n)));
    if (y.isNA()) {
      for (std::size_t k = 0; k < n * n; k++) H.set(k, Double::NA());
      return H;
    }
    std::vector<double> e(n, 0.0);
    for (std::size_t j = 0; j < n; j++) {
      e[j] = 1.0;
      sweep.forward(x_ids, e.data());
      sweep.reverse(y.id);
      e[j] = 0.0;
      for (std::size_t i = 0; i < n; i++) {
        H.set(j * n + i, Double(sweep.adj_dot[static_cast<std::size_t>(x_ids[i]) - sweep.start]));
      }
    }
    return H;
  });
}

template<typename X, typename V, typename Call>
inline auto hvp_impl(const X& x, const V& v, Call&& call_f) {
  const std::size_t n = x.size();
  ass<"hvp: x and v differ in length">(v.size() == n);
  std::vector<double> dir(n);
  for (std::size_t j = 0; j < n; j++) {
    ass<"hvp: NA in v">(!get_scalar_val(v.get(j)).isNA());
    dir[j] = static_cast<double>(get_val(v.get(j)));
  }
  return with_recorded_scalar_f(x, call_f, [n, &dir](HessianSweep& sweep, const std::vector<int>& x_ids, const ReverseDouble& y) {
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{n});
    res.dim = Dim{n};
    if (y.isNA()) {
      for (std::size_t i = 0; i < n; i++) res.set(i, Double::NA());
      return res;
    }
    sweep.forward(x_ids, dir.data());
    sweep.reverse(y.id);
    for (std::size_t i = 0; i < n; i++) {
      res.set(i, Double(sweep.adj_dot[static_cast<std::size_t>(x_ids[i]) - sweep.start]));
    }
    return res;
  });
}

// extra converted to the type f takes inside the recording, so its constants are rewound too
template<typename F, typename E>
inline auto call_with_extra(const F& f, const E& extra) {
  return [&f, &extra](const auto& xv) {
    if constexpr (requires { &F::operator(); }) {
      typename SecondArgOf<F>::type e;
      e = extra;
      return f(xv, e);
    } else {
      return f(xv, extra);
    }
  };
}

template<typename F, typename X> requires (IsArray<Decayed<X>>)
inline auto hessian(const F& f, const X& x) {
  return hessian_impl(x, [&f](const auto& xv) { return f(xv); });
}

template<typename F, typename X, typename E> requires (IsArray<Decayed<X>>)
inline auto hessian(const F& f, const X& x, const E& extra) {
  return hessian_impl(x, call_with_extra(f, extra));
}

template<typename F, typename X, typename V> requires (IsArray<Decayed<X>> && IsArray<Decayed<V>>)
inline auto hvp(const F& f, const X& x, const V& v) {
  return hvp_impl(x, v, [&f](const auto& xv) { return f(xv); });
}

template<typename F, typename X, typename V, typename E> requires (IsArray<Decayed<X>> && IsArray<Decayed<V>>)
inline auto hvp(const F& f, const X& x, const V& v, const E& extra) {
  return hvp_impl(x, v, call_with_extra(f, extra));
}

} // namespace etr

#endif
