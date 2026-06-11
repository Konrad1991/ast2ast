#ifndef SOLVE_ETR_HPP
#define SOLVE_ETR_HPP

// solve(A, B): X with A %*% X == B, via LAPACK LU (dgetrf + dgetrs).
//   solve(A)        -> A^-1            (B defaults to the identity, as in R)
//   solve(A, b)     -> A^-1 b          (b a vector -> result is a vector;
//                                       b an n x k matrix -> result n x k)
// A has to be a square matrix. Element-type dispatch mirrors MatMul/Chol:
//   ReverseDouble  -> reverse-mode block op (TAPE_INTERN.push_solve)
//   Dual           -> forward mode (LU factor reused for value and derivative)
//   anything else  -> plain double solve
// All buffers are column-major (R convention).

namespace etr {

// ---------- shape helpers --------------------------------------------------
// RHS B is n x k. A vector (rank < 2) is an n x 1 column; the result then
// drops back to a rank-1 vector, matching R.
inline std::pair<std::size_t, std::size_t> solve_rhs_shape(const std::vector<std::size_t>& dim, std::size_t sz) {
  if (dim.size() >= 2) return {dim[0], dim[1]};
  return {sz, 1};
}

// Column-major n x n identity.
inline std::vector<double> solve_identity(std::size_t n) {
  std::vector<double> I(n * n, 0.0);
  for (std::size_t j = 0; j < n; ++j) I[j * n + j] = 1.0;
  return I;
}

// ---------- LU helpers -----------------------------------------------------
// Factor a column-major n x n matrix in place; fills ipiv. Errors if singular.
inline void solve_lu_factor(std::vector<double>& A, std::size_t n, std::vector<int>& ipiv) {
  int ni = static_cast<int>(n), info = 0;
  ipiv.assign(n, 0);
  F77_CALL(dgetrf)(&ni, &ni, A.data(), &ni, ipiv.data(), &info);
  ass<"solve: matrix is exactly singular">(info == 0);
}
// Apply the LU factors to k right-hand sides held in B (overwritten in place).
inline void solve_lu_apply(const std::vector<double>& LU, std::size_t n, std::size_t k,
                           std::vector<int>& ipiv, std::vector<double>& B, const char* trans) {
  int ni = static_cast<int>(n), ki = static_cast<int>(k), info = 0;
  F77_CALL(dgetrs)(trans, &ni, &ki, LU.data(), &ni, ipiv.data(), B.data(), &ni, &info FCONE);
  ass<"solve: triangular solve failed">(info == 0);
}

// ---------- per-mode kernels (raw column-major buffers) --------------------
// Plain: X = A^-1 B.
inline auto solve_plain(std::vector<double> A, std::size_t n,
                        std::vector<double> X, std::size_t k, bool vec_result) {
  std::vector<int> ipiv;
  solve_lu_factor(A, n, ipiv);
  solve_lu_apply(A, n, k, ipiv, X, "N");

  Array<Double, Buffer<Double, RBufferTrait>> res(SI{n * k});
  std::copy(X.begin(), X.end(), res.d.p_val);
  if (vec_result) res.dim = {n};
  else            res.dim = {n, k};
  return res;
}

// Forward (Dual): value X = A^-1 B and directional derivative
// Xdot = A^-1 (Bdot - Adot X).
inline auto solve_forward(std::vector<double> A, std::vector<double> Adot, std::size_t n,
                          std::vector<double> X, std::vector<double> Bdot, std::size_t k,
                          bool vec_result) {
  std::vector<int> ipiv;
  solve_lu_factor(A, n, ipiv);          // A now holds the LU factors
  solve_lu_apply(A, n, k, ipiv, X, "N"); // X = A^-1 B

  // Xdot starts as Bdot - Adot X
  const int ni = static_cast<int>(n), ki = static_cast<int>(k);
  const double neg_one = -1.0, one = 1.0;
  std::vector<double> Xdot = Bdot;
  F77_CALL(dgemm)("N", "N", &ni, &ki, &ni, &neg_one,
                  Adot.data(), &ni, X.data(), &ni, &one,
                  Xdot.data(), &ni FCONE FCONE);
  solve_lu_apply(A, n, k, ipiv, Xdot, "N"); // Xdot = A^-1 (Bdot - Adot X)

  Array<Dual, Buffer<Dual, LBufferTrait>> res(SI{n * k});
  for (std::size_t i = 0; i < n * k; ++i) res.set(i, Dual(X[i], Xdot[i]));
  if (vec_result) res.dim = {n};
  else            res.dim = {n, k};
  return res;
}

// Reverse (ReverseDouble): one Solve block op on the tape. The backward pass
// (G = A^-T Xbar; Bbar += G; Abar += -G X^T) lives in ReverseTape::reverse().
inline auto solve_reverse(std::vector<int> a_ids, std::vector<double> a_vals, std::size_t n,
                          std::vector<int> b_ids, std::vector<double> b_vals, std::size_t k,
                          bool vec_result) {
  const int head = TAPE_INTERN.push_solve(n, k,
      std::move(a_ids), std::move(a_vals),
      std::move(b_ids), std::move(b_vals));
  Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> res(SI{n * k});
  if (vec_result) res.dim = {n};
  else            res.dim = {n, k};
  for (std::size_t i = 0; i < n * k; ++i) {
    const int id = head + static_cast<int>(i);
    const bool na = TAPE_INTERN.is_na[static_cast<std::size_t>(id)] != 0;
    res.set(i, ReverseDouble(from_tape_id_t{}, id, na));
  }
  return res;
}

// ---------- top-level entry: solve(A, B) -----------------------------------
template<typename A, typename B> requires(IsArray<Decayed<A>> && IsArray<Decayed<B>>)
inline auto solve(const A& a, const B& b) {
  using TA = typename ExtractDataType<Decayed<A>>::value_type;
  using TB = typename ExtractDataType<Decayed<B>>::value_type;
  using TC = common_type_t<TA, TB>;
  static_assert(!(IS<TA, Dual> && IS<TB, ReverseDouble>) && !(IS<TA, ReverseDouble> && IS<TB, Dual>),
                "solve: forward-mode (Dual) and reverse-mode (ReverseDouble) "
                "cannot be mixed in the same computation.");

  const auto& da = a.get_dim();
  ass<"solve: 'a' has to be a square matrix">(da.size() == 2 && da[0] == da[1]);
  const std::size_t n = da[0];

  const auto sb = solve_rhs_shape(b.get_dim(), b.size());
  ass<"solve: 'b' must have as many rows as 'a'">(sb.first == n);
  const std::size_t k = sb.second;
  const bool vec_result = b.get_dim().size() < 2;

  if constexpr (IS<TC, ReverseDouble>) {
    std::vector<int>    a_ids;  std::vector<double> a_vals;
    std::vector<int>    b_ids;  std::vector<double> b_vals;
    if constexpr (IS<TA, ReverseDouble>) a_ids = matmul_extract_ids(a); else a_vals = matmul_extract_dvals(a);
    if constexpr (IS<TB, ReverseDouble>) b_ids = matmul_extract_ids(b); else b_vals = matmul_extract_dvals(b);
    return solve_reverse(std::move(a_ids), std::move(a_vals), n,
                         std::move(b_ids), std::move(b_vals), k, vec_result);
  } else if constexpr (IS<TC, Dual>) {
    return solve_forward(matmul_extract_dvals(a), matmul_extract_dot(a), n,
                         matmul_extract_dvals(b), matmul_extract_dot(b), k, vec_result);
  } else {
    return solve_plain(matmul_extract_dvals(a), n, matmul_extract_dvals(b), k, vec_result);
  }
}

// ---------- top-level entry: solve(A) = A^-1 -------------------------------
template<typename A> requires(IsArray<Decayed<A>>)
inline auto solve(const A& a) {
  using TA = typename ExtractDataType<Decayed<A>>::value_type;
  const auto& da = a.get_dim();
  ass<"solve: 'a' has to be a square matrix">(da.size() == 2 && da[0] == da[1]);
  const std::size_t n = da[0];

  if constexpr (IS<TA, ReverseDouble>) {
    // B = identity, a constant (no gradient flows back through it)
    return solve_reverse(matmul_extract_ids(a), {}, n,
                         {}, solve_identity(n), n, /*vec_result=*/false);
  } else if constexpr (IS<TA, Dual>) {
    return solve_forward(matmul_extract_dvals(a), matmul_extract_dot(a), n,
                         solve_identity(n), std::vector<double>(n * n, 0.0), n,
                         /*vec_result=*/false);
  } else {
    return solve_plain(matmul_extract_dvals(a), n, solve_identity(n), n,
                       /*vec_result=*/false);
  }
}

}  // namespace etr

#endif
