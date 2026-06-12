#ifndef TRISOLVE_ETR_HPP
#define TRISOLVE_ETR_HPP

// backsolve(R, b) / forwardsolve(L, b): solve a triangular system via BLAS
// dtrsm (no factorization — A is already triangular).
//   backsolve(R, b)     R upper-triangular, solves R x = b
//   forwardsolve(L, b)  L lower-triangular, solves L x = b
// b a vector -> result is a vector; b an n x k matrix -> result n x k.
// Triangularity is trusted (dtrsm ignores the other triangle), matching R.
// Element-type dispatch mirrors Solve.hpp:
//   ReverseDouble  -> reverse-mode block op (TAPE_INTERN.push_trisolve)
//   Dual           -> forward mode (two triangular solves)
//   anything else  -> plain double solve
// All buffers are column-major (R convention). Reuses Solve.hpp helpers.

namespace etr {

inline void trisolve_apply(std::vector<double>& A, std::size_t n, std::size_t k,
                           std::vector<double>& B, bool upper, const char* trans) {
  int ni = static_cast<int>(n), ki = static_cast<int>(k);
  const double one = 1.0;
  F77_CALL(dtrsm)("L", upper ? "U" : "L", trans, "N", &ni, &ki, &one,
                  A.data(), &ni, B.data(), &ni FCONE FCONE FCONE FCONE);
}

// Plain: X = A^-1 B.
inline auto trisolve_plain(std::vector<double> A, std::size_t n,
                           std::vector<double> X, std::size_t k, bool upper, bool vec_result) {
  trisolve_apply(A, n, k, X, upper, "N");
  Array<Double, Buffer<Double, RBufferTrait>> res(SI{n * k});
  std::copy(X.begin(), X.end(), res.d.p_val);
  if (vec_result) res.dim = {n};
  else            res.dim = {n, k};
  return res;
}

// Forward (Dual): X = A^-1 B and Xdot = A^-1 (Bdot - Adot X).
inline auto trisolve_forward(std::vector<double> A, std::vector<double> Adot, std::size_t n,
                             std::vector<double> X, std::vector<double> Bdot, std::size_t k,
                             bool upper, bool vec_result) {
  trisolve_apply(A, n, k, X, upper, "N"); // X = A^-1 B
  const int ni = static_cast<int>(n), ki = static_cast<int>(k);
  const double neg_one = -1.0, one = 1.0;
  std::vector<double> Xdot = Bdot;
  F77_CALL(dgemm)("N", "N", &ni, &ki, &ni, &neg_one,
                  Adot.data(), &ni, X.data(), &ni, &one,
                  Xdot.data(), &ni FCONE FCONE);
  trisolve_apply(A, n, k, Xdot, upper, "N"); // Xdot = A^-1 (Bdot - Adot X)

  Array<Dual, Buffer<Dual, LBufferTrait>> res(SI{n * k});
  for (std::size_t i = 0; i < n * k; ++i) res.set(i, Dual(X[i], Xdot[i]));
  if (vec_result) res.dim = {n};
  else            res.dim = {n, k};
  return res;
}

// Reverse (ReverseDouble): one TriSolve block op on the tape.
inline auto trisolve_reverse(std::vector<int> a_ids, std::vector<double> a_vals, std::size_t n,
                             std::vector<int> b_ids, std::vector<double> b_vals, std::size_t k,
                             bool upper, bool vec_result) {
  const int head = TAPE_INTERN.push_trisolve(n, k,
      std::move(a_ids), std::move(a_vals),
      std::move(b_ids), std::move(b_vals), upper);
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

template<typename A, typename B> requires(IsArray<Decayed<A>> && IsArray<Decayed<B>>)
inline auto trisolve(const A& a, const B& b, bool upper) {
  using TA = typename ExtractDataType<Decayed<A>>::value_type;
  using TB = typename ExtractDataType<Decayed<B>>::value_type;
  using TC = common_type_t<TA, TB>;
  static_assert(!(IS<TA, Dual> && IS<TB, ReverseDouble>) && !(IS<TA, ReverseDouble> && IS<TB, Dual>),
                "trisolve: forward-mode (Dual) and reverse-mode (ReverseDouble) "
                "cannot be mixed in the same computation.");

  const auto& da = a.get_dim();
  ass<"backsolve/forwardsolve: 'a' has to be a square matrix">(da.size() == 2 && da[0] == da[1]);
  const std::size_t n = da[0];

  const auto sb = solve_rhs_shape(b.get_dim(), b.size());
  ass<"backsolve/forwardsolve: 'b' must have as many rows as 'a'">(sb.first == n);
  const std::size_t k = sb.second;
  const bool vec_result = b.get_dim().size() < 2;

  if constexpr (IS<TC, ReverseDouble>) {
    std::vector<int>    a_ids;  std::vector<double> a_vals;
    std::vector<int>    b_ids;  std::vector<double> b_vals;
    if constexpr (IS<TA, ReverseDouble>) a_ids = matmul_extract_ids(a); else a_vals = matmul_extract_dvals(a);
    if constexpr (IS<TB, ReverseDouble>) b_ids = matmul_extract_ids(b); else b_vals = matmul_extract_dvals(b);
    return trisolve_reverse(std::move(a_ids), std::move(a_vals), n,
                            std::move(b_ids), std::move(b_vals), k, upper, vec_result);
  } else if constexpr (IS<TC, Dual>) {
    return trisolve_forward(matmul_extract_dvals(a), matmul_extract_dot(a), n,
                            matmul_extract_dvals(b), matmul_extract_dot(b), k, upper, vec_result);
  } else {
    return trisolve_plain(matmul_extract_dvals(a), n, matmul_extract_dvals(b), k, upper, vec_result);
  }
}

template<typename A, typename B> requires(IsArray<Decayed<A>> && IsArray<Decayed<B>>)
inline auto backsolve(const A& a, const B& b) { return trisolve(a, b, /*upper=*/true); }
template<typename A, typename B> requires(IsArray<Decayed<A>> && IsArray<Decayed<B>>)
inline auto forwardsolve(const A& a, const B& b) { return trisolve(a, b, /*upper=*/false); }

}  // namespace etr

#endif
