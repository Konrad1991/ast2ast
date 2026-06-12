#ifndef CROSSPROD_ETR_HPP
#define CROSSPROD_ETR_HPP

// crossprod(X) = XᵀX  and  tcrossprod(X) = XXᵀ for ast2ast Arrays.
//
// Element-type dispatch mirrors mat_mul (see MatMul.hpp), reusing its
// extract / shape helpers:
//   ReverseDouble  → reverse-mode block op (TAPE_INTERN.push_crossprod)
//   Dual           → forward-mode dgemm pair
//   anything else  → plain dgemm, result Array<Double>
//
// Forward:   crossprod  S = XᵀX (n x n)        tcrossprod  S = XXᵀ (m x m)
// Backward:  Xbar = X (Sbar + Sbarᵀ)            Xbar = (Sbar + Sbarᵀ) X
// (standard matrix-AD result; the adjoint need not be symmetric, hence
// Sbar + Sbarᵀ rather than 2 Sbar). All data column-major.

namespace etr {

// 1-D (or scalar) input is a column vector (R convention: crossprod(v) = vᵀv).
inline std::pair<std::size_t, std::size_t> crossprod_shape_of(const std::vector<std::size_t>& dim, std::size_t sz) {
  if (dim.size() >= 2) return {dim[0], dim[1]};
  return {sz, 1};
}

// Plain: any combination of Logical / Integer / Double. Single dgemm.
template<typename A> inline auto crossprod_plain(const A& a, std::size_t m, std::size_t n, bool transposed) {
  auto x = matmul_extract_dvals(a);
  const std::size_t d = transposed ? m : n;
  Array<Double, Buffer<Double, RBufferTrait>> result(SI{d * d});
  const int mi = static_cast<int>(m);
  const int ni = static_cast<int>(n);
  const double one = 1.0, zero = 0.0;
  if (!transposed)
    F77_CALL(dgemm)("T", "N", &ni, &ni, &mi, &one, x.data(), &mi, x.data(), &mi, &zero, result.d.p_val, &ni FCONE FCONE);
  else
    F77_CALL(dgemm)("N", "T", &mi, &mi, &ni, &one, x.data(), &mi, x.data(), &mi, &zero, result.d.p_val, &mi FCONE FCONE);
  result.dim = {d, d};
  return result;
}

// Forward (Dual): S.val and S.dot = dXᵀX + XᵀdX (crossprod) / dXXᵀ + XdXᵀ.
template<typename A> inline auto crossprod_forward(const A& a, std::size_t m, std::size_t n, bool transposed) {
  auto x_vals = matmul_extract_dvals(a);
  auto x_dot  = matmul_extract_dot(a);
  const std::size_t d = transposed ? m : n;
  std::vector<double> s_vals(d * d, 0.0);
  std::vector<double> s_dot (d * d, 0.0);
  const int mi = static_cast<int>(m);
  const int ni = static_cast<int>(n);
  const double one = 1.0, zero = 0.0;
  if (!transposed) { // n x n, contraction m
    F77_CALL(dgemm)("T", "N", &ni, &ni, &mi, &one, x_vals.data(), &mi, x_vals.data(), &mi, &zero, s_vals.data(), &ni FCONE FCONE);
    F77_CALL(dgemm)("T", "N", &ni, &ni, &mi, &one, x_dot.data(),  &mi, x_vals.data(), &mi, &zero, s_dot.data(),  &ni FCONE FCONE);
    F77_CALL(dgemm)("T", "N", &ni, &ni, &mi, &one, x_vals.data(), &mi, x_dot.data(),  &mi, &one,  s_dot.data(),  &ni FCONE FCONE);
  } else {           // m x m, contraction n
    F77_CALL(dgemm)("N", "T", &mi, &mi, &ni, &one, x_vals.data(), &mi, x_vals.data(), &mi, &zero, s_vals.data(), &mi FCONE FCONE);
    F77_CALL(dgemm)("N", "T", &mi, &mi, &ni, &one, x_dot.data(),  &mi, x_vals.data(), &mi, &zero, s_dot.data(),  &mi FCONE FCONE);
    F77_CALL(dgemm)("N", "T", &mi, &mi, &ni, &one, x_vals.data(), &mi, x_dot.data(),  &mi, &one,  s_dot.data(),  &mi FCONE FCONE);
  }
  Array<Dual, Buffer<Dual, LBufferTrait>> result(SI{d * d});
  result.dim = {d, d};
  for (std::size_t k = 0; k < d * d; ++k) {
    Dual val;
    val.val       = s_vals[k];
    val.dot       = s_dot[k];
    val.is_na     = std::isnan(s_vals[k]);
    val.is_na_dot = std::isnan(s_dot[k]);
    result.set(k, val);
  }
  return result;
}

// Reverse (ReverseDouble): one block op on the tape.
template<typename A> inline auto crossprod_reverse(const A& a, std::size_t m, std::size_t n, bool transposed) {
  std::vector<int>    a_ids  = matmul_extract_ids(a);
  std::vector<double> a_vals; // ids path only — input is always ReverseDouble here
  const int leader = TAPE_INTERN.push_crossprod(m, n, std::move(a_ids), std::move(a_vals), transposed);

  const std::size_t d = transposed ? m : n;
  Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> result(SI{d * d});
  result.dim = {d, d};
  for (std::size_t k = 0; k < d * d; ++k) {
    const int id = leader + static_cast<int>(k);
    const bool na = TAPE_INTERN.is_na[static_cast<std::size_t>(id)] != 0;
    result.set(k, ReverseDouble(from_tape_id_t{}, id, na));
  }
  return result;
}

template<typename A> requires(IsArray<Decayed<A>>) inline auto crossprod_impl(const A& a, bool transposed) {
  using T = typename ExtractDataType<Decayed<A>>::value_type;
  const auto s = crossprod_shape_of(a.get_dim(), a.size());
  const std::size_t m = s.first;
  const std::size_t n = s.second;
  if      constexpr (IS<T, ReverseDouble>) return crossprod_reverse(a, m, n, transposed);
  else if constexpr (IS<T, Dual>)          return crossprod_forward(a, m, n, transposed);
  else                                      return crossprod_plain  (a, m, n, transposed);
}

template<typename A> requires(IsArray<Decayed<A>>) inline auto crossprod(const A& a)  { return crossprod_impl(a, false); }
template<typename A> requires(IsArray<Decayed<A>>) inline auto tcrossprod(const A& a) { return crossprod_impl(a, true); }

// scalar input: 1 x 1 (R: crossprod(5) -> 1x1)
template<typename A> requires(IsScalarLike<A>) inline auto crossprod(const A& a) {
  using S = bare_t<to_ast_scalar_t<A>>;
  Array<S, Buffer<S, LBufferTrait>> arr(SI{1});
  arr.set(0, a);
  return crossprod_impl(arr, false);
}
template<typename A> requires(IsScalarLike<A>) inline auto tcrossprod(const A& a) {
  using S = bare_t<to_ast_scalar_t<A>>;
  Array<S, Buffer<S, LBufferTrait>> arr(SI{1});
  arr.set(0, a);
  return crossprod_impl(arr, true);
}

} // namespace etr

#endif
