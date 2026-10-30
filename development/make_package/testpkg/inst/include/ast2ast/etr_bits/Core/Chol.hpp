#ifndef CHOL_ETR_HPP
#define CHOL_ETR_HPP

// chol(A): upper-triangular R with A = t(R) %*% R (R's convention), via LAPACK
// dpotrf with uplo "U" -- only the upper triangle of A is read, same as base R.
// A has to be a symmetric positive-definite square matrix.

namespace etr {

template<typename A> inline auto chol_plain(const A& a, std::size_t n) {
  Array<Double, Buffer<Double, RBufferTrait>> res(SI{n * n});
  const auto vals = matmul_extract_dvals(a);
  std::copy(vals.begin(), vals.end(), res.d.p_val);

  int ni = static_cast<int>(n);
  int info = 0;
  F77_CALL(dpotrf)("U", &ni, res.d.p_val, &ni, &info FCONE);
  // info > 0: leading minor of order `info` is not PD (info < 0 is an illegal
  // argument, which cannot happen with the fixed arguments used here)
  ass<"chol: the matrix is not positive definite">(info == 0);

  // dpotrf leaves the lower triangle untouched; zero it (col-major: col*n + row)
  for (std::size_t c = 0; c < n; ++c) {
    for (std::size_t r = c + 1; r < n; ++r) {
      res.d.p_val[c * n + r] = 0.0;
    }
  }
  res.dim = {n, n};
  return res;
}

// Forward mode (Dual): value R = chol(A), directional derivative
// Rdot = Phi_U(R^-T Adot R^-1) * R, where Adot is the dot part of A and Phi_U
// keeps the strictly-upper triangle and halves the diagonal.
template<typename A> inline auto chol_forward(const A& a, std::size_t n) {
  std::vector<double> R = matmul_extract_dvals(a); // overwritten with chol by dpotrf
  std::vector<double> P = matmul_extract_dot(a);   // Adot, then R^-T Adot R^-1

  int ni = static_cast<int>(n);
  int info = 0;
  F77_CALL(dpotrf)("U", &ni, R.data(), &ni, &info FCONE);
  ass<"chol: the matrix is not positive definite">(info == 0);
  for (std::size_t c = 0; c < n; ++c)
    for (std::size_t r = c + 1; r < n; ++r)
      R[c * n + r] = 0.0;

  const double one = 1.0, zero = 0.0;
  // P <- R^-T Adot : solve R^T Y = Adot ; then P <- Y R^-1 : solve P R = Y
  F77_CALL(dtrsm)("L", "U", "T", "N", &ni, &ni, &one, R.data(), &ni, P.data(), &ni FCONE FCONE FCONE FCONE);
  F77_CALL(dtrsm)("R", "U", "N", "N", &ni, &ni, &one, R.data(), &ni, P.data(), &ni FCONE FCONE FCONE FCONE);

  // Phi_U: strictly-lower -> 0, diagonal halved (column-major: index col*n + row)
  for (std::size_t c = 0; c < n; ++c) {
    P[c * n + c] *= 0.5;
    for (std::size_t r = c + 1; r < n; ++r) P[c * n + r] = 0.0;
  }

  std::vector<double> rdot(n * n, 0.0);
  F77_CALL(dgemm)("N", "N", &ni, &ni, &ni, &one, P.data(), &ni, R.data(), &ni, &zero, rdot.data(), &ni FCONE FCONE);

  Array<Dual, Buffer<Dual, LBufferTrait>> res(SI{n * n});
  for (std::size_t k = 0; k < n * n; ++k) res.set(k, Dual(R[k], rdot[k]));
  res.dim = {n, n};
  return res;
}

// Reverse mode (ReverseDouble): one Chol block op on the tape. The backward
// pass (reverse-Cholesky adjoint) lives in ReverseTape::reverse().
template<typename A> inline auto chol_reverse(const A& a, std::size_t n) {
  const int head = TAPE_INTERN.push_chol(n, matmul_extract_ids(a), {});
  Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> res(SI{n * n});
  res.dim = {n, n};
  for (std::size_t k = 0; k < n * n; ++k) {
    const int id = head + static_cast<int>(k);
    const bool na = TAPE_INTERN.is_na[static_cast<std::size_t>(id)] != 0;
    res.set(k, ReverseDouble(from_tape_id_t{}, id, na));
  }
  return res;
}

template<typename A> requires(IsArray<Decayed<A>>) inline auto chol(const A& a) {
  using T = typename ExtractDataType<Decayed<A>>::value_type;
  const auto& dim = a.get_dim();
  ass<"chol: argument has to be a square matrix">(dim.size() == 2 && dim[0] == dim[1]);
  const std::size_t n = dim[0];
  if constexpr (IS<T, ReverseDouble>) {
    return chol_reverse(a, n);
  } else if constexpr (IS<T, Dual>) {
    return chol_forward(a, n);
  } else {
    return chol_plain(a, n);
  }
}

} // namespace etr

#endif
