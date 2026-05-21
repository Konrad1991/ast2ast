#ifndef MATMUL_ETR_HPP
#define MATMUL_ETR_HPP

// Matrix multiplication for ast2ast Arrays.
//
// Element-type dispatch (via common_type_t<TA, TB>):
//   ReverseDouble  → reverse-mode block op (TAPE_INTERN.push_matmul)
//   Dual           → forward-mode, three F77_CALL(dgemm) calls
//   anything else  → plain F77_CALL(dgemm), result Array<Double>
//
// Logical / Integer inputs are cast to double at the boundary; mixing them
// with Double / Dual / ReverseDouble is handled uniformly by the same code
// path as the highest-ranked side.
//
// Shape rules — match R's %*% as closely as the project's Array model allows:
//   2-D Array (dim.size() == 2)        → use dim as-is, column-major
//   1-D Array on the LEFT              → 1 x N (row vector)
//   1-D Array on the RIGHT             → N x 1 (column vector)
//   scalar input (size == 1)           → 1 x 1
//
// All matrix data is column-major throughout (R convention).

namespace etr {

// ---------- shape helpers --------------------------------------------------
inline std::pair<std::size_t, std::size_t> matmul_shape_of(const std::vector<std::size_t>& dim, std::size_t sz, bool is_left) {
  if (sz == 1) return {1, 1};
  if (dim.size() >= 2) return {dim[0], dim[1]};
  return is_left ? std::pair<std::size_t, std::size_t>{1, sz}
                 : std::pair<std::size_t, std::size_t>{sz, 1};
}

// ---------- input extraction -----------------------------------------------
// Pull a contiguous double buffer (column-major) from any non-ReverseDouble
// Array. Logical / Integer get cast; NA values become NaN.
template<typename A> inline std::vector<double> matmul_extract_dvals(const A& arr) {
  std::vector<double> out(arr.size());
  for (std::size_t k = 0; k < arr.size(); ++k) {
    auto x = arr.get(k);
    if constexpr (requires { x.isNA(); }) {
      out[k] = x.isNA() ? std::numeric_limits<double>::quiet_NaN()
                        : static_cast<double>(get_val(x));
    } else {
      out[k] = static_cast<double>(x);
    }
  }
  return out;
}

// Pull a contiguous dot buffer from an Array<Dual>. For non-Dual element
// types, returns zeros (their dot is 0 by definition).
template<typename A> inline std::vector<double> matmul_extract_dot(const A& arr) {
  using T = typename ExtractDataType<Decayed<A>>::value_type;
  std::vector<double> out(arr.size(), 0.0);
  if constexpr (IS<T, Dual>) {
    for (std::size_t k = 0; k < arr.size(); ++k) {
      auto x = arr.get(k);
      out[k] = x.isNADot() ? std::numeric_limits<double>::quiet_NaN() : x.dot;
    }
  }
  return out;
}

// Pull tape ids from an Array<ReverseDouble> in column-major order.
template<typename A> inline std::vector<int> matmul_extract_ids(const A& arr) {
  std::vector<int> ids(arr.size());
  for (std::size_t k = 0; k < arr.size(); ++k) ids[k] = arr.get(k).id;
  return ids;
}

// ---------- per-mode kernels -----------------------------------------------
// Plain: any combination of Logical / Integer / Double. Single dgemm.
template<typename T> double* get_ptr(const T& obj) {
  static_assert(IS<typename ExtractDataType<Decayed<T>>::value_type, Double>,
                "get_ptr: only Double element type is supported");
  if constexpr (IsLBufferArray<Decayed<T>> || IsRArray<Decayed<T>>) return obj.d.p_val;
  else if constexpr (IsBorrowArray<Decayed<T>>) return obj.d.p;
}
template<typename A, typename B> inline auto matmul_plain(const A& a, const B& b, std::size_t M, std::size_t K, std::size_t N) {
  using A_SCALAR = typename ExtractDataType<Decayed<A>>::value_type;
  using B_SCALAR = typename ExtractDataType<Decayed<B>>::value_type;
  double* a_ptr = nullptr;
  double* b_ptr = nullptr;
  std::vector<double> temp_a;
  std::vector<double> temp_b;
  if constexpr (IS<A_SCALAR, Double>) {
    a_ptr = get_ptr(a);
  } else {
    temp_a = matmul_extract_dvals(a);
    a_ptr = temp_a.data();
  }
  if constexpr (IS<B_SCALAR, Double>) {
    b_ptr = get_ptr(b);
  } else {
    temp_b = matmul_extract_dvals(b);
    b_ptr = temp_b.data();
  }
  Array<Double, Buffer<Double, RBufferTrait>> result(SI{M * N});
  const int    Mi = static_cast<int>(M);
  const int    Ni = static_cast<int>(N);
  const int    Ki = static_cast<int>(K);
  const double one = 1.0, zero = 0.0;
  F77_CALL(dgemm)("N", "N", &Mi, &Ni, &Ki, &one,
                  a_ptr, &Mi, b_ptr, &Ki, &zero,
                  result.d.p_val, &Mi FCONE FCONE);
  result.dim = {M, N};
  return result;
}

// Forward (Dual): three dgemms.
//   C.val = A.val * B.val
//   C.dot = A.dot * B.val + A.val * B.dot
template<typename A, typename B> inline auto matmul_forward(const A& a, const B& b, std::size_t M, std::size_t K, std::size_t N) {
  auto a_vals = matmul_extract_dvals(a);
  auto b_vals = matmul_extract_dvals(b);
  auto a_dot  = matmul_extract_dot(a);
  auto b_dot  = matmul_extract_dot(b);

  std::vector<double> c_vals(M * N, 0.0);
  std::vector<double> c_dot (M * N, 0.0);

  const int    Mi = static_cast<int>(M);
  const int    Ni = static_cast<int>(N);
  const int    Ki = static_cast<int>(K);
  const double one = 1.0, zero = 0.0;

  F77_CALL(dgemm)("N", "N", &Mi, &Ni, &Ki, &one,
                  a_vals.data(), &Mi, b_vals.data(), &Ki, &zero,
                  c_vals.data(), &Mi FCONE FCONE);
  F77_CALL(dgemm)("N", "N", &Mi, &Ni, &Ki, &one,
                  a_dot.data(),  &Mi, b_vals.data(), &Ki, &zero,
                  c_dot.data(),  &Mi FCONE FCONE);
  F77_CALL(dgemm)("N", "N", &Mi, &Ni, &Ki, &one,
                  a_vals.data(), &Mi, b_dot.data(),  &Ki, &one,
                  c_dot.data(),  &Mi FCONE FCONE);

  Array<Dual, Buffer<Dual, LBufferTrait>> result(SI{M * N});
  result.dim = {M, N};
  for (std::size_t k = 0; k < M * N; ++k) {
    Dual d;
    d.val       = c_vals[k];
    d.dot       = c_dot[k];
    d.is_na     = std::isnan(c_vals[k]);
    d.is_na_dot = std::isnan(c_dot[k]);
    result.set(k, d);
  }
  return result;
}

// Reverse (ReverseDouble on at least one side): one block op on the tape.
template<typename A, typename B> inline auto matmul_reverse(const A& a, const B& b, std::size_t M, std::size_t K, std::size_t N) {
  using TA = typename ExtractDataType<Decayed<A>>::value_type;
  using TB = typename ExtractDataType<Decayed<B>>::value_type;

  std::vector<int>    a_ids;
  std::vector<double> a_vals;
  if constexpr (IS<TA, ReverseDouble>) a_ids  = matmul_extract_ids(a);
  else                                  a_vals = matmul_extract_dvals(a);

  std::vector<int>    b_ids;
  std::vector<double> b_vals;
  if constexpr (IS<TB, ReverseDouble>) b_ids  = matmul_extract_ids(b);
  else                                  b_vals = matmul_extract_dvals(b);

  const int leader = TAPE_INTERN.push_matmul(M, K, N,
      std::move(a_ids), std::move(a_vals),
      std::move(b_ids), std::move(b_vals));

  Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> result(SI{M * N});
  result.dim = {M, N};
  for (std::size_t k = 0; k < M * N; ++k) {
    const int id = leader + static_cast<int>(k);
    const bool na = TAPE_INTERN.is_na[static_cast<std::size_t>(id)] != 0;
    result.set(k, ReverseDouble(from_tape_id_t{}, id, na));
  }
  return result;
}

// ---------- top-level entry: Array x Array ---------------------------------
template<typename A, typename B> requires(IsArray<Decayed<A>> && IsArray<Decayed<B>>) inline auto mat_mul(const A& a, const B& b) {
  using TA = typename ExtractDataType<Decayed<A>>::value_type;
  using TB = typename ExtractDataType<Decayed<B>>::value_type;
  using TC = common_type_t<TA, TB>;
  static_assert(!(IS<TA, Dual> && IS<TB, ReverseDouble>) && !(IS<TA, ReverseDouble> && IS<TB, Dual>),
                "mat_mul: forward-mode (Dual) and reverse-mode (ReverseDouble) "
                "cannot be mixed in the same computation.");

  const auto sa = matmul_shape_of(a.get_dim(), a.size(), /*is_left=*/true);
  const auto sb = matmul_shape_of(b.get_dim(), b.size(), /*is_left=*/false);
  ass<"mat_mul: non-conformable arguments (cols(A) != rows(B))">(sa.second == sb.first);
  const std::size_t M = sa.first;
  const std::size_t K = sa.second;
  const std::size_t N = sb.second;

  if      constexpr (IS<TC, ReverseDouble>) return matmul_reverse(a, b, M, K, N);
  else if constexpr (IS<TC, Dual>)          return matmul_forward(a, b, M, K, N);
  else                                       return matmul_plain  (a, b, M, K, N);
}

// ---------- scalar overloads ----------------------------------------------
// In R, `5 %*% 3` is a 1x1 matrix and `5 %*% c(1,2,3)` is a 1x3 matrix.
// We mirror that by lifting scalar inputs into 1x1 Arrays and recursing.
template<typename L, typename R> requires(IsScalarLike<L> && IsArray<Decayed<R>>) inline auto mat_mul(const L& l, const R& r) {
  using LS = bare_t<to_ast_scalar_t<L>>;
  Array<LS, Buffer<LS, LBufferTrait>> larr(SI{1});
  larr.set(0, l);
  return mat_mul(larr, r);
}

template<typename L, typename R> requires(IsArray<Decayed<L>> && IsScalarLike<R>) inline auto mat_mul(const L& l, const R& r) {
  using RS = bare_t<to_ast_scalar_t<R>>;
  Array<RS, Buffer<RS, LBufferTrait>> rarr(SI{1});
  rarr.set(0, r);
  return mat_mul(l, rarr);
}

template<typename L, typename R> requires(IsScalarLike<L> && IsScalarLike<R>) inline auto mat_mul(const L& l, const R& r) {
  using LS = bare_t<to_ast_scalar_t<L>>;
  using RS = bare_t<to_ast_scalar_t<R>>;
  Array<LS, Buffer<LS, LBufferTrait>> larr(SI{1});
  larr.set(0, l);
  Array<RS, Buffer<RS, LBufferTrait>> rarr(SI{1});
  rarr.set(0, r);
  return mat_mul(larr, rarr);
}

}  // namespace etr

#endif
