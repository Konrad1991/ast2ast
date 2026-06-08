#ifndef DIAG_ETR_HPP
#define DIAG_ETR_HPP

namespace etr {

// calc dimension for diag.
template<typename A> requires(IsScalarLike<Decayed<A>>) inline auto calc_ncol(const A& obj) {
  if constexpr (IS<Decayed<A>, Integer>)  {
    ass<"Found NA value in diag">(!obj.isNA());
    return static_cast<std::size_t>(get_val(obj));
  }
  else {
    ass<"Found NA value in diag">(!obj.isNA());
    Integer i = obj;
    return static_cast<std::size_t>(get_val(i));
  }
}

// ncol x ncol matrix, diagonal entry i set to value_at(i)
template<typename F> inline auto make_diag(std::size_t ncol, F value_at) {
  Array<Double, Buffer<Double, RBufferTrait>> res(SI{ncol * ncol});
  res.dim = std::vector<std::size_t>{ncol, ncol};
  for (std::size_t i = 0; i < ncol; i++) {
    res.set(i * ncol + i, value_at(i));
  }
  return res;
}

// scalar -> dimension, diagonal filled with 1.0 (R: diag(5) -> 5x5 identity)
template<typename A> requires(IsScalarLike<Decayed<A>>) inline auto diag(const A& scalar) {
  const std::size_t ncol = calc_ncol(scalar);
  return make_diag(ncol, [](std::size_t) { return Double(1.0); });
}

// single array. matrix -> extract diagonal (R: diag(M)); vector length 1 ->
// dimension/identity (R footgun); vector length > 1 -> values on diagonal
template<typename A> requires(IsArray<Decayed<A>>) inline auto diag(const A& arr) {
  const auto& dim = dim_view(arr.get_dim());
  if (dim.size() == 2) { // matrix: diagonal as a vector, column-major
    const std::size_t nr = dim[0];
    const std::size_t len = std::min(nr, dim[1]);
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{len});
    res.dim = std::vector<std::size_t>{len};
    for (std::size_t i = 0; i < len; i++) {
      res.set(i, arr.get(i + i * nr));
    }
    return res;
  }
  if (arr.size() == 1) {
    const std::size_t ncol = calc_ncol(arr.get(0));
    return make_diag(ncol, [](std::size_t) { return Double(1.0); });
  }
  return make_diag(arr.size(), [&](std::size_t i) { return arr.get(i); });
}

// two scalars: b defines the dimension, a is the value at the diagonal
template<typename A, typename B> requires(IsScalarLike<Decayed<A>> && IsScalarLike<Decayed<B>>)
inline auto diag(const A& scalar_a, const B& scalar_b) {
  const std::size_t ncol = calc_ncol(scalar_b);
  return make_diag(ncol, [&](std::size_t) { return scalar_a; });
}

// array & scalar: scalar defines the dimension, array recycled along the diagonal
template<typename A, typename B> requires(IsArray<Decayed<A>> && IsScalarLike<Decayed<B>>)
inline auto diag(const A& arr, const B& scalar_b) {
  const std::size_t ncol = calc_ncol(scalar_b);
  return make_diag(ncol, [&](std::size_t i) { return arr.get(safe_modulo(i, arr.size())); });
}

// scalar & array: array (length 1) defines the dimension, scalar is the diagonal value
template<typename A, typename B> requires(IsScalarLike<Decayed<A>> && IsArray<Decayed<B>>)
inline auto diag(const A& scalar_a, const B& arr_b) {
  ass<"found invalid dimension argument to diag. Expected a vector with length 1L">(arr_b.size() == 1);
  const std::size_t ncol = calc_ncol(arr_b.get(0));
  return make_diag(ncol, [&](std::size_t) { return scalar_a; });
}

// array & array: arr_b (length 1) defines the dimension, arr_a recycled along the diagonal
template<typename A, typename B> requires(IsArray<Decayed<A>> && IsArray<Decayed<B>>)
inline auto diag(const A& arr_a, const B& arr_b) {
  ass<"found invalid dimension argument to diag. Expected a vector with length 1L">(arr_b.size() == 1);
  const std::size_t ncol = calc_ncol(arr_b.get(0));
  return make_diag(ncol, [&](std::size_t i) { return arr_a.get(safe_modulo(i, arr_a.size())); });
}

} // namespace etr

#endif
