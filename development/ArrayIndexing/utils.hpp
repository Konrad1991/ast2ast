
// https://ctrpeach.io/posts/cpp20-string-literal-template-parameters/
template <std::size_t N> struct string_literal {
  constexpr string_literal(const char (&str)[N]) {
    std::copy_n(str, N, value.begin());
  }
  std::array<char, N> value;
};
template <string_literal msg> inline void ass(bool inp) {
  if (!inp)
    throw std::runtime_error(msg.value.data());
}
inline std::size_t safe_index_from_double(double x) {
  ass<"invalid index argument">(std::isfinite(x));
  double t = std::trunc(x);
  ass<"Negative indices are not supported">(t >= 1);
  return static_cast<std::size_t>(t);
}

template <class F, class... Args> inline F forEachArg(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...); return f;
}

inline std::size_t safe_modulo(std::size_t idx, std::size_t sz) {
  return sz ? (idx % sz) : 0;  // no UB if n==0
}

template<std::size_t N>
inline std::array<std::size_t, N> make_strides(const std::array<std::size_t, N>& dim) {
  std::array<std::size_t, N> stride{};
  stride[0] = 1;
  for (std::size_t k = 1; k < N; k++) stride[k] = stride[k-1] * dim[k-1];
  return stride;
}

template<std::size_t N>
inline std::array<std::size_t, N> make_strides_from_vec(const std::vector<std::size_t>& dim) {
  ass<"Dimension mismatch in make_strides_from_vec">(dim.size() == N);
  std::array<std::size_t, N> stride{};
  stride[0] = 1;
  for (std::size_t k = 1; k < N; k++) stride[k] = stride[k-1] * dim[k-1];
  return stride;
}

