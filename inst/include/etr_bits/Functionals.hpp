#ifndef FUNCTIONALS_ETR_HPP
#define FUNCTIONALS_ETR_HPP

// map / Reduce / Filter / apply

namespace etr {

// return type of a callable: std::function directly, otherwise deduced from a
// non-overloaded operator() (an inline fn stringifies to a lambda with concrete
// parameter types, so &T::operator() is a plain member-function pointer).
template <typename T> struct return_type {
private:
  template <typename C, typename R, typename... A>
  static R ret_of(R (C::*)(A...));
  template <typename C, typename R, typename... A>
  static R ret_of(R (C::*)(A...) const);
public:
  using type = decltype(ret_of(&T::operator()));
};
template <typename R, typename... A> struct return_type<std::function<R(A...)>> {
  using type = R;
};
template <typename T> using return_type_t = typename return_type<Decayed<T>>::type;

template <typename... Args> inline std::size_t map_size(const Args &...args) {
  std::size_t n = 1;
  bool seen = false;
  forEachArg(
    [&](const auto &arg) {
      using T = Decayed<decltype(arg)>;
      if constexpr (!IsScalarLike<T>) {
        if (!seen) {
          n = arg.size();
          seen = true;
        } else {
          ass<"length mismatch between arguments in map">(arg.size() == n);
        }
      }
    },
    args...);
  return n;
}

template <typename Arg>
inline decltype(auto) map_at(const Arg &arg, std::size_t i) {
  if constexpr (IsScalarLike<Decayed<Arg>>) {
    return (arg);
  } else if constexpr (IsCollection<Decayed<Arg>>) {
    return (arg[i]); // Collection has no get(); operator[] is 0-based
  } else {
    return arg.get(i);
  }
}

template <typename F, typename First, typename... Rest>
inline auto map(const F &f, const First &first, const Rest &...rest) {
  using R = return_type_t<F>;
  const std::size_t n = map_size(first, rest...);
  if constexpr (IsScalarLike<Decayed<R>>) {
    Array<R, Buffer<R, RBufferTrait>> res(SI{n});
    res.dim = std::vector<std::size_t>{n};
    for (std::size_t i = 0; i < n; i++) {
      res.set(i, f(map_at(first, i), map_at(rest, i)...));
    }
    return res;
  } else if constexpr (IsArray<Decayed<R>>) {
    using Inner = typename ExtractDataType<Decayed<R>>::value_type;
    Array<Inner, Buffer<Inner, RBufferTrait>> res;
    std::vector<std::size_t> dim;
    for (std::size_t i = 0; i < n; i++) {
      R temp = f(map_at(first, i), map_at(rest, i)...);
      if (i == 0) {
        dim = temp.get_dim();
        res.d.resize(temp.size()*n);
        res.dim = dim;
        res.dim.push_back(n);
      } else {
        const auto temp_dim = temp.get_dim();
        ass<"map: results of the mapped function differ in rank">(temp_dim.size() == dim.size());
        for (std::size_t d = 0; d < dim.size() && d < temp_dim.size(); d++) {
          ass<"map: results of the mapped function differ in extent">(temp_dim[d] == dim[d]);
        }
      }
      for (std::size_t j = 0; j < temp.size(); j++) res.set(i * temp.size() + j, temp.get(j));
    }
    return res;
  } else { // is a new_type --> assured at the R side
    static_assert(
      !IsCollection<Decayed<R>>,
      "map: the mapped function may not return a collection - wrap it in a new_type"
    );
    Collection<R> res(n);
    for (std::size_t i = 0; i < n; i++) {
      res[i] = f(map_at(first, i), map_at(rest, i)...);
    }
    return res;
  }
}

template <typename F, typename X>
inline auto reduce(const F &f, const X &x) {
  using R = return_type_t<F>;
  const std::size_t n = x.size();
  ass<"Reduce: empty input">(n > 0);
  R acc = map_at(x, 0);
  for (std::size_t i = 1; i < n; i++) acc = f(acc, map_at(x, i));
  return acc;
}

template <typename F, typename X>
inline auto filter(const F &f, const X &x) {
  using XT = typename ExtractDataType<Decayed<X>>::value_type;
  const std::size_t n = x.size();
  Array<XT, Buffer<XT, RBufferTrait>> res;
  res.d.resize(n);
  std::size_t cnt = 0;
  for (std::size_t i = 0; i < n; i++) {
    if (static_cast<bool>(get_val(f(x.get(i))))) res.set(cnt++, x.get(i));
  }
  res.d.resize(cnt);
  res.dim = std::vector<std::size_t>{cnt};
  return res;
}

template<typename T> inline const int detect_apply_mode(const T& t) {
  if constexpr (IsScalarLike<Decayed<T>>) {
    std::size_t d = static_cast<std::size_t>(get_val(t));
    return d;
  } else if constexpr (IsArray<Decayed<T>>) {
    ass<"The Dim argument to apply has to be a scalar or a vector of length 1">(t.size() == 1);
    std::size_t d = static_cast<std::size_t>(get_val(t.get(0)));
    return d;
  } else {
    ass<"Found unsupported Dim argument to apply">(false);
    return -1; // Please compiler
  }
}

template <typename F, typename D, typename X>
requires (IsArray<Decayed<X>>)
inline auto apply(const F &f, const D &d, const X &x) {
  using XT = typename ExtractDataType<Decayed<X>>::value_type;
  using R = return_type_t<F>;
  const auto &dim = x.get_dim();
  ass<"apply: the third argument has to be a matrix">(dim.size() == 2);
  const int margin = detect_apply_mode(d);
  ass<"apply: MARGIN has to be 1 (rows) or 2 (columns)">(margin == 1 || margin == 2);
  const std::size_t nr = dim[0];
  const std::size_t nc = dim[1];
  const std::size_t n_slices  = (margin == 1) ? nr : nc;
  const std::size_t slice_len = (margin == 1) ? nc : nr;

  // element (r, c) of an nr x nc matrix is at flat index c*nr + r
  auto slice_at = [&](std::size_t s) {
    Array<XT, Buffer<XT, RBufferTrait>> out(SI{slice_len});
    out.dim = std::vector<std::size_t>{slice_len};
    for (std::size_t k = 0; k < slice_len; k++) {
      const std::size_t r = (margin == 1) ? s : k;
      const std::size_t c = (margin == 1) ? k : s;
      out.set(k, x.get(c * nr + r));
    }
    return out;
  };

  if constexpr (IsScalarLike<Decayed<R>>) {
    Array<R, Buffer<R, RBufferTrait>> res(SI{n_slices});
    res.dim = std::vector<std::size_t>{n_slices};
    for (std::size_t s = 0; s < n_slices; s++) res.set(s, f(slice_at(s)));
    return res;
  } else if constexpr (IsArray<Decayed<R>>) {
    using Inner = typename ExtractDataType<Decayed<R>>::value_type;
    Array<Inner, Buffer<Inner, RBufferTrait>> res;
    std::size_t klen = 0;
    for (std::size_t s = 0; s < n_slices; s++) {
      R tmp = f(slice_at(s));
      if (s == 0) {
        klen = tmp.size();
        res.d.resize(klen * n_slices);
        res.dim = std::vector<std::size_t>{klen, n_slices};
      } else {
        ass<"apply: f results differ in length">(tmp.size() == klen);
      }
      for (std::size_t k = 0; k < klen; k++) res.set(s * klen + k, tmp.get(k));
    }
    return res;
  } else {
    static_assert(IsScalarLike<Decayed<R>> || IsArray<Decayed<R>>,
      "apply: f has to return a scalar or a vector");
  }
}


} // namespace etr
#endif
