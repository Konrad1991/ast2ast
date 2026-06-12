#ifndef DIAG_ETR_HPP
#define DIAG_ETR_HPP

namespace etr {

// calc dimension for diag.
template<typename A> inline auto calc_ncol_or_nrow(const A& obj) {
  if constexpr (IsArray<Decayed<A>>) {
    Integer i = obj.get(0);
    ass<"Found NA value in diag">(!i.isNA());
    return static_cast<std::size_t>(get_val(i));
  } else {
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
}

// ncol x ncol matrix, diagonal entry i set to value_at(i)
template<typename Type, typename F> inline auto make_diag(std::size_t nrow, std::size_t ncol, F value_at) {
  if constexpr (!IS<Type, ReverseDouble>) {
    Array<Type, Buffer<Type, RBufferTrait>> res(SI{nrow * ncol});
    res.dim = std::vector<std::size_t>{nrow, ncol};
    std::size_t iter = ncol < nrow ? ncol : nrow;
    for (std::size_t i = 0; i < iter; i++) {
      res.set(i * nrow + i, value_at(i));
    }
    return res;
  } else {
    Array<Type, Buffer<Type>> res(SI{nrow * ncol});
    res.dim = std::vector<std::size_t>{nrow, ncol};
    std::size_t iter = ncol < nrow ? ncol : nrow;
    for (std::size_t i = 0; i < iter; i++) {
      res.set(i * nrow + i, value_at(i));
    }
    return res;
  }
}

// Double
// x is scalar
template<typename RealType, typename A, typename B, typename C> requires IsScalarLike<Decayed<A>>
inline auto diag(const A& x, const B& nrow, const C& ncol) {
  const std::size_t nrow_ = calc_ncol_or_nrow(nrow);
  const std::size_t ncol_ = calc_ncol_or_nrow(ncol);
  constexpr bool is_numeric = IS<Decayed<A>, Double> || IS<Decayed<A>, Dual> || IS<Decayed<A>, ReverseDouble>;
  if constexpr (is_numeric) {
    return make_diag<RealType>(nrow_, ncol_,
                             [&](std::size_t i) { return x; }
                             );
  } else {
    return make_diag<Decayed<A>>(nrow_, ncol_,
                             [&](std::size_t i) { return x; }
                             );
  }

}
// x is Array
template<typename RealType, typename A, typename B, typename C> requires IsArray<Decayed<A>>
inline auto diag(const A& x, const B& nrow, const C& ncol) {
  const std::size_t nrow_ = calc_ncol_or_nrow(nrow);
  const std::size_t ncol_ = calc_ncol_or_nrow(ncol);

  using T = typename ExtractDataType<Decayed<A>>::value_type;
  constexpr bool is_numeric = IS<T, Double> || IS<T, Dual> || IS<T, ReverseDouble>;
  if constexpr (is_numeric) {
    return make_diag<RealType>(nrow_, ncol_,
                             [&](std::size_t i) { return x.get(safe_modulo(i, x.size())); }
                             );
  } else {
    return make_diag<T>(nrow_, ncol_,
                        [&](std::size_t i) { return x.get(safe_modulo(i, x.size())); }
                        );
  }
}

// extract the diagonal of a matrix as a vector
template<typename A> requires IsScalarLike<Decayed<A>>
inline auto get_diag(const A& scalar) {
  ass<"You cannot extract a diagonal from a scalar">(false);
  Array<Decayed<A>, Buffer<Decayed<A>, RBufferTrait>> res; // please the compiler
  return res;
}

template<typename A> requires IsArray<Decayed<A>>
inline auto get_diag(const A& arr) {
  using T = typename ExtractDataType<Decayed<A>>::value_type;
  const auto& dim = dim_view(arr.get_dim());
  ass<"Error in get_diag: argument is not a matrix">(dim.size() == 2);
  const std::size_t nr = dim[0];
  const std::size_t nc = dim[1];
  const std::size_t n = nc < nr ? nc : nr;
  if constexpr (!IS<T, ReverseDouble>) {
    Array<T, Buffer<T, RBufferTrait>> res(SI{n});
    res.dim = std::vector<std::size_t>{n};
    for (std::size_t i = 0; i < n; i++) res.set(i, arr.get(i + i * nr));
    return res;
  } else {
    // no RBufferTrait -> enforce copy; copied ReverseDouble aliases tape id, grad flows
    Array<T, Buffer<T>> res(SI{n});
    res.dim = std::vector<std::size_t>{n};
    for (std::size_t i = 0; i < n; i++) res.set(i, arr.get(i + i * nr));
    return res;
  }
}

} // namespace etr

#endif
