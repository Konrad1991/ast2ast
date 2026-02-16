#ifndef ALLOCATION_ETR_HPP
#define ALLOCATION_ETR_HPP

namespace etr {

// ---------------------------------------------------------------------
// Concatenate ---------------------------------------------------------
// ---------------------------------------------------------------------
inline auto determine_type(const auto &rest) {
  using restType = Decayed<decltype(rest)>;
  if constexpr (!IsArray<restType>) {
    return typename ReRef<decltype(rest)>::type{};
  } else {
    return typename ExtractDataType<restType>::value_type{};
  }
};

inline auto determine_type(const auto &first, const auto &rest) {
  using firstType = Decayed<decltype(first)>;
  using restType = Decayed<decltype(rest)>;
  if constexpr (!IsArray<firstType> && !IsArray<restType>) {
    return typename common_type<firstType, restType>::type{};
  }
  else if constexpr (IsArray<firstType> && !IsArray<restType>) {
    using firstInner = typename ExtractDataType<firstType>::value_type;
    return typename common_type<firstInner, restType>::type{};
  }
  else if constexpr (!IsArray<firstType> && IsArray<restType>) {
    using restInner = typename ExtractDataType<restType>::value_type;
    return typename common_type<firstType, restInner>::type{};
  }
  else if constexpr (IsArray<firstType> && IsArray<restType>) {
    using firstInner = typename ExtractDataType<firstType>::value_type;
    using restInner = typename ExtractDataType<restType>::value_type;
    return typename common_type<firstInner, restInner>::type{};
  }
}

inline auto determine_type(const auto &first, const auto &...rest) {
  using firstType = Decayed<decltype(first)>;
  using restType = decltype(determine_type(rest...));
  if constexpr (!IsArray<firstType> && !IsArray<restType>) {
    return typename common_type<firstType, restType>::type{};
  }
  else if constexpr (IsArray<firstType> && !IsArray<restType>) {
    using firstInner = typename ExtractDataType<firstType>::value_type;
    return typename common_type<firstInner, restType>::type{};
  }
  else if constexpr (!IsArray<firstType> && IsArray<restType>) {
    using restInner = typename ExtractDataType<restType>::value_type;
    return typename common_type<firstType, restInner>::type{};
  }
  else if constexpr (IsArray<firstType> && IsArray<restType>) {
    using firstInner = typename ExtractDataType<firstType>::value_type;
    using restInner = typename ExtractDataType<restType>::value_type;
    return typename common_type<firstInner, restInner>::type{};
  }
}

template <typename... Args> inline auto c(Args &&...args) {
  static_assert(sizeof...(Args) > 0,
  "\n\n[etr::c Error]\nYou cannot use c() without any arguments\n");

  using cType = decltype(determine_type(args...));
  int size = 0;

  forEachArg(
    [&](auto arg) {
      using testType = Decayed<decltype(arg)>;
      constexpr bool is_scalar = IsScalarLike<testType>;
      if constexpr (is_scalar) {
        size++;
      } else {
        size += arg.size();
      }
    },
    args...);

  Array<cType, Buffer<cType, RBufferTrait>> ret(SI{size});
  ret.dim = std::vector<std::size_t>{static_cast<std::size_t>(size)};
  std::size_t index = 0;

  forEachArg(
    [&](const auto &arg) {
      using testType = Decayed<decltype(arg)>;
      constexpr bool is_scalar = IsScalarLike<testType>;
      if constexpr (is_scalar) {
        if constexpr (IS<testType, cType>) {
          ret.set(index, arg);
        } else {
          ret.set(index, static_cast<cType>(arg));
        }
        index++;
      } else {
        using InnerType = typename ReRef<decltype(arg)>::type::value_type;
        if constexpr (IS<InnerType, cType>) {
          for (int i = 0; i < arg.size(); i++) {
            ret.set(index + i, arg.get(i));
          }
        } else {
          for (int i = 0; i < arg.size(); i++) {
            ret.set(index + i, static_cast<cType>(arg.get(i)));
          }
        }
        index += arg.size();
      }
    },
    args...);
  return ret;
}

// ---------------------------------------------------------------------
// Colon ---------------------------------------------------------------
// ---------------------------------------------------------------------
template<typename T>
inline auto ConvertValueColon(const T& obj) {
  using DecayedT = Decayed<T>;

  static_assert(!IsLogical<DecayedT>,
  "\n\n[etr::colon error]\n"
  "You passed a logical value to the ':' operator.\n"
  "This is not allowed in R and not supported here.\n"
  "Please use integer or double values only.\n\n");

  constexpr bool is_scalar = IsScalarLike<DecayedT>;

  if constexpr(is_scalar) {
    return obj;
  } else {
    ass<": accepts only vector of length 1">(obj.size() == 1);
    return obj.get(0);
  }
}

template <typename DataType, typename S, typename E> inline auto colonInternal(S& startScalar, E& endScalar) {
  auto start = get_val(startScalar);
  const auto end = get_val(endScalar);
  if (start < end) {
    std::size_t length = static_cast<std::size_t>(end - start + 1);
    ass<"invalid start or end values as argument to colon(:)">(length >= 1);
    Array<DataType, Buffer<DataType, RBufferTrait>> ret(SI{length});
    ret.dim = std::vector<std::size_t>{length};
    std::size_t counter = 0;
    while (start <= end) {
      ret.d.p[counter] = start;
      start++;
      counter++;
    }
    return ret;
  } else {
    std::size_t length = static_cast<std::size_t>(start - end + 1);
    ass<"invalid start or end values as argument to colon(:)">(length >= 1);
    Array<DataType, Buffer<DataType, RBufferTrait>> ret(SI{length});
    ret.dim = std::vector<std::size_t>{length};
    std::size_t counter = 0;
    while (end <= start) {
      ret.d.p[counter] = start;
      start--;
      counter++;
    }
    return ret;
  }
}

template <typename A, typename O>
inline auto colon(const A& start,const O& end) {
  auto s = ConvertValueColon(start);
  auto e = ConvertValueColon(end);
  using DataType = typename common_type<decltype(s), decltype(e)>::type;
  return colonInternal<DataType>(s, e);
}

// ---------------------------------------------------------------------
// rep -----------------------------------------------------------------
// ---------------------------------------------------------------------
template<typename T>
inline std::size_t ConvertTimesRep(const T& times) {
  using DecayedT = Decayed<T>;
  constexpr bool is_scalar = IsScalarLike<DecayedT>;
  if constexpr(is_scalar) {
    const auto v = get_val(times);
    std::size_t res = static_cast<std::size_t>(v);
    ass<"times in fct rep has to be a positive integer">(res >= 1);
    return res;
  } else {
    ass<"times in rep has to be a vector of length 1">(times.size() == 1);
    const auto v = get_val(times.get(0));
    std::size_t res = static_cast<std::size_t>(v);
    ass<"times in fct rep has to be a positive integer">(res >= 1);
    return res;
  }
}

template <typename L, typename R>
inline auto repInternal(const L &inp, const R& times) {
  if constexpr (!IsArray<L>) {
    std::size_t length = ConvertTimesRep(times);
    Array<L, Buffer<L, RBufferTrait>> ret(SI{length});
    for (std::size_t i = 0; i < ret.size(); i++) ret.set(i, inp);
    ret.dim = std::vector<std::size_t>{length};
    return ret;
  } else if constexpr (IsArray<L>) {
    std::size_t length = ConvertTimesRep(times) * inp.size();
    using DataType = typename ExtractDataType<Decayed<L>>::value_type;
    Array<DataType, Buffer<DataType, RBufferTrait>> ret(SI{length});
    std::size_t counter = 0;
    for (std::size_t i = 0; i < ret.size(); i++) {
      ret.set(i, inp.get(counter));
      counter++;
      if (counter >= inp.size())
        counter = 0;
    }
    return ret;
  }
}

template <typename L, typename R> inline auto rep(L &&inp, R &&times) {
  return repInternal(inp, times);
}

// ---------------------------------------------------------------------
// vector, matrix and array --------------------------------------------
// ---------------------------------------------------------------------
template<typename T>
inline std::size_t ConvertSizeVec(const T& s) {
  using DecayedT = Decayed<T>;
  constexpr bool is_scalar = IsScalarLike<DecayedT>;
  if constexpr(is_scalar) {
    const auto v = get_val(s);
    ass<"size in fct vector/logical/integer/numeric/matrix/array has to be a positive integer">(v >= 1);
    std::size_t res = static_cast<std::size_t>(v);
    return res;
  } else {
    ass<"size in fct vector/logical/integer/numeric/matrix/array has to be a vector of length 1">(s.size() == 1);
    const auto v = get_val(s.get(0));
    ass<"size in fct vector/logical/integer/numeric/matrix/array has to be a positive integer">(v >= 1);
    std::size_t res = static_cast<std::size_t>(v);
    return res;
  }
}

template <typename Type, typename T> inline auto createRVec(T s) {
  std::size_t size = ConvertSizeVec(s);
  ass<"invalid length argument">(size > 0);
  Array<Type, Buffer<Type, RBufferTrait>> res(SI{size});
  res.dim = std::vector<std::size_t>{size};
  return res;

}

template <typename T> inline auto logical(const T &inp) {
  return createRVec<Logical>(inp);
}
template <typename T> inline auto integer(const T &inp) {
  return createRVec<Integer>(inp);
}
template <typename T> inline auto numeric(const T &inp) {
  return createRVec<Double>(inp);
}
template <typename T> inline auto numeric_dual(const T &inp) {
  return createRVec<Dual>(inp);
}
template <typename T> inline auto numeric_rev_ad(const T &inp) {
  return createRVec<Variable<Double>>(inp);
}

template <typename Type, typename R, typename C> inline auto createRMat(const R& nrow, const C& ncol) {
  std::size_t nr = ConvertSizeVec(nrow);
  std::size_t nc = ConvertSizeVec(ncol);
  ass<"invalid length argument">((nr*nc)> 0);
  Array<Type, Buffer<Type, RBufferTrait>> res(SI{nc*nr});
  res.dim = std::vector<std::size_t>{nr, nc};
  return res;
}

template<typename T, typename R, typename C>
inline auto matrix(const T& inp, const R& nrow, const C& ncol) {
  std::size_t nr = ConvertSizeVec(nrow);
  std::size_t nc = ConvertSizeVec(ncol);
  using DecayedT = Decayed<T>;
  constexpr bool is_scalar = IsScalarLike<DecayedT>;
  if constexpr(is_scalar) {
    auto res = createRMat<Decayed<T>>(nrow, ncol);
    for (std::size_t i = 0; i < res.size(); i++) {
      res.set(i, inp);
    }
    return res;
  } else {
    using DataType = typename ExtractDataType<Decayed<T>>::value_type;
    auto res = createRMat<DataType>(nrow, ncol);
    ass<"Input for matrix does not match size (nrow * ncol)">(inp.size() == (nr * nc));
    for (std::size_t i = 0; i < res.size(); i++) {
      res.set(i, inp.get(i));
    }
    return res;
  }
}

template<typename Dim>
inline auto calc_dim(const Dim& dim) {
  using DecayedDim = Decayed<Dim>;
  constexpr bool is_scalar_dim = IsScalarLike<DecayedDim>;
  if constexpr (is_scalar_dim) {
    std::size_t size = ConvertSizeVec(dim);
    return std::vector<std::size_t>{size};
  } else {
    std::vector<std::size_t> dim_vec(dim.size());
    for (std::size_t i = 0; i < dim_vec.size(); i++) {
      dim_vec[i] = ConvertSizeVec(dim.get(i));
    }
    return dim_vec;
  }
}

template<typename T, typename Dim>
inline auto array(const T& inp, const Dim& dim_inp) {
  const auto dim = calc_dim(dim_inp);
  std::size_t size = 1; for (std::size_t i = 0; i < dim.size(); i++) {
    size *= dim[i];
  }
  using DecayedT = Decayed<T>;
  constexpr bool is_scalar = IsScalarLike<DecayedT>;
  if constexpr (is_scalar) {
    ass<"invalid length argument">(size > 0);
    Array<DecayedT, Buffer<DecayedT, RBufferTrait>> res(SI{size});
    for (std::size_t i = 0; i < res.size(); i++) {
      res.set(i, inp);
    }
    res.dim = std::move(dim);
    return res;
  } else {
    using DataType = typename ExtractDataType<Decayed<T>>::value_type;
    ass<"invalid length argument">(size > 0);
    Array<DataType, Buffer<DataType, RBufferTrait>> res(SI{size});
    ass<"Input for array does not match size (prod(dims))">(inp.size() == size);
    for (std::size_t i = 0; i < res.size(); i++) {
      res.set(i, inp.get(i));
    }
    res.dim = std::move(dim);
    return res;
  }

}

} // namespace etr
#endif
