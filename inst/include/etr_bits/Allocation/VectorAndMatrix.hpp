#ifndef VECTOR_ETR_H
#define VECTOR_ETR_H

namespace etr {

template<typename T>
inline std::size_t ConvertSizeVec(const T& s) {
  using DecayedT = Decayed<T>;
  static_assert(IsCppFloat<DecayedT> || IsCppInt<DecayedT>,
  "size argument of vector/logical/integer/numeric has to be of type integer or double");
  if constexpr(IsArithV<DecayedT>) {
    if constexpr (IsCppInt<DecayedT>) {
      ass<"size in fct vector/logical/integer/numeric has to be a positive integer">(s >= 1);
      return static_cast<std::size_t>(s);
    } else {
      std::size_t s_ = static_cast<std::size_t>(std::floor(s));
      ass<"size in fct vector/logical/integer/numeric has to be a positive integer">(s_ >= 1);
      return s_;
    }
  } else {
    using Elem = typename ExtractDataType<DecayedT>::value_type;
    ass<"size in vector/logical/integer/numeric has to be a vector of length 1">(s.size() == 1);
    if constexpr (IsCppInt<Elem>) {
      ass<"size ... must be a positive integer">(s[0] >= 1);
      return static_cast<std::size_t>(s[0]);
    } else {
      const double v = std::floor(static_cast<double>(s[0]));
      ass<"size ... must be a positive integer">(v >= 1.0);
      return static_cast<std::size_t>(v);
    }
  }
}

template <typename Type, typename T> inline auto createRVec(T s) {
  std::size_t size = ConvertSizeVec(s);
  ass<"invalid length argument">(size > 0);
  return Vec<Type, Buffer<Type, RBufferTrait>>(SI{size});
}

template <typename T> inline auto logical(const T &inp) {
  return createRVec<bool>(inp);
}
template <typename T> inline auto integer(const T &inp) {
  return createRVec<int>(inp);
}
template <typename T> inline auto numeric(const T &inp) {
  return createRVec<double>(inp);
}

template <typename Type, typename R, typename C> inline auto createRMat(const R& nrow, const C& ncol) {
  std::size_t nr = ConvertSizeVec(nrow);
  std::size_t nc = ConvertSizeVec(ncol);
  ass<"invalid length argument">((nr*nc)> 0);
  return Mat<Type, Buffer<Type, RBufferTrait>>(SI{nr}, SI{nc});
}

template<typename T, typename R, typename C>
inline auto matrix(const T& inp, const R& nrow, const C& ncol) {
  std::size_t nr = ConvertSizeVec(nrow);
  std::size_t nc = ConvertSizeVec(ncol);
  if constexpr(IsArithV<T>) {
    auto res = createRMat<Decayed<T>>(nrow, ncol);
    for (std::size_t i = 0; i < res.size(); i++) {
      res[i] = inp;
    }
    return res;
  } else {
    using DataType = typename ExtractDataType<Decayed<T>>::value_type;
    auto res = createRMat<DataType>(nrow, ncol);
    ass<"Input for matrix does not match size (nrow * ncol)">(res.size() == (nr * nc));
    for (std::size_t i = 0; i < res.size(); i++) {
      res[i] = inp[i];
    }
    return res;
  }

}

} // namespace etr

#endif
