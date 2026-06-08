#ifndef TRANSPOSE_ETR_HPP
#define TRANSPOSE_ETR_HPP

namespace etr {

template<typename A> requires(IsScalarLike<Decayed<A>>) inline auto transpose(const A& scalar) {
  ass<"You cannot transpose a scalar value">(false);
  Array<Decayed<A>, Buffer<Decayed<A>, RBufferTrait>> res; // please the compiler
  return res;
}

template<typename A> requires(IsArray<Decayed<A>>) inline auto transpose(const A& arr) {
  using Type = typename ExtractDataType<Decayed<A>>::value_type;
  const auto& dim = dim_view(arr.get_dim());

  if (dim.size() == 1) { // vector
    Array<Type, Buffer<Type, RBufferTrait>> res(SI{arr.size()});
    res.dim = std::vector<std::size_t>{1, arr.size()};
    for (std::size_t i = 0; i < arr.size(); i++) {
      res.set(i, arr.get(i));
    }
    return res;
  }
  else if (dim.size() == 2) { // matrix
    const std::size_t nr = dim[0];
    const std::size_t nc = dim[1];

    Array<Type, Buffer<Type, RBufferTrait>> res(SI{arr.size()});
    res.dim = std::vector<std::size_t>{nc, nr};

    for (std::size_t r = 0; r < nr; r++) {
      for (std::size_t c = 0; c < nc; c++) {
        res.set(r * nc + c, arr.get(r + c * nr));
      }
    }
    return res;
  }
  else {
    ass<"Error in t: Argument is not a matrix">(false);
    Array<Type, Buffer<Type, RBufferTrait>> res(SI{arr.size()}); // please the compiler
    return res;
  }
}

}

#endif
