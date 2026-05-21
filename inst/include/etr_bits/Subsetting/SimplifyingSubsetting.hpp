#ifndef SUBSETTING_SIMPLIFYING_ETR_HPP
#define SUBSETTING_SIMPLIFYING_ETR_HPP

namespace etr {

template<typename T>
inline size_t ExtractIndex(const T& obj) {
  using DecayedT = Decayed<T>;

  if constexpr(IsScalarLike<DecayedT>) {
    const auto s = get_scalar_val(obj);
    using sType = Decayed<decltype(s)>;
    static_assert(!IsLogical<sType>, "simplified cannot handle logical values");
    constexpr bool is_int = IsInteger<sType>;
    if constexpr (is_int) {
      return static_cast<std::size_t>(get_val(s));
    } else if constexpr(!is_int) {
      return safe_index_from_double(get_val(s));
    }
  }

  else if constexpr (IsArray<DecayedT>) {
    using E = typename ExtractDataType<DecayedT>::value_type;
    static_assert(!IsLogical<E>, "simplified cannot handle logical values");
    constexpr bool is_int = IsInteger<E>;
    ass<"at accepts only vector of length 1">(obj.size() == 1);
    if constexpr (is_int) {
      return static_cast<std::size_t>(get_val(obj.get(0)));
    } else if constexpr(!is_int) {
      return safe_index_from_double(get_val(obj.get(0)));
    }
  }
}

// direct access vector memory if possible.
// -----------------------------------------------------------------------------------------------------------
template <typename ArrayType, typename... Args>
requires (IsLBufferArray<ArrayType> || IsBorrowArray<ArrayType>)
inline decltype(auto) at(ArrayType& arr, const Args&... args) {
  constexpr std::size_t N = sizeof...(Args);
  const auto& dim = dim_view(arr.get_dim());
  if (N > dim.size()) {
    ass<"Too many index arguments for array rank">(false);
  }
  if (N < dim.size()) {
    ass<"Too less index arguments for array rank">(false);
  }

  int counter = 0;
  std::array<std::size_t, N> indices{};
  forEachArg(
    [&](const auto& arg) {
      indices[counter++] = ExtractIndex(arg) - 1;
    },
    args...
  );

  std::size_t idx = 0;
  auto stride = make_strides_from_vec<N>(dim);
  for (std::size_t i = 0; i < N; i++) {
    idx += indices[i] * stride[i];
  }

  ass<"No memory was allocated">(arr.d.allocated);
  ass<"Error: out of boundaries">(idx < arr.d.sz);

  using DataType = typename ExtractDataType<Decayed<ArrayType>>::value_type;
  using ArrStorage = Decayed<decltype(arr.d)>;
  constexpr bool IsBuf = IsLBuffer<ArrStorage>;

  if constexpr (IsReverseDouble<DataType>) {
    return (arr.d.get(idx));
  } else if constexpr (IsDouble<DataType>) {
    if constexpr (IsBuf) return DoubleRef{ &arr.d.p_val[idx], &arr.d.p_na[idx] };
    else                 return DoubleRef{ &arr.d.p[idx], arr.d.p_na ? &arr.d.p_na[idx] : nullptr };
  } else if constexpr (IsInteger<DataType>) {
    if constexpr (IsBuf) return IntegerRef{ &arr.d.p_val[idx], &arr.d.p_na[idx] };
    else                 return IntegerRef{ &arr.d.p[idx], arr.d.p_na ? &arr.d.p_na[idx] : nullptr };
  } else if constexpr (IsLogical<DataType>) {
    if constexpr (IsBuf) return LogicalRef{ &arr.d.p_val[idx], &arr.d.p_na[idx] };
    else                 return LogicalRef{ &arr.d.p[idx], arr.d.p_na ? &arr.d.p_na[idx] : nullptr };
  } else if constexpr (IsDual<DataType>) {
    if constexpr (IsBuf)
      return DualRef{ &arr.d.p_val[idx], &arr.d.p_dot[idx],
                      &arr.d.p_na[idx],  &arr.d.p_na_dot[idx] };
    else
      return DualRef{ &arr.d.p_val[idx], &arr.d.p_dot[idx],
                      arr.d.p_na     ? &arr.d.p_na[idx]     : nullptr,
                      arr.d.p_na_dot ? &arr.d.p_na_dot[idx] : nullptr };
  } else {
    ass<"Borrow/Buffer at(): unsupported datatype">(false);
    return DoubleRef{nullptr};
  }
}

template <typename Obj>
inline decltype(auto) at_linear(Obj& obj, std::size_t idx) {
  using ObjType = std::remove_cvref_t<Obj>;

  if constexpr (IsSubsetView<ObjType>) {
    const std::size_t parent_idx = obj.translate(idx);
    auto& parent = obj.obj.get();
    return at_linear(parent, parent_idx);

  } else if constexpr (IsBorrow<ObjType> || IsLBuffer<ObjType>) {
    using DataType = typename ObjType::value_type;
    constexpr bool IsBuf = IsLBuffer<ObjType>;

    if constexpr (IsReverseDouble<DataType>) {
      return (obj.p[idx]);
    } else if constexpr (IsDouble<DataType>) {
      if constexpr (IsBuf) return DoubleRef{ &obj.p_val[idx], &obj.p_na[idx] };
      else                 return DoubleRef{ &obj.p[idx], obj.p_na ? &obj.p_na[idx] : nullptr };
    } else if constexpr (IsInteger<DataType>) {
      if constexpr (IsBuf) return IntegerRef{ &obj.p_val[idx], &obj.p_na[idx] };
      else                 return IntegerRef{ &obj.p[idx], obj.p_na ? &obj.p_na[idx] : nullptr };
    } else if constexpr (IsLogical<DataType>) {
      if constexpr (IsBuf) return LogicalRef{ &obj.p_val[idx], &obj.p_na[idx] };
      else                 return LogicalRef{ &obj.p[idx], obj.p_na ? &obj.p_na[idx] : nullptr };
    } else if constexpr (IsDual<DataType>) {
      if constexpr (IsBuf)
        return DualRef{ &obj.p_val[idx], &obj.p_dot[idx],
                        &obj.p_na[idx],  &obj.p_na_dot[idx] };
      else
        return DualRef{ &obj.p_val[idx], &obj.p_dot[idx],
                        obj.p_na     ? &obj.p_na[idx]     : nullptr,
                        obj.p_na_dot ? &obj.p_na_dot[idx] : nullptr };
    } else {
      ass<"at_linear(): unsupported datatype">(false);
      return DoubleRef{nullptr};
    }

  } else {
    ass<"at_linear(): unsupported storage type">(false);
    return DoubleRef{nullptr};
  }
}

template <typename ArrayType, typename... Args>
requires IsSubsetArray<std::remove_reference_t<ArrayType>>
inline decltype(auto) at(ArrayType&& arr, const Args&... args) {
  constexpr std::size_t N = sizeof...(Args);
  const auto& dim = dim_view(arr.get_dim());

  if (N != dim.size()) {
    if (N > dim.size()) ass<"Too many index arguments for array rank">(false);
    else               ass<"Too less index arguments for array rank">(false);
  }

  int counter = 0;
  std::array<std::size_t, N> indices{};
  forEachArg(
    [&](const auto& arg) {
      indices[counter++] = ExtractIndex(arg) - 1;
    },
    args...
  );

  std::size_t idx = 0;
  auto stride = make_strides_from_vec<N>(dim);
  for (std::size_t i = 0; i < N; i++) {
    idx += indices[i] * stride[i];
  }

  ass<"Error: out of boundaries">(idx < arr.d.size());
  return at_linear(arr.d, idx);
}

template <typename ArrayType, typename... Args>
inline const auto at(const ArrayType& arr, const Args&... args) {
  constexpr std::size_t N = sizeof...(Args);
  const auto& dim = dim_view(arr.get_dim());
  if (N > dim.size()) {
    ass<"Too many index arguments for array rank">(false);
  }
  if (N < dim.size()) {
    ass<"Too less index arguments for array rank">(false);
  }

  int counter = 0;
  std::array<std::size_t, N> indices;
  forEachArg(
    [&](const auto& arg) {
      indices[counter++] = ExtractIndex(arg) - 1;
    },
    args...
  );
  std::size_t idx = 0;
  auto stride = make_strides_from_vec<N>(dim);
  for (std::size_t i = 0; i < N; i++) {
    idx += indices[i] * stride[i];
  }
  return arr.get(idx);
}

} // namespace etr

#endif
