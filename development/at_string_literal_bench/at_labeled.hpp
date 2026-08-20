#ifndef AT_LABELED_BENCH_HPP
#define AT_LABELED_BENCH_HPP

// Same body/try-catch as at_tagged.hpp, but the R subexpression is an
// ordinary runtime const char* argument instead of a string_literal NTTP.
// Every call site shares one instantiation of at_labeled -- the label is
// just data, not part of the type.

namespace etr {

template <typename ArrayType, typename... Args>
requires (IsLBufferArray<ArrayType> || IsBorrowArray<ArrayType>)
inline decltype(auto) at_labeled(ArrayType& arr, const char* expr, const Args&... args) {
  try {
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
    ass<"Error: out of boundaries">(idx < arr.d.size());

    using DataType = typename ExtractDataType<Decayed<ArrayType>>::value_type;
    using ArrStorage = Decayed<decltype(arr.d)>;
    constexpr bool IsBuf = IsLBuffer<ArrStorage>;

    if constexpr (IsReverseDouble<DataType>) {
      return arr.d.get(idx);
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
  } catch (const std::exception& e) {
    throw std::runtime_error(std::string("In '") + expr + "': " + e.what());
  }
}

} // namespace etr

#endif
