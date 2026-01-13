#ifndef SUBSETTING_SIMPLIFYING_ETR_HPP
#define SUBSETTING_SIMPLIFYING_ETR_HPP

namespace etr {

template<typename T>
inline size_t ExtractIndex(const T& obj) {
  using DecayedT = Decayed<T>;
  if constexpr(IsArithV<DecayedT> || IsADType<DecayedT>) {
    static_assert(IsDouble<DecayedT> || IsInteger<DecayedT> || IsDual<DecayedT> || IsVariable<DecayedT>, "simplified can only handle integer or double values");
    constexpr bool is_double = IsDouble<DecayedT> || IsInteger<DecayedT> || IsDual<DecayedT>;
    if constexpr (!is_double) {
      return static_cast<std::size_t>(get_val(obj));
    } else if constexpr(is_double) {
      return safe_index_from_double(get_val(obj));
    }
  } else if constexpr (IsArray<DecayedT>) {
    constexpr bool is_double = IsDouble<DecayedT> || IsInteger<DecayedT> || IsDual<DecayedT>;
    ass<"at accepts only vector of length 1">(obj.size() == 1);
    if constexpr (!is_double) {
      return static_cast<std::size_t>(get_val(obj.get(0)));
    } else if constexpr(is_double) {
      return safe_index_from_double(get_val(obj.get(0)));
    }
  }
}

// Wrapper for borrow
// -----------------------------------------------------------------------------------------------------------
struct LogicalRef {
  bool* p;

  // implicit read
  operator Logical() const { return Logical(*p); }

  // assignment from scalar
  LogicalRef& operator=(const Logical& x) { *p = get_val(x); return *this; }
};
struct IntegerRef {
  int* p;

  // implicit read
  operator Integer() const { return Integer(*p); }

  // assignment from scalar
  IntegerRef& operator=(const Integer& x) { *p = get_val(x); return *this; }
};
struct DoubleRef {
  double* p;

  // implicit read
  operator Double() const { return Double(*p); }

  // assignment from scalar
  DoubleRef& operator=(const Double& x) { *p = get_val(x); return *this; }
};
struct DualRef {
  double* p_val;
  double* p_dot;

  // implicit read
  operator Dual() const { return Dual(*p_val, *p_dot); }

  // assignment from scalar
  DualRef& operator=(const Dual& x) { *p_val = get_val(x); *p_dot = x.dot; return *this; }
};


// direct access vector memory if possible.
// -----------------------------------------------------------------------------------------------------------
template <typename ArrayType, typename... Args>
requires (IsLBufferArray<ArrayType> || IsBorrowArray<ArrayType>)
inline decltype(auto) at(ArrayType& arr, const Args&... args) {
  constexpr std::size_t N = sizeof...(Args);
  const auto& dim = dim_view(arr.dim);
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
  if constexpr (IsLBufferArray<ArrayType>) {
    return arr.d.p[idx];
  } else if constexpr (IsBorrowArray<ArrayType>){
    using DataType = typename ExtractDataType<Decayed<ArrayType>>::value_type;
    if constexpr (IsDouble<DataType>) {
      return DoubleRef{ &arr.d.p[idx] };
    } else if constexpr (IsInteger<DataType>) {
      return IntegerRef{ &arr.d.p[idx] };
    } else if constexpr (IsLogical<DataType>) {
      return LogicalRef{ &arr.d.p[idx] };
    } else if constexpr (IsDual<DataType>) {
      return DualRef{ &arr.d.p_val[idx], &arr.d.p_dot[idx] };
    } else {
      ass<"Borrow at(): unsupported datatype">(false);
      return DoubleRef{nullptr}; // unreachable, just to satisfy compilers sometimes
    }
  }
}

// template <typename ArrayType, typename... Args>
// requires IsBorrowArray<ArrayType>
// inline auto at(ArrayType& arr, const Args&... args) {
//   return subset(arr, args...);
// }

template <typename ArrayType, typename... Args>
inline const auto at(const ArrayType& arr, const Args&... args) {
  constexpr std::size_t N = sizeof...(Args);
  const auto& dim = dim_view(arr.dim);
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
