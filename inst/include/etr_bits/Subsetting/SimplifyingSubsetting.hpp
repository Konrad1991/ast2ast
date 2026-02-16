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

// Wrapper for borrow
// -----------------------------------------------------------------------------------------------------------
struct LogicalRef {
  bool* p_val;
  operator Logical() const { return Logical(*p_val); }
  LogicalRef& operator=(const Logical& x) {
    *p_val = get_val(x);
    return *this;
  }
  template<typename T> requires (IsArithV<T> || IsADType<T>) LogicalRef& operator=(const T& x) {
    *p_val = static_cast<bool>(get_val(x));
    return *this;
  }
  inline operator bool() const { return *p_val; }
  explicit inline LogicalRef(bool* v);
  inline LogicalRef(Logical) = delete;
  inline LogicalRef(bool) = delete;

  template<typename T> requires IsArray<Decayed<T>> inline LogicalRef& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    *this = arr.get(0);
    return *this;
  }
};

struct IntegerRef {
  int* p_val;
  operator Integer() const { return Integer(*p_val); }
  IntegerRef& operator=(const Integer& x) {
    *p_val = get_val(x);
    return *this;
  }
  template<typename T> requires (IsArithV<T> || IsADType<T>) IntegerRef& operator=(const T& x) {
    *p_val = static_cast<int>(get_val(x));
    return *this;
  }
  explicit inline IntegerRef(int* v);
  inline IntegerRef(Integer) = delete;
  inline IntegerRef(int) = delete;

  template<typename T> requires IsArray<Decayed<T>> inline IntegerRef& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    *this = arr.get(0);
    return *this;
  }
};
struct DoubleRef {
  double* p_val;
  operator Double() const { return Double(*p_val); }
  DoubleRef& operator=(const Double& x) {
    *p_val = get_val(x);
    return *this;
  }
  template<typename T> requires (IsArithV<T> || IsADType<T>) DoubleRef& operator=(const T& x) {
    *p_val = static_cast<double>(get_val(x));
    return *this;
  }
  explicit inline DoubleRef(double* v);
  inline DoubleRef(Double) = delete;
  inline DoubleRef(double) = delete;
  template<typename T> requires IsArray<Decayed<T>> inline DoubleRef& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    *this = arr.get(0);
    return *this;
  }
};

struct DualRef {
  double* p_val;
  double* p_dot;
  operator Dual() const { return Dual(*p_val, *p_dot); }
  DualRef& operator=(const Dual& x) {
    *p_val = get_val(x);
    *p_dot = x.dot;
    return *this;
  }
  template<typename T> requires (IsArithV<T>) DualRef& operator=(const T& x) {
    *p_val = static_cast<double>(get_val(x));
    return *this;
  }
  explicit inline DualRef(double* v, double* d);
  inline DualRef(Dual) = delete;
  inline DualRef(double, double) = delete;
  template<typename T> requires IsArray<Decayed<T>> inline DualRef& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    *this = arr.get(0);
    return *this;
  }
};

inline LogicalRef::LogicalRef(bool* v): p_val(v) {}
inline IntegerRef::IntegerRef(int* v) : p_val(v) {}
inline DoubleRef::DoubleRef(double* v): p_val(v) {}
inline DualRef::DualRef(double* v, double* d) : p_val(v), p_dot(d) {}

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
  ass<"No memory was allocated">(arr.d.allocated);
  ass<"Error: out of boundaries">( (idx < arr.d.sz) && (idx >= 0));
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

template <typename Obj> inline decltype(auto) at_linear(Obj& obj, std::size_t idx) {
  using ObjType = std::remove_cvref_t<Obj>;
  if constexpr (IsSubsetView<ObjType>) {
    const std::size_t parent_idx = obj.translate(idx);
    auto& parent = obj.obj.get();
    return at_linear(parent, parent_idx);

  } else if constexpr (IsLBuffer<ObjType>) {
    return obj.p[idx];
  } else if constexpr (IsBorrow<ObjType>) {
    using DataType = typename ExtractDataType<Decayed<ObjType>>::value_type;
    if constexpr (IsDouble<DataType>)   return DoubleRef{ &obj.p[idx] };
    if constexpr (IsInteger<DataType>)  return IntegerRef{ &obj.p[idx] };
    if constexpr (IsLogical<DataType>)  return LogicalRef{ &obj.p[idx] };
    if constexpr (IsDual<DataType>)     return DualRef{ &obj.p_val[idx], &obj.p_dot[idx] };
    ass<"Borrow at(): unsupported datatype">(false);
    return DoubleRef{nullptr}; // unreachable
  } else {
    ass<"at_linear(): unsupported storage type">(false);
    return DoubleRef{nullptr}; // unreachable
  }
}

template <typename ArrayType, typename... Args> requires IsSubsetArray<std::remove_reference_t<ArrayType>>
inline decltype(auto) at(ArrayType&& arr, const Args&... args) {
  constexpr std::size_t N = sizeof...(Args);
  const auto& dim = dim_view(arr.get_dim());

  if (N != dim.size()) {
    if (N > dim.size()) ass<"Too many index arguments for array rank">(false);
    else               ass<"Too less index arguments for array rank">(false);
  }

  int counter = 0;
  std::array<std::size_t, N> indices{};
  forEachArg([&](const auto& arg) {
      indices[counter++] = ExtractIndex(arg) - 1;
    }, args...);

  std::size_t idx = 0;
  auto stride = make_strides_from_vec<N>(dim);
  for (std::size_t i = 0; i < N; i++) {
    idx += indices[i] * stride[i];
  }

  ass<"Error: out of boundaries">(idx < arr.d.size());

  auto& obj = arr.d.obj.get();
  return at_linear(obj, idx);
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
