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
// p_na is optional: Borrow-backed refs have no NA array, so they pass nullptr
// and NA info is not tracked. Buffer-backed refs pass the real pointer so NA
// survives through reads and writes.
struct LogicalRef {
  bool* p_val;
  bool* p_na;
  operator Logical() const {
    Logical out(*p_val);
    if (p_na) out.is_na = *p_na;
    return out;
  }
  // Explicit copy-assign: write through the referenced slot, do not rebind.
  // Without this the compiler-generated copy-assign copies pointers and
  // statements like `at(a, i) = at(a, j)` silently become no-ops.
  LogicalRef& operator=(const LogicalRef& other) {
    if (this == &other) return *this;
    *p_val = *other.p_val;
    if (p_na) *p_na = other.p_na ? *other.p_na : false;
    return *this;
  }
  LogicalRef& operator=(const Logical& x) {
    *p_val = get_val(x);
    if (p_na) *p_na = x.is_na;
    return *this;
  }
  template<typename T> requires (IsArithV<T> || IsADType<T>) LogicalRef& operator=(const T& x) {
    *p_val = static_cast<bool>(get_val(x));
    if (p_na) *p_na = get_scalar_val(x).is_na;
    return *this;
  }
  inline operator bool() const { return *p_val; }
  explicit inline LogicalRef(bool* v, bool* n = nullptr);
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
  bool* p_na;
  operator Integer() const {
    Integer out(*p_val);
    if (p_na) out.is_na = *p_na;
    return out;
  }
  IntegerRef& operator=(const IntegerRef& other) {
    if (this == &other) return *this;
    *p_val = *other.p_val;
    if (p_na) *p_na = other.p_na ? *other.p_na : false;
    return *this;
  }
  IntegerRef& operator=(const Integer& x) {
    *p_val = get_val(x);
    if (p_na) *p_na = x.is_na;
    return *this;
  }
  template<typename T> requires (IsArithV<T> || IsADType<T>) IntegerRef& operator=(const T& x) {
    *p_val = static_cast<int>(get_val(x));
    if (p_na) *p_na = get_scalar_val(x).is_na;
    return *this;
  }
  explicit inline IntegerRef(int* v, bool* n = nullptr);
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
  bool*   p_na;
  operator Double() const {
    Double out(*p_val);
    if (p_na) out.is_na = *p_na;
    return out;
  }
  DoubleRef& operator=(const DoubleRef& other) {
    if (this == &other) return *this;
    *p_val = *other.p_val;
    if (p_na) *p_na = other.p_na ? *other.p_na : false;
    return *this;
  }
  DoubleRef& operator=(const Double& x) {
    *p_val = get_val(x);
    if (p_na) *p_na = x.is_na;
    return *this;
  }
  template<typename T> requires (IsArithV<T> || IsADType<T>) DoubleRef& operator=(const T& x) {
    *p_val = static_cast<double>(get_val(x));
    if (p_na) *p_na = get_scalar_val(x).is_na;
    return *this;
  }
  explicit inline DoubleRef(double* v, bool* n = nullptr);
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
  bool*   p_na;
  bool*   p_na_dot;
  operator Dual() const {
    Dual out(*p_val, *p_dot);
    if (p_na)     out.is_na     = *p_na;
    if (p_na_dot) out.is_na_dot = *p_na_dot;
    return out;
  }
  DualRef& operator=(const DualRef& other) {
    if (this == &other) return *this;
    *p_val = *other.p_val;
    *p_dot = *other.p_dot;
    if (p_na)     *p_na     = other.p_na     ? *other.p_na     : false;
    if (p_na_dot) *p_na_dot = other.p_na_dot ? *other.p_na_dot : false;
    return *this;
  }
  DualRef& operator=(const Dual& x) {
    *p_val = get_val(x);
    *p_dot = x.dot;
    if (p_na)     *p_na     = x.is_na;
    if (p_na_dot) *p_na_dot = x.is_na_dot;
    return *this;
  }
  template<typename T> requires (IsArithV<T>) DualRef& operator=(const T& x) {
    *p_val    = static_cast<double>(get_val(x));
    *p_dot    = 0.0;
    if (p_na)     *p_na     = x.is_na;
    if (p_na_dot) *p_na_dot = false;
    return *this;
  }
  explicit inline DualRef(double* v, double* d, bool* n = nullptr, bool* nd = nullptr);
  inline DualRef(Dual) = delete;
  inline DualRef(double, double) = delete;
  template<typename T> requires IsArray<Decayed<T>> inline DualRef& operator=(const T& arr) {
    using  inner = typename ExtractDataType<T>::value_type;
    ass<"You cannot assign an array with length > 1 to a scalar variable">(arr.size() == 1);
    *this = arr.get(0);
    return *this;
  }
};

inline LogicalRef::LogicalRef(bool*   v, bool* n) : p_val(v), p_na(n) {}
inline IntegerRef::IntegerRef(int*    v, bool* n) : p_val(v), p_na(n) {}
inline DoubleRef ::DoubleRef (double* v, bool* n) : p_val(v), p_na(n) {}
inline DualRef   ::DualRef   (double* v, double* d, bool* n, bool* nd)
  : p_val(v), p_dot(d), p_na(n), p_na_dot(nd) {}

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

  if constexpr (IsVariable<DataType>) {
    return (arr.d.p[idx]);
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

    if constexpr (IsVariable<DataType>) {
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
