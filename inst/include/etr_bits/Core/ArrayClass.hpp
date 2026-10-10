#ifndef BUFFER_ARRAY_ETR_HPP
#define BUFFER_ARRAY_ETR_HPP

namespace etr {

template <typename T>
concept IsSoAScalar = IsDouble<T> || IsInteger<T> || IsLogical<T>;

// NOTE: writes through __restrict pointers instead of temp.set(i, ...):
// the stores then can't alias scalars inside `src` (e.g. dx^2 in x / dx^2),
// so those are loaded once, not per element. n must fit the buffer.
template <typename T, typename Src>
inline void fill_soa(from_ast_scalar_t<T>* __restrict val, bool* __restrict na,
                     const Src& src, std::size_t n) {
  using SrcT = typename ReRef<Src>::type::value_type;
  for (std::size_t i = 0; i < n; i++) {
    T x;
    if constexpr (IS<SrcT, T>) {
      x = src.get(i);
    } else {
      x = cast_preserve_na<T>(src.get(i));
    }
    val[i] = x.val;
    na[i] = x.is_na;
  }
}

// temp.resize(n) + fill from src
template <typename T, typename Src>
inline void fill_temp(Buffer<T, LBufferTrait>& temp, const Src& src) {
  const std::size_t n = src.size();
  temp.resize(n);
  if constexpr (IsSoAScalar<T>) {
    fill_soa<T>(temp.p_val, temp.p_na, src, n);
  } else {
    using SrcT = typename ReRef<Src>::type::value_type;
    for (std::size_t i = 0; i < n; i++) {
      if constexpr (IS<SrcT, T>) {
        temp.set(i, src.get(i));
      } else {
        temp.set(i, cast_preserve_na<T>(src.get(i)));
      }
    }
  }
}

/*
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
----------------------------------L Buffer --------------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
*/
template<typename T> struct Array<T, Buffer<T, LBufferTrait>> {
  using Type = T;
  using DType = Buffer<T, LBufferTrait>;
  using value_type = T;
  Buffer<T, LBufferTrait> d;
  Buffer<T, LBufferTrait> temp;
  Dim dim;

  // ======================= internal methods =================================================
  decltype(auto) get(std::size_t idx) const { return d.get(idx); }
  void set(std::size_t idx, const value_type& val) { d.set(idx, val); }

  explicit operator bool() const {
    ass<"Error in if: the condition has length > 1">(this->size() == 1);
    return static_cast<bool>(get_val(d.get(0)));
  }
  explicit operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      return d.get(0);
    } else {
      return d.get(0);
    }
  }

  std::size_t size() const { return d.size(); }
  const Dim& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  template <typename OtherObj, typename DataTypeOtherObj>
  void copy_with_temp(const OtherObj& other_obj) {
    fill_temp(temp, other_obj);
  }
  template<typename T2>
  void assign(const T2& other_obj) {
    using DataTypeOtherArray = typename ReRef<decltype(other_obj)>::type::value_type;
    copy_with_temp<T2, DataTypeOtherArray>(other_obj);
    d.moveit(temp);
  }

  // ======================= Constructors ===================================================
  // empty array
  explicit Array() : d(), temp(), dim() {}

  // Scalar constructors
  Array(Logical b) : d(1), temp(1), dim(1, 1) { d.set(0, static_cast<T>(b));}
  Array(Integer sz) : d(1), temp(1), dim(1, 1) { d.set(0, static_cast<T>(sz));}
  Array(Double sz) : d(1), temp(1), dim(1, 1) { d.set(0, get_val(sz)); }
  Array(Dual sz) : d(1), temp(1), dim(1, 1) { d.set(0, static_cast<T>(sz));}
  Array(ReverseDouble sz) : d(1), temp(1), dim(1, 1) { d.set(0, sz); }

  // define Arraytor with specific size
  explicit Array(const SI &sz) : d(sz.sz), temp(sz.sz), dim(1, sz.sz) {}

  // Copy other Array with LBuffer
  Array(const Array& other) {
    assign(other);
    dim = other.get_dim();
  }
  // Move other Array e.g. vec = std::move(other_vec)
  Array(Array&& other) noexcept(std::is_nothrow_move_constructible_v<decltype(d)>)
  : d(std::move(other.d)), dim(std::move(other.get_dim())) {}

  // Copy view vectors: e.g. Array<double> a(sinus(c(1.1, 2.2)));
  template<typename T2, typename R2>
  requires(IsUnary<R2> || IsBinary<R2> || IsComparison<R2> || IsSubsetView<R2>)
  Array(const Array<T2, R2>& other) {
    assign(other);
    dim = other.get_dim();
  }

  // Materialize an owned copy from a borrowed array, e.g. returning a
  // borrowed parameter as an owned value.
  template<typename T2, typename R2>
  requires(IsBorrow<R2>)
  Array(const Array<T2, R2>& other) {
    assign(other);
    dim = other.get_dim();
  }

  // Example: Array<Double, Buffer<Double>> a(Array<Double, Buffer<Double, RBufferTrait>>(SI{3}));
  template<typename T2, typename R2>
  requires(IS<T, T2> && IsRArray<Array<T2, R2>>)
  Array(Array<T2, R2>&& other) {
    d.moveit(other.d);
    dim = std::move(other.get_dim());
  }
  // Example: Array<Double, Buffer<Double>> a(Array<Integer, Buffer<Integer, RBufferTrait>>(SI{3}));
  template<typename T2, typename R2>
  requires((!IS<T, T2>) && IsRArray<Array<T2, R2>>)
  Array(Array<T2, R2>&& other) {
    assign(other);
    dim = std::move(other.get_dim());
  }

  // Buffer
  explicit Array(Buffer<T, LBufferTrait>&& inp, const Dim& dim_) noexcept : d(std::move(inp)), temp(), dim(dim_) {}
  explicit Array(Buffer<T, LBufferTrait>& inp, const Dim& dim_) : d(inp), temp(), dim(dim_) {}
  explicit Array(Buffer<T, LBufferTrait>&& inp) noexcept : d(std::move(inp)), temp(), dim(1, d.size()) {}
  explicit Array(Buffer<T, LBufferTrait>& inp) : d(inp), temp(), dim(1, d.size()) {}

#ifdef STANDALONE_ETR
#else
  Dim get_dims_from_sexp(SEXP s) {
    SEXP dim = Rf_getAttrib(s, R_DimSymbol);
    if (dim == R_NilValue) {
      return Dim{
        static_cast<std::size_t>(Rf_length(s))
      };
    }
    int n = Rf_length(dim);
    int* p = INTEGER(dim);

    Dim dims(static_cast<std::size_t>(n), 0);
    for (int i = 0; i < n; ++i) {
      dims[i] = static_cast<std::size_t>(p[i]);
    }
    return dims;
  }
  Array(SEXP inp) : d(inp) {
    dim = std::move(get_dims_from_sexp(inp));
  }
#endif

  // Catches other stuff
  template<typename...Args> requires NoVectorDim<Args...>
  Array(Args...) {
    ass<"Constructor not supported">(sizeof(T) == 0);
  }

  // ======================= assignments =================================================
  // assign scalar values
  template <typename TD>
  requires IsScalarLike<TD>
  Array &operator=(const TD inp) {
    if constexpr (IS<TD, T>) {
      d.resize(1);
      d.set(0, inp);
      dim.resize(1);
      dim[0] = d.size();
      return *this;
    } else {
      d.resize(1);
      d.set(0, static_cast<T>(inp));
      dim.resize(1);
      dim[0] = d.size();
      return *this;
    }
  }

  // copy assignment for same type
  Array& operator=(const Array& other_obj) {
    assign(other_obj);
    dim = other_obj.get_dim();
    return *this;
  }

  // copy assignment LArrays and expressions
  template<typename T2>
  requires(!IsScalarLike<Decayed<T2>> && IsArray<Decayed<T2>>)
  Array &operator=(const T2& other_obj) {
    assign(other_obj);
    dim = other_obj.get_dim();
    return *this;
  }
  // move assignment. Swap resources e.g. Array<int> v; v = array(0, 1:3)
  template<typename T2, typename R2>
  requires(IS<T, T2> && IsRArray<Array<T2, R2>>)
  Array& operator=(Array<T2, R2>&& other_obj) {
    d.moveit(other_obj.d);
    dim = std::move(other_obj.get_dim());
    return *this;
  }

#ifdef STANDALONE_ETR
#else
  Array &operator=(SEXP s) {
    d.initSEXP(s);
    dim = get_dims_from_sexp(s);
    return *this;
  }
#endif
};

/*
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
----------------------------------R Buffer --------------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
*/
template<typename T> struct Array<T, Buffer<T, RBufferTrait>> {
  using Type = T;
  using DType = Buffer<T, RBufferTrait>;
  using value_type = T;
  Buffer<T, RBufferTrait> d;
  Dim dim;

  // ======================= internal methods =================================================
  void set(std::size_t idx, const T& val) { d.set(idx, val); }
  decltype(auto) get(std::size_t idx) const { return d.get(idx); }
  // grows from empty -- lets a result of runtime-determined length (e.g.
  // which()) be built without going through SI, which floors at size 1
  void push_back(const T& val) { d.push_back(val); }

  explicit operator bool() const {
    ass<"Error in if: the condition has length > 1">(this->size() == 1);
    return static_cast<bool>(get_val(d.get(0)));
  }
  explicit operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      return d.get(0);
    } else {
      return d.get(0);
    }
  }

  std::size_t size() const { return d.size(); }
  const Dim& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  // ======================= Constructors ===================================================
  explicit Array() : d(), dim(1, 0) {}
  explicit Array(const SI &sz) : d(sz.sz) {}

  // Copy other Array with RBuffer
  Array(const Array& other) noexcept(std::is_nothrow_move_constructible_v<decltype(d)>)
  : d(std::move(other.d)), dim(std::move(other.get_dim())) {}

  Array(Array&& other) noexcept(std::is_nothrow_move_constructible_v<decltype(d)>)
  : d(std::move(other.d)), dim(std::move(other.get_dim())) {}

  template<typename...Args> requires NoVectorDim<Args...>
  Array(Args...) {
    ass<"Constructor not supported">(sizeof(T) == 0);
  }

  // ======================= assignments =================================================
  template<typename Arg>
  Array& operator=(const Arg& a) {
    static_assert(
    sizeof(T) == 0,
    "\n\n\n"
    "[etr::assignment Error]\n"
    "You tried to assign to something without a variable-name calculation."
    "Expressions like: c(1, 2, 3) <- 4:6 are illegal"
    "\n\n\n"
  );
  }
  Array& operator=(const Array&) = delete;
  Array& operator=(Array&&) = delete;
};

/*
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
----------------------------------Borrow ----------------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
*/
template<typename T> requires (IsArithV<T> || IsReverseDouble<T>) struct Array<T, Borrow<T, BorrowTrait>> {
  using Type = T;
  using DType = Borrow<T, BorrowTrait>;
  using value_type = T;
  Borrow<T, BorrowTrait> d;
  Buffer<T, LBufferTrait> temp;
  Dim dim;

  // ======================= internal methods =================================================
  decltype(auto) get(std::size_t idx) const { return d.get(idx); }
  void set(std::size_t idx, const value_type& val) { d.set(idx, val); }

  explicit operator bool() const {
    ass<"Error in if: the condition has length > 1">(this->size() == 1);
    return static_cast<bool>(get_val(d.get(0)));
  }
  explicit operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      return d.get(0);
    } else {
      return d.get(0);
    }
  }

  std::size_t size() const { return d.size(); }
  const Dim& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  template <typename OtherObj, typename DataTypeOtherObj>
  void copy_with_temp(const OtherObj& other_obj) {
    fill_temp(temp, other_obj);
  }
  template<typename T2>
  void assign(const T2& other_obj) {
    using DataTypeOtherArray = typename ReRef<decltype(other_obj)>::type::value_type;
    copy_with_temp<T2, DataTypeOtherArray>(other_obj);
    const std::size_t n = other_obj.size();
    ass<"the borrowed region is too small for this assignment.">(n <= d.capacity);
    d.resize(n);
    for (std::size_t i = 0; i < n; i++) {
      d.set(i, temp.get(i));
    }
  }

  // ======================= Constructors ===================================================
  template<typename T2>
  explicit Array(T2 *ptr, std::size_t s, const Dim& dim_) : d(ptr, s), dim(dim_) {}

  template<typename T2>
  requires IsDual<T>
  explicit Array(T2 *ptr_val, T2* ptr_dot, std::size_t s, const Dim& dim_) : d(ptr_val, ptr_dot, s), dim(dim_) {}

#ifdef STANDALONE_ETR
#else
  Dim get_dims_from_sexp(SEXP s) {
    SEXP dim = Rf_getAttrib(s, R_DimSymbol);
    if (dim == R_NilValue) {
      return Dim{
        static_cast<std::size_t>(Rf_length(s))
      };
    }
    int n = Rf_length(dim);
    int* p = INTEGER(dim);

    Dim dims(static_cast<std::size_t>(n), 0);
    for (int i = 0; i < n; ++i) {
      dims[i] = static_cast<std::size_t>(p[i]);
    }
    return dims;
  }
  Array(SEXP inp) : d(inp) {
    dim = std::move(get_dims_from_sexp(inp));
  }
#endif

  template<typename...Args> requires NoVectorDim<Args...>
  Array(Args... args) {
    ass<"Constructor not supported">(sizeof(T) == 0);
  }
  // ======================= assignments =================================================
  // assign scalar values
  template <typename TD>
  requires IsScalarLike<TD>
  Array &operator=(const TD inp) {
    ass<"number of items to replace is not a multiple of replacement length">(1 <= d.capacity);
    if constexpr (IS<TD, T>) {
      d.resize(1);
      d.set(0, inp);
      dim.resize(1);
      dim[0] = d.size();
      return *this;
    } else {
      d.resize(1);
      d.set(0, static_cast<T>(inp));
      dim.resize(1);
      dim[0] = d.size();
      return *this;
    }
  }

  // copy assignment for same type
  Array& operator=(const Array& other_obj) {
    assign(other_obj);
    dim = other_obj.get_dim();
    return *this;
  }

  // copy assignment LArrays and expressions
  template<typename T2>
  requires(!IsScalarLike<Decayed<T2>> && IsArray<Decayed<T2>>)
  Array &operator=(const T2& other_obj) {
    assign(other_obj);
    dim = other_obj.get_dim();
    return *this;
  }
  // move assignment. but one has to copy --> due to Borrow
  template<typename T2, typename R2>
  requires(IS<T, T2> && IsRArray<Array<T2, R2>>)
  Array& operator=(Array<T2, R2>&& other_obj) {
    assign(other_obj);
    dim = std::move(other_obj.get_dim());
    return *this;
  }

#ifdef STANDALONE_ETR
#else
  Array &operator=(SEXP s) {
    d.initSEXP(s);
    dim = get_dims_from_sexp(s);
    return *this;
  }
#endif
};

/* ---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------- Unary Operation ------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
*/
template<typename T, typename I, typename Trait> struct Array<T, UnaryOperation<I, Trait>> {
  using Type = T;
  using DType = UnaryOperation<I, Trait>;
  using value_type = T;
  UnaryOperation<I, Trait> d;
  Dim dim;

  // ======================= internal methods =================================================
  decltype(auto) get(std::size_t idx) const { return d.get(idx); }
  void set(std::size_t idx, const value_type& val) { d.set(idx, val); }

  explicit operator bool() const {
    ass<"Error in if: the condition has length > 1">(this->size() == 1);
    return static_cast<bool>(get_val(d.get(0)));
  }
  explicit operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      return d.get(0);
    } else {
      return d.get(0);
    }
  }

  std::size_t size() const { return d.size(); }
  const Dim& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  template <typename L2, typename OperationTrait>
  explicit Array(UnaryOperation<L2, OperationTrait> &&inp, Dim&& dim_) : d(std::move(inp)), dim(std::move(dim_)) {}
  template <typename L2, typename OperationTrait>
  explicit Array(const UnaryOperation<L2, OperationTrait> &inp, const Dim& dim_) : d(inp), dim(dim_) {}
  template<typename...Args> requires NoVectorDim<Args...>
  Array(Args...) {
    ass<"Constructor not supported">(sizeof(T) == 0);
  }

  template<typename Arg>
  Array& operator=(const Arg& a) {
    static_assert(
    sizeof(T) == 0,
    "\n\n\n"
    "[etr::assignment Error]\n"
    "You tried to assign to something to a unary operation"
    "Expressions like: sin(c(1, 2, 3)) <- 5 are illegal"
    "\n\n\n"
  );
  }
  Array& operator=(const Array&) = delete;
  Array& operator=(Array&&) = delete;
};

/*
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
--------------------------------- Binary Operation ------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
*/
template<typename T, typename L, typename R, typename Trait> struct Array<T, BinaryOperation<L, R, Trait>> {
  using Type = T;
  using DType = BinaryOperation<L, R, Trait>;
  using value_type = T;
  BinaryOperation<L, R, Trait> d;
  Dim dim;

  // ======================= internal methods =================================================
  decltype(auto) get(std::size_t idx) const { return d.get(idx); }
  void set(std::size_t idx, const value_type& val) { d.set(idx, val); }

  explicit operator bool() const {
    ass<"Error in if: the condition has length > 1">(this->size() == 1);
    return static_cast<bool>(get_val(d.get(0)));
  }
  explicit operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      return d.get(0);
    } else {
      return d.get(0);
    }
  }

  std::size_t size() const { return d.size(); }
  const Dim& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  template <typename L2, typename R2, typename OperationTrait>
  explicit Array(BinaryOperation<L2, R2, OperationTrait> &&inp, Dim&& dim_) : d(std::move(inp)), dim(std::move(dim_)) {}
  template <typename L2, typename R2, typename OperationTrait>
  explicit Array(const BinaryOperation<L2, R2, OperationTrait> &inp, const Dim& dim_) : d(inp), dim(dim_) {}
  template<typename...Args> requires NoVectorDim<Args...>
  Array(Args...) {
    ass<"Constructor not supported">(sizeof(T) == 0);
  }

  template<typename Arg>
  Array& operator=(const Arg& a) {
    static_assert(
    sizeof(T) == 0,
    "\n\n\n"
    "[etr::assignment Error]\n"
    "You tried to assign to something to a binary operation"
    "Expressions like: 1:3 + 4:6 <- 5 are illegal"
    "\n\n\n"
  );
  }
  Array& operator=(const Array&) = delete;
  Array& operator=(Array&&) = delete;
};

/*
---------------------------------------------------------------------------------------------
--------------------------------- Range (a:b, seq_len) --------------------------------------
---------------------------------------------------------------------------------------------
More specialized than the BinaryOperation Array above. A range is a value
(start + RangeSpec), so unlike other expressions it can be default
constructed, copied and assigned. That lets a variable hold one (IntRange).
*/
template<typename T> struct Array<T, BinaryOperation<T, RangeSpec, RangeTrait>> {
  using Type = T;
  using DType = BinaryOperation<T, RangeSpec, RangeTrait>;
  using value_type = T;
  DType d;
  Dim dim;

  decltype(auto) get(std::size_t idx) const { return d.get(idx); }
  decltype(auto) get_unchecked(std::size_t idx) const { return d.get_unchecked(idx); }
  template<typename V> void set(std::size_t, const V&) = delete;

  explicit operator bool() const {
    ass<"Error in if: the condition has length > 1">(this->size() == 1);
    return static_cast<bool>(get_val(d.get(0)));
  }
  explicit operator value_type() const { return d.get(0); }

  std::size_t size() const { return d.size(); }
  const Dim& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  Array() : d(T{}, RangeSpec{1, 0}), dim(Dim{0}) {}
  explicit Array(DType&& inp, Dim&& dim_) : d(std::move(inp)), dim(std::move(dim_)) {}
  explicit Array(const DType& inp, const Dim& dim_) : d(inp), dim(dim_) {}
  Array(const Array& other) : d(other.d), dim(other.dim) {}
  Array(Array&& other) noexcept : d(std::move(other.d)), dim(std::move(other.dim)) {}

  Array& operator=(const Array& other) {
    d.l = other.d.l;
    d.r = other.d.r;
    dim = other.dim;
    return *this;
  }
  Array& operator=(Array&& other) noexcept {
    d.l = std::move(other.d.l);
    d.r = std::move(other.d.r);
    dim = std::move(other.dim);
    return *this;
  }
};
using IntRange = Array<Integer, BinaryOperation<Integer, RangeSpec, RangeTrait>>;

/*
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
----------------------------------SubsetView  -----------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
a <- array(1:36, c(4, 3, 3))
dim(a[1:3, 1:2, 1]) --> 3, 2
dim(a[1:2, 1, 1:3]) --> 2, 3
a[1:3, 1:2, 1] <- a[1:2, 1, 1:3]
Thus, only the size has to be the same, but not the dim?
*/
template<typename T, typename O, std::size_t N, typename Trait> struct Array<T, SubsetView<O, N, Trait>> {
  using Type = T;
  using DType = SubsetView<O, N, Trait>;
  using value_type = T;
  SubsetView<O, N, Trait> d;
  Buffer<T, LBufferTrait> temp;
  Dim dim;

  // ======================= internal methods =================================================
  decltype(auto) get(std::size_t idx) const { return d.get(idx); }
  void set(std::size_t idx, const value_type& val) { d.set(idx, val); }

  explicit operator bool() const {
    ass<"Error in if: the condition has length > 1">(this->size() == 1);
    return static_cast<bool>(get_val(d.get(0)));
  }
  explicit operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      return d.get(0);
    } else {
      return d.get(0);
    }
  }

  std::size_t size() const { return d.size(); }
  const Dim& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  template <typename OtherObj, typename DataTypeOtherObj>
  void copy_with_temp(const OtherObj& other_obj) {
    fill_temp(temp, other_obj);
  }
  template<typename T2>
  void assign(const T2& other_obj) {
    using DataTypeOtherArray = typename ReRef<decltype(other_obj)>::type::value_type;
    copy_with_temp<T2, DataTypeOtherArray>(other_obj);
    const std::size_t n = other_obj.size();
    ass<"number of items to replace is not a multiple of replacement length">(n == d.size());
    // TODO: can I safely move temp and d?
    for (std::size_t i = 0; i < n; i++) {
      d.set(i, temp.get(i));
    }
  }

  // ======================= Constructors ===================================================
  template <typename O2, std::size_t N2, typename Trait2>
  explicit Array(SubsetView<O2, N2, Trait2> &&inp, Dim&& dim_) : d(std::move(inp)), dim(std::move(dim_)) {}
  template <typename O2, std::size_t N2, typename Trait2>
  explicit Array(SubsetView<O2, N2, Trait2> &inp, Dim&& dim_) : d(inp), dim(std::move(dim_)) {}

  template<typename...Args> requires NoVectorDim<Args...>
  Array(Args...) {
    ass<"Constructor not supported">(sizeof(T) == 0);
  }

  // ======================= assignments =================================================
  // assign scalar values
  template <typename TD>
  requires IsScalarLike<TD>
  Array &operator=(const TD inp) {
    if constexpr (IS<TD, T>) {
      for (std::size_t i = 0; i < d.size(); ++i) {
        d.set(i, inp);
      }
    } else {
      T val = static_cast<T>(inp);
      for (std::size_t i = 0; i < d.size(); ++i) {
        d.set(i, val);
      }
    }
    return *this;
  }

  // copy assignment for same type
  Array& operator=(const Array& other_obj) {
    assign(other_obj);
    return *this;
  }

  // copy assignment LArrays and expressions
  template<typename T2>
  requires(!IsScalarLike<Decayed<T2>> && IsArray<Decayed<T2>>)
  Array &operator=(const T2& other_obj) {
    assign(other_obj);
    return *this;
  }
  // move assignment. Swap resources e.g. Array<int> v; v = array(0, 1:3)
  template<typename T2, typename R2>
  requires(IS<T, T2> && IsRArray<Array<T2, R2>>)
  Array& operator=(Array<T2, R2>&& other_obj) {
    assign(other_obj);
    return *this;
  }
};

/*
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
----------------------------------ConstSubsetView  ------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
*/
template<typename T, typename O, std::size_t N, typename Trait> struct Array<T, ConstSubsetView<O, N, Trait>> {
  using Type = T;
  using DType = ConstSubsetView<O, N, Trait>;
  using value_type = T;
  ConstSubsetView<O, N, Trait> d;
  Buffer<T, LBufferTrait> temp;
  Dim dim;

  // ======================= internal methods =================================================
  decltype(auto) get(std::size_t idx) const { return d.get(idx); }
  void set(std::size_t idx, const value_type& val) { d.set(idx, val); }

  explicit operator bool() const {
    ass<"Error in if: the condition has length > 1">(this->size() == 1);
    return static_cast<bool>(get_val(d.get(0)));
  }
  explicit operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      return d.get(0);
    } else {
      return d.get(0);
    }
  }

  std::size_t size() const { return d.size(); }
  const Dim& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  // ======================= Constructors ===================================================
  template <typename O2, std::size_t N2, typename Trait2>
  explicit Array(ConstSubsetView<O2, N2, Trait2> &&inp, Dim&& dim_) : d(std::move(inp)), dim(std::move(dim_)) {}
  template <typename O2, std::size_t N2, typename Trait2>
  explicit Array(ConstSubsetView<O2, N2, Trait2> &inp, Dim&& dim_) : d(inp), dim(std::move(dim_)) {}

  template<typename...Args> requires NoVectorDim<Args...>
  Array(Args...) {
    ass<"Constructor not supported">(sizeof(T) == 0);
  }
  // ======================= assignments =================================================
  template<typename Arg>
  Array& operator=(const Arg& a) {
    static_assert(
    sizeof(T) == 0,
    "\n\n\n"
    "[etr::assignment Error]\n"
    "You tried to assign something to a subset of a temporary unnamed object"
    "Expressions like: c(1, 2, 3)[1] <- 5 are illegal"
    "\n\n\n"
  );
  }
  Array& operator=(const Array&) = delete;
  Array& operator=(Array&&) = delete;
};

} // namespace etr

#endif
