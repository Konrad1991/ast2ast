#ifndef BUFFER_ARRAY_ETR_HPP
#define BUFFER_ARRAY_ETR_HPP

namespace etr {

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
  std::vector<std::size_t> dim;

  // ======================= internal methods =================================================
  decltype(auto) operator[](std::size_t idx) { return d[idx]; }
  decltype(auto) operator[](std::size_t idx) const { return d[idx]; }

  operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      // otherwise subsetting does not work. Thus, warn instead of assert. Even though, R throws an error.
      return d[0];
    } else {
      return d[0];
    }
  }

  std::size_t size() const { return d.size(); }
  const std::vector<std::size_t>& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }
  auto begin() { return d.begin(); }
  auto end() { return d.end(); }

  template <typename OtherObj, typename DataTypeOtherObj>
  void copy_with_temp(const OtherObj& other_obj) {
    temp.resize(other_obj.size());
    for (std::size_t i = 0; i < other_obj.size(); i++) {
      if constexpr (IS<DataTypeOtherObj, T>) {
        temp[i] = other_obj[i];
      } else {
        temp[i] = static_cast<T>(other_obj[i]);
      }
    }
  }
  template<typename T2>
  void assign(const T2& other_obj) {
    using DataTypeOtherArray = typename ReRef<decltype(other_obj.d)>::type::value_type;
    copy_with_temp<T2, DataTypeOtherArray>(other_obj);
    d.moveit(temp);
  }

  // ======================= Constructors ===================================================
  // empty array
  explicit Array() : d(), temp(), dim() {}

  // arithmetic constructors
  Array(int sz) : d(1), temp(1), dim(1, 1) { d[0] = static_cast<T>(sz);}
  explicit Array(std::size_t sz) : d(1), temp(1), dim(1, 1) { d[0] = sz;}
  Array(double sz) : d(1), temp(1), dim(1, 1) { d[0] = sz;}
  Array(bool b) : d(1), temp(1), dim(1, 1) { d[0] = static_cast<T>(b);}

  // define Arraytor with specific size
  explicit Array(const SI &sz) : d(sz.sz), temp(sz.sz), dim(1, sz.sz) {}

  // Copy other Array with LBuffer
  Array(const Array& other) {
    assign(other);
    dim = other.get_dim();
  }
  // Move other Array e.g. vec = std::move(other_vec)
  Array(Array&& other) noexcept(std::is_nothrow_move_constructible_v<decltype(d)>)
  : d(std::move(other.d)), dim(std::move(other.dim)) {}

  // Copy view vectors: e.g. Array<double> a(sinus(c(1.1, 2.2)));
  template<typename T2, typename R2>
  requires(IsUnary<R2> || IsBinary<R2> || IsComparison<R2> || IsSubsetView<R2>)
  Array(const Array<T2, R2>& other) {
    assign(other);
    dim = other.get_dim();
  }

  // move: e.g. Array<int> arr = array(1.2, c(1, 2, 3))
  template<typename T2, typename R2>
  requires(IS<T, T2> && IsRArray<Array<T2, R2>>)
  Array(Array<T2, R2>&& other) {
    d.moveit(other.d);
    dim = std::move(other.dim);
  }
  // move: e.g. Array<int> arr = array(0, c(1, 2, 3))
  template<typename T2, typename R2>
  requires((!IS<T, T2>) && IsRArray<Array<T2, R2>>)
  Array(Array<T2, R2>&& other) {
    assign(other);
    dim = std::move(other.dim);
  }

  // Buffer
  explicit Array(Buffer<T, LBufferTrait>&& inp, const std::vector<std::size_t>& dim_) noexcept : d(std::move(inp)), temp(), dim(dim_) {}
  explicit Array(Buffer<T, LBufferTrait>& inp, const std::vector<std::size_t>& dim_) : d(inp), temp(), dim(dim_) {}
  explicit Array(Buffer<T, LBufferTrait>&& inp) noexcept : d(std::move(inp)), temp(), dim(1, d.size()) {}
  explicit Array(Buffer<T, LBufferTrait>& inp) : d(inp), temp(), dim(1, d.size()) {}

#ifdef STANDALONE_ETR
#else
  std::vector<std::size_t> get_dims_from_sexp(SEXP s) {
    SEXP dim = Rf_getAttrib(s, R_DimSymbol);
    if (dim == R_NilValue) {
      return std::vector<std::size_t>{
        static_cast<std::size_t>(Rf_length(s))
      };
    }
    int n = Rf_length(dim);
    int* p = INTEGER(dim);

    std::vector<std::size_t> dims(n);
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
  template<typename...Args>
  Array(Args...) {
    ass<"Constructor not supported">(sizeof(T) == 0);
  }

  // ======================= assignments =================================================
  // assign scalar values
  template <typename TD>
  requires IsArithV<TD>
  Array &operator=(const TD inp) {
    if constexpr (IS<TD, T>) {
      d.resize(1);
      d[0] = inp;
      dim.resize(1);
      dim[0] = d.size();
      return *this;
    } else {
      d.resize(1);
      d[0] = static_cast<T>(inp);
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
  requires(!IsArithV<Decayed<T2>> && IsArray<Decayed<T2>>)
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
    dim = std::move(other_obj.dim);
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
  std::vector<std::size_t> dim;

  // ======================= internal methods =================================================
  decltype(auto) operator[](std::size_t idx) { return d[idx]; }
  decltype(auto) operator[](std::size_t idx) const { return d[idx]; }

  operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      // otherwise subsetting does not work. Thus, warn instead of assert. Even though, R throws an error.
      return d[0];
    } else {
      return d[0];
    }
  }

  std::size_t size() const { return d.size(); }
  const std::vector<std::size_t>& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  // ======================= Constructors ===================================================
  explicit Array(const SI &sz) : d(sz.sz) {}

  Array(Array&& other) noexcept(std::is_nothrow_move_constructible_v<decltype(d)>)
  : d(std::move(other.d)), dim(std::move(other.dim)) {}

  template<typename...Args>
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
template<typename T> struct Array<T, Borrow<T, BorrowTrait>> {
  using Type = T;
  using DType = Borrow<T, BorrowTrait>;
  using value_type = T;
  Borrow<T, BorrowTrait> d;
  Buffer<T, LBufferTrait> temp;
  std::vector<std::size_t> dim;

  // ======================= internal methods =================================================
  decltype(auto) operator[](std::size_t idx) { return d[idx]; }
  decltype(auto) operator[](std::size_t idx) const { return d[idx]; }

  operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      // otherwise subsetting does not work. Thus, warn instead of assert. Even though, R throws an error.
      return d[0];
    } else {
      return d[0];
    }
  }

  std::size_t size() const { return d.size(); }
  const std::vector<std::size_t>& get_dim() const { return dim; }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }
  auto begin() { return d.begin(); }
  auto end() { return d.end(); }

  template <typename OtherObj, typename DataTypeOtherObj>
  void copy_with_temp(const OtherObj& other_obj) {
    temp.resize(other_obj.size());
    for (std::size_t i = 0; i < other_obj.size(); i++) {
      if constexpr (IS<DataTypeOtherObj, T>) {
        temp[i] = other_obj[i];
      } else {
        temp[i] = static_cast<T>(other_obj[i]);
      }
    }
  }
  template<typename T2>
  void assign(const T2& other_obj) {
    using DataTypeOtherArray = typename ReRef<decltype(other_obj.d)>::type::value_type;
    copy_with_temp<T2, DataTypeOtherArray>(other_obj);
    ass<"the borrowed region is too small for this assignment.">(other_obj.size() <= d.capacity);
    ass<"number of items to replace is not a multiple of replacement length">(other_obj.size() == d.size());
    d.sz = other_obj.size();
    for (std::size_t i = 0; i < other_obj.size(); i++) {
      d[i] = temp[i];
    }
  }

  // ======================= Constructors ===================================================
  explicit Array(T *ptr, std::size_t s, const std::vector<std::size_t>& dim_) : d(ptr, s), dim(dim_) {}
  template<typename...Args>
  Array(Args...) {
    ass<"Constructor not supported">(sizeof(T) == 0);
  }
  // ======================= assignments =================================================
  // assign scalar values
  template <typename TD>
  requires IsArithV<TD>
  Array &operator=(const TD inp) {
    ass<"number of items to replace is not a multiple of replacement length">(1 <= d.capacity);
    if constexpr (IS<TD, T>) {
      d.resize(1);
      d[0] = inp;
      dim.resize(1);
      dim[0] = d.size();
      return *this;
    } else {
      d.resize(1);
      d[0] = static_cast<T>(inp);
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
  requires(!IsArithV<Decayed<T2>> && IsArray<Decayed<T2>>)
  Array &operator=(const T2& other_obj) {
    assign(other_obj);
    dim = other_obj.get_dim();
    return *this;
  }
  // move assignment. but one has to still has to copy --> due to Borrow
  template<typename T2, typename R2>
  requires(IS<T, T2> && IsRArray<Array<T2, R2>>)
  Array& operator=(Array<T2, R2>&& other_obj) {
    assign(other_obj);
    dim = std::move(other_obj.dim);
    return *this;
  }

#ifdef STANDALONE_ETR
#else
  std::vector<std::size_t> get_dims_from_sexp(SEXP s) {
    SEXP dim = Rf_getAttrib(s, R_DimSymbol);
    if (dim == R_NilValue) {
      return std::vector<std::size_t>{
        static_cast<std::size_t>(Rf_length(s))
      };
    }
    int n = Rf_length(dim);
    int* p = INTEGER(dim);

    std::vector<std::size_t> dims(n);
    for (int i = 0; i < n; ++i) {
      dims[i] = static_cast<std::size_t>(p[i]);
    }
    return dims;
  }
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
  ConstHolder<std::vector<std::size_t>> dim;

  // ======================= internal methods =================================================
  decltype(auto) operator[](std::size_t idx) { return d[idx]; }
  decltype(auto) operator[](std::size_t idx) const { return d[idx]; }

  operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      // otherwise subsetting does not work. Thus, warn instead of assert. Even though, R throws an error.
      return d[0];
    } else {
      return d[0];
    }
  }

  std::size_t size() const { return d.size(); }
  const std::vector<std::size_t>& get_dim() const { return dim.get(); }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  template <typename L2, typename OperationTrait>
  explicit Array(UnaryOperation<L2, OperationTrait> &&inp, std::vector<std::size_t>&& dim_) : d(std::move(inp)), dim(std::move(dim_)) {}
  template <typename L2, typename OperationTrait>
  explicit Array(const UnaryOperation<L2, OperationTrait> &inp, const std::vector<std::size_t>& dim_) : d(inp), dim(dim_) {}
  template<typename...Args>
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
  ConstHolder<std::vector<std::size_t>> dim;

  // ======================= internal methods =================================================
  decltype(auto) operator[](std::size_t idx) { return d[idx]; }
  decltype(auto) operator[](std::size_t idx) const { return d[idx]; }

  operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      // otherwise subsetting does not work. Thus, warn instead of assert. Even though, R throws an error.
      return d[0];
    } else {
      return d[0];
    }
  }

  std::size_t size() const { return d.size(); }
  const std::vector<std::size_t>& get_dim() const { return dim.get(); }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }

  template <typename L2, typename R2, typename OperationTrait>
  explicit Array(BinaryOperation<L2, R2, OperationTrait> &&inp, std::vector<std::size_t>&& dim_) : d(std::move(inp)), dim(std::move(dim_)) {}
  template <typename L2, typename R2, typename OperationTrait>
  explicit Array(const BinaryOperation<L2, R2, OperationTrait> &inp, const std::vector<std::size_t>& dim_) : d(inp), dim(dim_) {}
  template<typename...Args>
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
  Holder<std::vector<std::size_t>> dim;

  // ======================= internal methods =================================================
  decltype(auto) operator[](std::size_t idx) { return d[idx]; }
  decltype(auto) operator[](std::size_t idx) const { return d[idx]; }

  operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      // otherwise subsetting does not work. Thus, warn instead of assert. Even though, R throws an error.
      return d[0];
    } else {
      return d[0];
    }
  }

  std::size_t size() const { return d.size(); }
  const std::vector<std::size_t>& get_dim() const { return dim.get(); }
  auto begin() const { return d.begin(); }
  auto end() const { return d.end(); }
  auto begin() { return d.begin(); }
  auto end() { return d.end(); }

  template <typename OtherObj, typename DataTypeOtherObj>
  void copy_with_temp(const OtherObj& other_obj) {
    temp.resize(other_obj.size());
    for (std::size_t i = 0; i < other_obj.size(); i++) {
      if constexpr (IS<DataTypeOtherObj, T>) {
        temp[i] = other_obj[i];
      } else {
        temp[i] = static_cast<T>(other_obj[i]);
      }
    }
  }
  template<typename T2>
  void assign(const T2& other_obj) {
    using DataTypeOtherArray = typename ReRef<decltype(other_obj.d)>::type::value_type;
    copy_with_temp<T2, DataTypeOtherArray>(other_obj);
    ass<"number of items to replace is not a multiple of replacement length">(other_obj.size() == d.size());
    d.moveit(temp);
  }

  // ======================= Constructors ===================================================
  template <typename O2, std::size_t N2, typename Trait2>
  explicit Array(SubsetView<O2, N2, Trait2> &&inp, std::vector<std::size_t>&& dim_) : d(std::move(inp)), dim(std::move(dim_)) {}
  template <typename O2, std::size_t N2, typename Trait2>
  explicit Array(SubsetView<O2, N2, Trait2> &inp, std::vector<std::size_t>&& dim_) : d(inp), dim(std::move(dim_)) {}

  template<typename...Args>
  Array(Args...) {
    ass<"Constructor not supported">(sizeof(T) == 0);
  }

  // ======================= assignments =================================================
  // assign scalar values
  template <typename TD>
  requires IsArithV<TD>
  Array &operator=(const TD inp) {
    if constexpr (IS<TD, T>) {
      for (std::size_t i = 0; i < d.size(); ++i) {
        d[i] = inp;
      }
    } else {
      T val = static_cast<T>(inp);
      for (std::size_t i = 0; i < d.size(); ++i) {
        d[i] = val;
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
  requires(!IsArithV<Decayed<T2>> && IsArray<Decayed<T2>>)
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

} // namespace etr

#endif
