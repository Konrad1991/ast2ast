#ifndef BUFFER_MATRIX_ETR_HPP
#define BUFFER_MATRIX_ETR_HPP

namespace etr {

template <typename T, typename R> struct Mat {
  using Type = T;
  R d;
  Buffer<T> temp;
  using DType = R;
  using value_type = typename ReRef<decltype(d)>::type::value_type;

  // NOTE: define Matrix with specific size
  explicit Mat(SI &&nrow, SI&& ncol) : d(nrow.sz, ncol.sz) {}

  //  NOTE: Copy other Mat
  //  It is important to note that Mat<Borrow> does not require the copy and move constructors below.
  //  It is only possible to construct Mat<Borrow> with pointers and size arguments
  Mat(const Mat& other) {
    if constexpr (IsBorrow<R>) {
      d = other.d;
    } else {
      assign(other);
    }
  }
  //  NOTE: Move other Mat
  //  e.g. vec = std::move(other_vec)
  Mat(Mat&& other) noexcept {
    d.moveit(other.d);
  }
  // move: e.g. Mat<int> vec = c(1, 2, 3)
  template<typename T2, typename R2>
  requires(IS<T, T2> && IsLBuffer<R> && IsRArrayLike<Mat<T2, R2>>)
  Mat(Mat<T2, R2>&& other) {
    d.moveit(other.d);
  }
  // move: e.g. Mat<int> vec = c(1.2, 2.2, 3.3)
  template<typename T2, typename R2>
  requires((!IS<T, T2>) && IsLBuffer<R> && IsRArrayLike<Mat<T2, R2>>)
  Mat(Mat<T2, R2>&& other) {
    assign(other);
  }

  // NOTE: Buffer
  template <typename L2> explicit Mat(const Buffer<L2> &&inp) : d(inp) { }
  template <typename L2> explicit Mat(const Buffer<L2> &inp) : d(inp) { }
  template <typename L2, typename TraitOther>
  explicit Mat(const Buffer<L2, TraitOther> &inp) : d(inp) { }

  // NOTE: Borrow
  template <typename U = R, typename T2>
    requires IS<U, Borrow<T>>
  explicit Mat(Borrow<T2> &&borrowed) : d(borrowed) { }
  template <typename U = R, typename T2>
    requires IS<U, Borrow<T>>
  explicit Mat(Borrow<T2> &borrowed) : d(borrowed) { }
  template <typename U = R>
    requires IS<U, Borrow<T>>
  explicit Mat(T *ptr, std::size_t s) : d(ptr, s) {}

#ifdef STANDALONE_ETR
#else
  template <typename U = R>
    requires IS<U, Borrow<T>>
  explicit Mat(SEXP &&inp) = delete;

  template <typename U = R>
    requires IS<U, Borrow<T>>
  explicit Mat(SEXP inp) : d(inp) {}

  template <typename U = R>
    requires (!IS<U, Borrow<T>>)
  Mat(SEXP &&inp) = delete;

  template <typename U = R>
    requires (!IS<U, Borrow<T>>)
  Mat(SEXP inp) : d(inp) {}
#endif

  // NOTE: Subset lazy
  template <typename L2, typename R2, typename TraitL>
    requires IS<TraitL, SubsetClassTrait>
  explicit Mat(SubsetClass<L2, R2, TraitL> &&inp) : d(std::move(inp)) { }
  template <typename L2, typename R2, typename TraitL>
    requires IS<TraitL, SubsetClassTrait>
  explicit Mat(SubsetClass<L2, R2, TraitL> &inp) : d(std::move(inp)) { }

  // NOTE: Binary operation
  template <typename L2, typename R2, typename OperationTrait>
  explicit Mat(BinaryOperation<L2, R2, OperationTrait> &&inp) : d(inp) { }
  template <typename L2, typename R2, typename OperationTrait>
  explicit Mat(BinaryOperation<L2, R2, OperationTrait> &inp) : d(inp) { }

  // NOTE: Unary operation
  template <typename L2, typename OperationTrait>
  explicit Mat(UnaryOperation<L2, OperationTrait> &&inp) : d(inp) { }
  template <typename L2, typename OperationTrait>
  explicit Mat(UnaryOperation<L2, OperationTrait> &inp) : d(inp) { }

  // NOTE: arithmetic constructors
  explicit Mat(int sz) : d(1) { d[0] = static_cast<T>(sz); }
  explicit Mat(std::size_t sz) : d(1) { d[0] = sz; }
  Mat(double sz) : d(1) { d[0] = sz; }
  Mat(bool b) : d(1) { d[0] = static_cast<T>(b); }

  // NOTE: pointer constructor
  template<typename T2>
  requires IsBorrow<R>
  explicit Mat(T* ptr, std::size_t nrow_, std::size_t ncol_) : d(ptr, nrow_, ncol_) {}
  explicit Mat(T* ptr, int nrow_, int ncol_) : d(ptr, nrow_, ncol_) {}

  // NOTE: empty vector
  explicit Mat() : d() {}

  template <typename OtherObj>
  void copyFromMat(const OtherObj& other_obj) {
    if constexpr (IsBorrow<R>) {
      ass<"Sizes do not match">(d.sz <= other_obj.size());
      d.sz = other_obj.size();
    } else {
      if (d.sz < other_obj.size()) {
        d.resize(other_obj.size());
      } else {
        d.sz = other_obj.size();
      }
    }
    for (std::size_t i = 0; i < d.size(); i++) {
      d[i] = other_obj[i];
    }
  }
  // NOTE: vector or matrix l object
  template <typename T2>
    requires IsArrayLike<const T2>
  Mat(const T2 &&other_obj) {
    // TODO: handle different base type
    copyFromMat(other_obj);
  }

  template <typename OtherObj>
  void moveFromMat(const OtherObj& other_obj) {
    if constexpr (IsBorrow<R>) {
      ass<"Sizes do not match">(d.sz <= other_obj.size());
      d.sz = other_obj.size();
      for (std::size_t i = 0; i < d.size(); i++) {
        d[i] = other_obj[i];
      }
    } else {
      std::size_t tempSize = other_obj.size();
      d.allocated = other_obj.d.allocated;
      other_obj.d.sz = this->size();
      d.sz = tempSize;
      T *tempP = other_obj.d.p;
      other_obj.d.p = d.p;
      d.p = tempP;
    }
  }

  // Assignments
  template <typename TD>
    requires IsArithV<TD>
  Mat &operator=(const TD inp) {
    invalid_lhs();
    // Same Type
    // ---------------------------------------------------------------------
    if constexpr (is<TD, T>) {
      if constexpr (IS<SubsetClassTrait, typename ReRef<R>::type::TypeTrait>) {
        for (std::size_t i = 0; i < d.size(); i++) {
          d[i] = inp;
        }
      } else if constexpr (IsBorrow<R>) {
        d.sz = 1;
        d[0] = inp;

      } else {
        d.resize(1);
        d[0] = inp;
      }
      return *this;
      // Different Type
      // ---------------------------------------------------------------------
    } else {
      if constexpr (IS<SubsetClassTrait, typename ReRef<R>::type::TypeTrait>) {
        for (std::size_t i = 0; i < d.size(); i++) {
          d[i] = static_cast<T>(inp);
        }

      } else if constexpr (IsBorrow<R>) {
        d.sz = 1;
        d[0] = static_cast<T>(inp);

      } else {
        d.resize(1);
        d[0] = static_cast<T>(inp);
      }
      return *this;
    }
  }

  void invalid_lhs() {
    static_assert(!IsUnary<R>, "Cannot assign to unary calculation");
    static_assert(!IsBinary<R>, "Cannot assign to binary calculation");
  }
  template <typename OtherObj, typename DataTypeOtherObj>
  void copyWithTemp(const OtherObj& other_obj) {
    temp.resize(other_obj.size());
    for (std::size_t i = 0; i < other_obj.size(); i++) {
      if constexpr (is<DataTypeOtherObj, T>) {
        temp[i] = other_obj[i];
      } else {
        temp[i] = static_cast<T>(other_obj[i]);
      }
    }
  }

  template<typename T2>
  void assign(const T2& other_obj) {
    invalid_lhs();
    using DataTypeOtherVec = typename ReRef<decltype(other_obj.d)>::type::value_type;
    copyWithTemp<T2, DataTypeOtherVec>(std::forward<decltype(other_obj)>(other_obj));
    if constexpr (IsLBuffer<R>) {
      d.moveit(temp);
    } else if constexpr (IsBorrow<R>) {
      ass<"number of items to replace is not a multiple of replacement length">(other_obj.size() <= d.capacity);
      ass<"size cannot be increased above the size of the borrowed object">(d.sz <= other_obj.size());
      d.sz = other_obj.size();
      for (std::size_t i = 0; i < other_obj.size(); i++) d[i] = temp[i];
    } else if constexpr (IS<SubsetClassTrait, typename ReRef<R>::type::TypeTrait>) {
      ass<"number of items to replace is not a multiple of replacement length">(other_obj.size() == d.size());
      for (std::size_t i = 0; i < d.size(); i++) d[safe_modulo(i, d.size())] = temp[i];
    } else if constexpr (IsRBuffer<R>) { // Sometimes in c() required
      d.moveit(temp);
    }
    copy_matrix_dim(other_obj);
  }

  // copy assignment
  // Intended for: Calculations, subsets
  template <typename T2>
  requires (
  !IsArithV<Decayed<T2>> && (!IsRArrayLike<T2>)
  )
  Mat &operator=(const T2 &other_obj) {
    assign(std::forward<decltype(other_obj)>(other_obj));
    return *this;
  }

  // copy assignment
  // defined to handle the case where the type of *this and other_obj is the same
  Mat& operator=(const Mat& other_obj) {
    assign(std::forward<decltype(other_obj)>(other_obj));
    return *this;
  }

  // copy assignment
  // defined to handle the case where the type of *this and other_obj is the same but its a Vec
  template<typename T2>
  Mat& operator=(const T2& other_obj) {
    assign(std::forward<decltype(other_obj)>(other_obj));
    return *this;
  }

  // move assignment
  // Intended to swap resources between Buffer and RBuffer e.g. v = c(1, 2, 3)
  template<typename T2, typename R2>
  requires(
  IS<T, T2> && IsLBuffer<R> && IsRArrayLike<Mat<T2, R2>>
  )
  Mat& operator=(Mat<T2, R2>&& other_obj) {
    d.moveit(other_obj.d);
    return *this;
  }
  // move assignment
  // If R is borrow it has to be copied!
  template<typename T2, typename R2>
  requires(
  IsBorrow<R>
  )
  Mat& operator=(Mat<T2, R2>&& other_obj) {
    assign(std::forward<decltype(other_obj)>(other_obj));
    return *this;
  }

#ifdef STANDALONE_ETR
#else
  Mat &operator=(SEXP s) {
    d.initSEXP(s);
    // TODO: add check that this is a matrix
    return *this;
  }
#endif

  value_type &operator[](std::size_t idx) { return d[idx]; }
  value_type operator[](std::size_t idx) const { return d[idx]; }

  operator value_type() const {
    if constexpr (IS<value_type, bool>) {
      warn<"Warning in if: the condition has length > 1">(this->size() == 1);
      // NOTE: otherwise subsetting does not work. Thus, warn instead of assert
      return d[0];
    } else {
      return d[0];
    }
  }

  std::size_t size() const { return d.size(); }
  bool im() const { return d.im(); }
  std::size_t nc() const { return d.nc(); }
  std::size_t nr() const { return d.nr(); }

  template<typename OtherObj>
  void copy_matrix_dim(const OtherObj& other) {
    if (other.im()) d.set_matrix(other.nr(), other.nc());
    ass<"Length does not match nrow*ncol">(d.size() == (d.nr() * d.nc()));
  }

  auto begin() const {
    return d.begin();
  }
  auto end() const {
    return d.end();
  }

  T &back() const { return d.p[this->size()]; }

  void fill(T value) { d.fill(value); }
  void resize(std::size_t newSize) { d.resize(newSize); }
};

} // namespace etr

#endif
