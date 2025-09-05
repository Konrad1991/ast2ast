#ifndef BUFFER_VECTOR_ETR_HPP
#define BUFFER_VECTOR_ETR_HPP

namespace etr {

template <typename T, typename R> struct Vec {
  using Type = T;
  R d;
  Buffer<T> temp;
  using DType = R;
  using RetType = typename ReRef<decltype(d)>::type::RetType;
  using InnerTrait = typename ReRef<R>::type::TypeTrait;

  // TODO: construct Vec from Mat

  // NOTE: define Vector with specific size
  explicit Vec(SI &sz) : d(sz.sz) {}
  explicit Vec(SI &&sz) : d(sz.sz) {}

  //  NOTE: Copy other Vec
  //  It is important to note that Vec<Borrow> does not require the copy and move constructors below.
  //  It is only possible to construct Vec<Borrow> with pointers and size arguments
  Vec(const Vec& other) {
    if constexpr (IsBorrow<R>) {
      d = other.d;
    } else {
      assign(other);
    }
  }
  //  NOTE: Move other Vec
  //  e.g. vec = std::move(other_vec)
  Vec(Vec&& other) noexcept(std::is_nothrow_move_constructible_v<R>)
  : d(std::move(other.d)) {}

  // move: e.g. Vec<int> vec = c(1, 2, 3)
  template<typename T2, typename R2>
  requires(IS<T, T2> && IsLBuffer<R> && IsRArrayLike<Vec<T2, R2>>)
  Vec(Vec<T2, R2>&& other) {
    d.moveit(other.d);
  }
  // move: e.g. Vec<int> vec = c(1.2, 2.2, 3.3)
  template<typename T2, typename R2>
  requires((!IS<T, T2>) && IsLBuffer<R> && IsRArrayLike<Vec<T2, R2>>)
  Vec(Vec<T2, R2>&& other) {
    assign(other);
  }

  // NOTE: Buffer
  template <typename L2> explicit Vec(Buffer<L2> &&inp) noexcept : d(std::move(inp)) {}
  template <typename L2> explicit Vec(Buffer<L2> &inp) : d(inp) {}
  template <typename L2, typename TraitOther>
  explicit Vec(const Buffer<L2, TraitOther> &inp) : d(inp) {}

  // NOTE: Borrow
  template <typename U = R, typename T2>
    requires IS<U, Borrow<T>>
  explicit Vec(Borrow<T2> &&borrowed) : d(borrowed) {}
  template <typename U = R, typename T2>
    requires IS<U, Borrow<T>>
  explicit Vec(Borrow<T2> &borrowed) : d(borrowed) {}
  template <typename U = R>
    requires IS<U, Borrow<T>>
  explicit Vec(T *ptr, std::size_t s) : d(ptr, s) {}

#ifdef STANDALONE_ETR
#else
  Vec(SEXP inp) : d(inp) {}
#endif

  // NOTE: Subset lazy
  template <typename L2, typename R2, typename TraitL>
    requires IS<TraitL, SubsetClassTrait>
  explicit Vec(SubsetClass<L2, R2, TraitL> &&inp) : d(std::move(inp)) {}
  template <typename L2, typename R2, typename TraitL>
    requires IS<TraitL, SubsetClassTrait>
  explicit Vec(SubsetClass<L2, R2, TraitL> &inp) : d(std::move(inp)) { }

  // NOTE: Binary operation
  template <typename L2, typename R2, typename OperationTrait>
  explicit Vec(BinaryOperation<L2, R2, OperationTrait> &&inp) : d(inp) {}
  template <typename L2, typename R2, typename OperationTrait>
  explicit Vec(BinaryOperation<L2, R2, OperationTrait> &inp) : d(inp) {}

  // NOTE: Unary operation
  template <typename L2, typename OperationTrait>
  explicit Vec(UnaryOperation<L2, OperationTrait> &&inp) : d(inp) {}
  template <typename L2, typename OperationTrait>
  explicit Vec(UnaryOperation<L2, OperationTrait> &inp) : d(inp) {}

  // NOTE: arithmetic constructors
  explicit Vec(int sz) : d(1) { d[0] = static_cast<T>(sz); } // TODO: why explicit.
  explicit Vec(std::size_t sz) : d(1) { d[0] = sz; }
  Vec(double sz) : d(1) { d[0] = sz;}
  Vec(bool b) : d(1) { d[0] = static_cast<T>(b);}

  // NOTE: pointer constructor
  template<typename T2>
  requires IsBorrow<R>
  explicit Vec(T* ptr, std::size_t sz) : d(ptr, sz) {}
  template<typename T2>
  requires IsBorrow<R>
  explicit Vec(T* ptr, int sz) : d(ptr, sz) {}

  // NOTE: empty vector
  explicit Vec() : d() {}

  template <typename OtherObj>
  void copyFromVec(const OtherObj& other_obj) {
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
  Vec(const T2 &&other_obj) {
    // TODO: handle different base type
    copyFromVec(other_obj);
  }

  template <typename OtherObj>
  void moveFromVec(const OtherObj& other_obj) {
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
  Vec &operator=(const TD inp) {
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
    static_assert(
    !IsUnary<R>,
    "\n\n\n"
    "[etr::assignment Error]\n"
    "You tried to assign to a (unary) calculation."
    "\n\n\n"
  );
    static_assert(
    !IsBinary<R>,
    "\n\n\n"
    "[etr::assignment Error]\n"
    "You tried to assign to a (binary) calculation."
    "\n\n\n"
  );
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
    using DataTypeOtherVec = typename ReRef<decltype(other_obj.d)>::type::RetType;
    copyWithTemp<T2, DataTypeOtherVec>(other_obj);
    if constexpr (IsLBuffer<R>) {
      d.moveit(temp);
    } else if constexpr (IsBorrow<R>) {
      ass<"number of items to replace is not a multiple of replacement length">(other_obj.size() <= d.capacity);
      d.sz = other_obj.size();
      for (std::size_t i = 0; i < other_obj.size(); i++) d[i] = temp[i];
    } else if constexpr (IS<SubsetClassTrait, typename ReRef<R>::type::TypeTrait>) {
      ass<"number of items to replace is not a multiple of replacement length">(other_obj.size() == d.size());
      for (std::size_t i = 0; i < d.size(); i++) d[safe_modulo(i, d.size())] = temp[i];
    } else if constexpr (IsRBuffer<R>) { // Sometimes in c() required
      d.moveit(temp);
    }
  }

  // copy assignment
  // Intended for: Calculations, subsets
  template <typename T2>
  requires (
    !IsArithV<Decayed<T2>> && (!IsRArrayLike<T2>)
  )
  Vec &operator=(const T2 &other_obj) {
    assign(other_obj);
    return *this;
  }

  // copy assignment
  // Intendend for matrices
  template<typename T2>
  requires(
    !IsArithV<Decayed<T2>> && IsMat<T2>
  )
  Vec &operator=(const T2& other_obj) {
    assign(other_obj);
    return *this;
  }

  // copy assignment
  // defined to handle the case where the type of *this and other_obj is the same
  Vec& operator=(const Vec& other_obj) {
    assign(other_obj);
    return *this;
  }

  // move assignment
  // Intended to swap resources between Buffer and RBuffer e.g. v = c(1, 2, 3)
  template<typename T2, typename R2>
  requires(
    IS<T, T2> && IsLBuffer<R> && IsRArrayLike<Vec<T2, R2>>
  )
  Vec& operator=(Vec<T2, R2>&& other_obj) {
    d.moveit(other_obj.d);
    return *this;
  }
  // move assignment
  // If R is borrow it has to be copied!
  template<typename T2, typename R2>
  requires(
  IsBorrow<R>
  )
  Vec& operator=(Vec<T2, R2>&& other_obj) {
    assign(std::forward<decltype(other_obj)>(other_obj));
    return *this;
  }

#ifdef STANDALONE_ETR
#else
  Vec &operator=(SEXP s) {
    d.initSEXP(s);
    return *this;
  }
#endif

  RetType &operator[](std::size_t idx) { return d[idx]; }
  RetType operator[](std::size_t idx) const { return d[idx]; }

  operator RetType() const {
    if constexpr (IS<RetType, bool>) {
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

  auto begin() const {
    return d.begin();
  }
  auto end() const {
    return d.end();
  }

  T &back() const { return d.p[this->size()]; } // TODO: use the back function from R. Because not all classes have p

  void fill(T value) { d.fill(value); }
  void resize(std::size_t newSize) { d.resize(newSize); }
};

} // namespace etr

#endif
