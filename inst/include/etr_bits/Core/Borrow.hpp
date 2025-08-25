#ifndef BORROW_ETR_H
#define BORROW_ETR_H

// TODO: add SEXP stuff and remove BorrowSEXP
namespace etr {
// Points to a Variable and stores size
template <typename T, typename BorrowTrait> struct Borrow {
  using RetType = T;
  using Type = T;
  using Trait = BorrowTrait;
  using TypeTrait = BorrowTrait;
  T *p = nullptr;
  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;
  bool is_matrix = false;
  std::size_t nrow = 0;
  std::size_t ncol = 0;

  std::size_t size() const { return sz; }
  std::size_t nr() const { return nrow; }
  std::size_t nc() const { return ncol; }
  bool im() const { return is_matrix; }
  void set_matrix_default() noexcept {
    is_matrix = false;
    nrow = 0;
    ncol = 0;
  }
  void set_matrix(std::size_t nrow_, std::size_t ncol_) {
    is_matrix = true;
    nrow = nrow_;
    ncol = ncol_;
  }

  // Empty constructor
  Borrow() { };

  // Copy constructor
  Borrow(const Borrow<T> &other)
    : sz(other.sz), capacity(other.capacity), allocated(other.allocated),
    is_matrix(other.is_matrix), nrow(other.nrow), ncol(other.ncol) {
    p = other.p;
  }
  // Copy assignment
  Borrow &operator=(const Borrow<T> &other) {
    p = other.p;
    sz = other.sz;
    capacity = other.capacity;
    allocated = other.allocated;
    if (other.is_matrix) {
      is_matrix = true;
      nrow = other.nrow;
      ncol = other.ncol;
    } else {
      set_matrix_default();
    }
    return *this;
  }
  // Move constructor
  Borrow(Borrow<T> &&other) noexcept
    : sz(other.sz), capacity(other.capacity), allocated(other.allocated),
    is_matrix(std::exchange(other.is_matrix, false)),
    nrow(std::exchange(other.nrow, 0)), ncol(std::exchange(other.ncol, 0)),
    p(other.p) {
    other.capacity = 0;
    other.sz = 0;
    other.allocated = false;
    other.p = nullptr;
  }
  // Move assignment
  Borrow& operator=(Borrow<T> && other) {
    sz = other.sz;
    capacity = other.capacity;
    allocated = other.allocated;
    is_matrix = other.is_matrix;
    nrow = other.nrow;
    ncol = other.ncol;
    p = other.p;
    other.capacity = 0;
    other.sz = 0;
    other.allocated = false;
    other.p = nullptr;
    other.is_matrix = false;
    other.nrow = 0;
    other.ncol = 0;
    return *this;
  }

  Borrow(std::size_t i) = delete;

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s) {
    if constexpr (is<RetType, double>) {
      ass<"R object is not of type numeric">(Rf_isReal(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      capacity = static_cast<std::size_t>(sz);
      p = REAL(s);
      allocated = true;
      if (Rf_isMatrix(s)) {
        set_matrix(Rf_nrows(s), Rf_ncols(s));
      }
    } else if constexpr (is<RetType, int>) {
      ass<"R object is not of type integer">(Rf_isInteger(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      capacity = static_cast<std::size_t>(sz);
      p = INTEGER(s);
      allocated = true;
      if (Rf_isMatrix(s)) {
        set_matrix(Rf_nrows(s), Rf_ncols(s));
      }
    } else if constexpr (is<RetType, bool>) {
      ass<"R object is not of type logical">(Rf_isLogical(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      capacity = static_cast<std::size_t>(sz);
      p = LOGICAL(s);
      allocated = true;
      if (Rf_isMatrix(s)) {
        set_matrix(Rf_nrows(s), Rf_ncols(s));
      }
    } else {
      static_assert(sizeof(T) == 0, "Unsupported type found");
    }
  }
  Borrow(SEXP s) {
    initSEXP(s);
  };
#endif

  void init(T *p_, std::size_t sz_) {
    this->p = p_;
    this->sz = sz_;
    capacity = sz;
    this->allocated = true;
    set_matrix_default();
  }
  Borrow(T *p_, std::size_t sz_) {
    init(p_, sz_);
  }
  Borrow(T *p_, int sz_) {
    init(p_, sz_);
  }

  void init(T *p_, std::size_t nrow_, std::size_t ncol_) {
    this->p = p_;
    this->sz = nrow_ * ncol_;
    capacity = sz;
    this->allocated = true;
    set_matrix(nrow_, ncol_);
  }
  Borrow(T *p_, std::size_t nrow_, std::size_t ncol_) {
    init(p_, nrow_, ncol_);
  }
  Borrow(T *p_, int nrow_, int ncol_) {
    init(p_, nrow_, ncol_);
  }

  template <typename TInp>
    requires(IsArithV<Decayed<TInp>>)
  void fill(const TInp &val) {
    if constexpr (IS<T, TInp>) {
      std::fill(p, p + sz, val);
    } else {
      auto temp = static_cast<T>(val);
      std::fill(p, p + sz, temp);
    }
  }

  template <typename TInp> void fill(const TInp &inp) {
    ass<"cannot use fill with vectors of different lengths">(inp.size() == sz);
    using DataType = typename ExtractDataType<Decayed<TInp>>::RetType;
    if constexpr (IsVec<TInp>) {
      if constexpr (!is<DataType, T>) {
        for (std::size_t i = 0; i < sz; i++)
          p[i] = static_cast<T>(inp[i]);
      } else {
        DataType *ptr = inp.getPtr();
        std::copy(ptr, ptr + sz, p);
      }
    } else if constexpr (IsRVec<TInp> && is<DataType, T>) {
      delete[] p;
      DataType *ptr = inp.getPtr();
      inp.d.p = nullptr;
      inp.d.allocated = false;
      p = ptr;
    } else {
      if constexpr (is<DataType, T>) {
        for (std::size_t i = 0; i < sz; i++)
          p[i] = inp[i];
      } else {
        for (std::size_t i = 0; i < sz; i++)
          p[i] = static_cast<T>(inp[i]);
      }
    }
  }

  ~Borrow() {}

  void init(std::size_t size) = delete;
  void resize(std::size_t newSize) {
    ass<"Size has to be larger than 0">(newSize >= 0);
    if (newSize <= capacity) {
      sz = newSize;
    } else {
      ass<"Cannot resize Borrow element above size of borrowed object">(false);
    }
  };
  void set(std::size_t idx, T val) {
    ass<"Index has to be a positive number">(idx >= 0);
    ass(idx < sz, "Index cannot be larger than size of Indices");
    p[idx] = val;
  }

  RetType operator[](std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    // NOTE: negative idx leads to overflow and
    // is than the max possible value of std::size_t
    ass<"Error: out of boundaries">(idx < sz);
    return p[idx];
  }

  RetType &operator[](std::size_t idx) {
    ass<"No memory was allocated">(allocated);
    // NOTE: negative idx leads to overflow and
    // is than the max possible value of std::size_t
    ass<"Error: out of boundaries">(idx < sz);
    return p[idx];
  }

  template <typename L2> void moveit(L2 &other) = delete;
  auto begin() const { return It<T>{p}; }
  auto end() const { return It<T>{p + sz}; }
  T &back() { return p[sz]; }
  T *data() const { return p; }
  void realloc(int new_size) = delete;
  void push_back(T input) = delete;
  void setSize(std::size_t sz_) { this->sz = sz_; }
  void setPtr(const T *pOther) { this->p = pOther; }
};

} // namespace etr

#endif
