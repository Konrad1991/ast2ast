#ifndef BORROW_ETR_H
#define BORROW_ETR_H

namespace etr {
// Points to a Variable and stores size
template <typename T, typename BorrowTrait> struct Borrow {
  using value_type = T;
  using Type = T;
  using Trait = BorrowTrait;
  using TypeTrait = BorrowTrait;

  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;
  T *p = nullptr;

  std::size_t size() const noexcept { return allocated ? sz : 0; }

  // Empty constructor
  Borrow() {};

  // Copy constructor
  Borrow(const Borrow<T> &other)
    : sz(other.sz), capacity(other.capacity), allocated(other.allocated) {
    p = other.p;
  }
  // Copy assignment
  Borrow &operator=(const Borrow<T> &other) {
    p = other.p;
    sz = other.sz;
    capacity = other.capacity;
    allocated = other.allocated;
    return *this;
  }
  // Move constructor
  // Move assignment
  // Made default thus it behaves like copying
  Borrow(Borrow&&) noexcept = default;
  Borrow& operator=(Borrow&&) noexcept = default;

  Borrow(std::size_t i) = delete;

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s) {
    if constexpr (IS<value_type, double>) {
      ass<"R object is not of type numeric">(Rf_isReal(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      ass<"R object seems to be empty">(sz >= 1);
      capacity = static_cast<std::size_t>(sz);
      p = REAL(s);
      allocated = true;
    } else if constexpr (IS<value_type, int>) {
      ass<"R object is not of type integer">(Rf_isInteger(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      ass<"R object seems to be empty">(sz >= 1);
      capacity = static_cast<std::size_t>(sz);
      p = INTEGER(s);
      allocated = true;
    } else {
      static_assert(sizeof(T) == 0, "Unsupported type found");
    }
  }
  Borrow(SEXP s) {
    initSEXP(s);
  };
#endif

  void init(T *p_, std::size_t sz_) {
    ass<"null pointer with positive size">(p_ != nullptr || sz_ == 0);
    this->p = p_;
    this->sz = sz_;
    capacity = sz;
    this->allocated = true;
  }
  Borrow(T *p_, std::size_t sz_) {
    init(p_, sz_);
  }
  Borrow(T *p_, int sz_) {
    init(p_, sz_);
  }

  ~Borrow() {}

  void init(std::size_t size) = delete;
  void resize(std::size_t newSize) {
    ass<"cannot resize borrowed array beyond original memory">(newSize <= capacity);
    sz = newSize;
  }

  value_type get(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    return p[idx];
  }

  void set(std::size_t idx, const value_type& val) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p[idx] = val;
  }

  template <typename L2> void moveit(L2 &other) = delete;
  auto begin() const { return It<const T>{p}; }
  auto end() const { return It<const T>{p + sz}; }
  void realloc(int new_size) = delete;
  void push_back(T input) = delete;
};

template <typename BorrowTrait> struct Borrow<Dual, BorrowTrait> {
  using value_type = Dual;
  using Type = Dual;
  using Trait = BorrowTrait;
  using TypeTrait = BorrowTrait;

  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;
  double *p_val = nullptr; // values
  double *p_dot = nullptr; // derivatives

  std::size_t size() const noexcept { return allocated ? sz : 0; }

  // Empty constructor
  Borrow() {};

  // Copy constructor
  Borrow(const Borrow<Dual> &other)
    : sz(other.sz), capacity(other.capacity), allocated(other.allocated) {
    p_val = other.p_val;
    p_dot = other.p_dot;
  }
  // Copy assignment
  Borrow &operator=(const Borrow<Dual> &other) {
    p_val = other.p_val;
    p_dot = other.p_dot;
    sz = other.sz;
    capacity = other.capacity;
    allocated = other.allocated;
    return *this;
  }
  Borrow(Borrow&&) noexcept = default;
  Borrow& operator=(Borrow&&) noexcept = default;

  Borrow(std::size_t i) = delete;

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s_val, SEXP s_dot) {
    ass<"R object is not of type numeric">(Rf_isReal(s_val));
    ass<"R object is not of type numeric">(Rf_isReal(s_dot));
    std::size_t sz_val = static_cast<std::size_t>(Rf_length(s_val));
    std::size_t sz_dot = static_cast<std::size_t>(Rf_length(s_dot));
    ass<"R object seems to be empty">(sz_val >= 1);
    ass<"R object seems to be empty">(sz_dot >= 1);
    ass<"Lengths of Value object and Derivative object do not match">(sz_dot == sz_val);
    capacity = static_cast<std::size_t>(sz_val);
    sz = capacity;
    p_val = REAL(s_val);
    p_dot = REAL(s_dot);
    allocated = true;
  }
  Borrow(SEXP s_val, SEXP s_dot) {
    initSEXP(s_val, s_dot);
  };
#endif

  void init(double* val_p_, double* dot_p_, std::size_t sz_) {
    ass<"null pointer with positive size">(val_p_ != nullptr || dot_p_ != nullptr || sz_ == 0);
    p_val = val_p_;
    p_dot = dot_p_;
    sz = sz_;
    capacity = sz;
    allocated = true;
  }
  Borrow(double *val_p_, double* dot_p_, std::size_t sz_) {
    init(val_p_, dot_p_, sz_);
  }
  Borrow(double *val_p_, double* dot_p_, int sz_) {
    init(val_p_, dot_p_, sz_);
  }

  ~Borrow() {}

  void init(std::size_t size) = delete;
  void resize(std::size_t newSize) {
    ass<"cannot resize borrowed array beyond original memory">(newSize <= capacity);
    sz = newSize;
  }

  value_type get(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    return Dual{ p_val[idx], p_dot[idx] };
  }
  void set(std::size_t idx, const Dual& d) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p_val[idx] = d.val;
    p_dot[idx] = d.dot;
  }

  struct Iterator {
    const double* v;
    const double* d;
    std::size_t idx;
    Dual operator*() const { return Dual{ v[idx], d[idx] }; }
    Iterator& operator++() { ++idx; return *this; }
    bool operator!=(const Iterator& other) const { return idx != other.idx; }
  };

  Iterator begin() const { return Iterator{ p_val, p_dot, 0 }; }
  Iterator end()   const { return Iterator{ p_val, p_dot, sz }; }
  template <typename L2> void moveit(L2 &other) = delete;

  void realloc(int new_size) = delete;
  void push_back(Dual input) = delete;
};

} // namespace etr

#endif
