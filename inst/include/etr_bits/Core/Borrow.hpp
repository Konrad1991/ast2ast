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

  std::size_t size() const { return sz; }

  // Empty constructor
  Borrow() { };

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
  Borrow(Borrow<T> &&other) noexcept
    : sz(other.sz), capacity(other.capacity), allocated(other.allocated), p(other.p) {
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
    p = other.p;
    other.capacity = 0;
    other.sz = 0;
    other.allocated = false;
    other.p = nullptr;
    return *this;
  }

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
    } else if constexpr (IS<value_type, bool>) {
      ass<"R object is not of type logical">(Rf_isLogical(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      ass<"R object seems to be empty">(sz >= 1);
      capacity = static_cast<std::size_t>(sz);
      p = LOGICAL(s);
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

  ~Borrow() {}

  void init(std::size_t size) = delete;
  void resize(std::size_t newSize) {
    ass<"cannot resize borrowed array beyond original memory">(newSize <= capacity);
    sz = newSize;
  }
  void set(std::size_t idx, T val) {
    ass<"Index has to be a positive number">(idx >= 0);
    ass(idx < sz, "Index cannot be larger than size of Indices");
    p[idx] = val;
  }

  value_type operator[](std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    return p[idx];
  }

  value_type &operator[](std::size_t idx) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    return p[idx];
  }

  template <typename L2> void moveit(L2 &other) = delete;
  auto begin() const { return It<T>{p}; }
  auto end() const { return It<T>{p + sz}; }
  T *data() const { return p; }
  void realloc(int new_size) = delete;
  void push_back(T input) = delete;
};

} // namespace etr

#endif
