#ifndef BORROW_ETR_H
#define BORROW_ETR_H

// TODO: add SEXP stuff and remove BorrowSEXP
namespace etr {
// Points to a Variable and stores size
template <typename T, typename BorrowTrait> struct Borrow {
  using RetType = T;
  using Type = T;
  using Trait = BorrowTrait;
  T *p = nullptr;
  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;
  MatrixParameter mp;

  std::size_t size() const { return sz; }
  bool im() const { return mp.im(); }
  std::size_t nc() const { return mp.nc(); }
  std::size_t nr() const { return mp.nr(); }
  void setMatrix(bool i, std::size_t nrow, std::size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }

  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }

  Borrow(const Borrow<T> &other)
      : sz(other.sz), capacity(other.capacity), allocated(other.allocated) {
    p = other.p;
    mp.setMatrix(other.mp);
  }
  Borrow(Borrow<T> &&other) noexcept
      : sz(other.sz), capacity(other.capacity), allocated(other.allocated),
        p(other.p) {
    other.capacity = 0;
    other.sz = 0;
    other.allocated = false;
    other.p = nullptr;
    mp.setMatrix(other.mp);
  }
#ifdef STANDALONE_ETR
#else
  Borrow(SEXP s) = delete;
#endif

  Borrow(std::size_t i) {
    sz = 0;
    capacity = 0;
    allocated = false;
  };
  Borrow(int i) = delete;
  Borrow() {
    sz = 0;
    capacity = 0;
    allocated = false;
  };
  Borrow(std::size_t r, std::size_t c) = delete;
  Borrow(std::size_t r, std::size_t c, const double value) = delete;
  Borrow(T *p_, std::size_t sz_) {
    this->p = p_;
    this->sz = sz_;
    capacity = sz;
    this->allocated = true;
  }

  void init(T *p_, std::size_t sz_) {
    this->p = p_;
    this->sz = sz_;
    capacity = sz;
    this->allocated = true;
  }

  Borrow &operator=(const Borrow<T> &other) {
    p = other.p;
    sz = other.sz;
    capacity = other.capacity;
    allocated = other.allocated;
    mp.setMatrix(other.mp);
    return *this;
  }

  template <typename TInp>
    requires(IsArithV<ReRef<TInp>>)
  void fill(TInp &&val) {
    if constexpr (IS<T, TInp>) {
      std::fill(p, p + sz, val);
    } else {
      auto temp = static_cast<T>(val);
      std::fill(p, p + sz, temp);
    }
  }

  template <typename TInp> void fill(TInp &&inp) {
    ass<"cannot use fill with vectors of different lengths">(inp.size() == sz);
    using DataType =
        typename ExtractDataType<ReRef<TInp>>::RetType;
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
    ass<"Error: out of boundaries --> value below 1">(idx >= 0);
    ass<"Error: out of boundaries">(idx < sz);
    return p[idx];
  }

  RetType &operator[](std::size_t idx) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries --> value below 1">(idx >= 0);
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

  friend std::ostream &operator<<(std::ostream &os, const Borrow &b) {
    os << "Vec [ ";
    for (std::size_t i = 0; i < b.size(); i++) {
      os << b.p[i] << " ";
    }
    os << "]";
    return os;
  }
};

} // namespace etr

#endif
