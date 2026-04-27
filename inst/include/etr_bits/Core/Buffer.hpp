#ifndef BUFFER_ETR_H
#define BUFFER_ETR_H

namespace etr {

// Iterator for the AoS fallback Buffer (used for Variable<T>).
template <typename Scalar>
struct BufferIt {
  const Scalar* ptr;

  using value_type = Scalar;
  using reference  = Scalar;
  using difference_type = std::ptrdiff_t;
  using iterator_category = std::forward_iterator_tag;

  reference operator*() const { return *ptr; }

  BufferIt& operator++() {
    ++ptr;
    return *this;
  }

  bool operator!=(const BufferIt& other) const {
    return ptr != other.ptr;
  }
};

//==============================================================================
// Generic Buffer — AoS layout.
// The split data/NA representation does not apply to Variable<T> (which stores
// an expression tree, not a plain value), so Variable buffers use this
// fallback. All other scalar types are handled by the specializations below.
//==============================================================================
template <typename T, typename BufferTrait> struct Buffer {

  using value_type = T;
  using Trait = BufferTrait;
  using TypeTrait = BufferTrait;

  T *p = nullptr;
  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;

  std::size_t size() const noexcept{
    if (!allocated) return 0;
    return sz;
  }

  void reset() noexcept {
    this -> allocated = false;
    this -> p = nullptr;
    this -> sz = 0;
    this -> capacity = 0;
  }

  Buffer() {}

  Buffer(const Buffer& other)
  : sz(other.sz), capacity(other.capacity), allocated(other.allocated) {
    if (!allocated || capacity == 0) { reset(); return; }
    if (sz > capacity) {
      ass<"Buffer invariant violated: sz > capacity">(sz > capacity);
    }
    p = new T[capacity];
    for (std::size_t i = 0; i < sz; i++) {
      p[i] = other.p[i];
    }
  }
  Buffer(Buffer &&other) noexcept
    : p(std::exchange(other.p, nullptr)), sz(std::exchange(other.sz, 0)),
    capacity(std::exchange(other.capacity, 0)),
    allocated(std::exchange(other.allocated, false)) {}
  Buffer &operator=(const Buffer &other) {
    if (this == &other)
      return *this;
    if (other.size() > capacity) {
      resize(other.size());
    }
    sz = other.size();
    if (sz) std::copy_n(other.p, sz, p);
    return *this;
  }
  Buffer &operator=(Buffer &&other) noexcept {
    swap(other);
    return *this;
  }
  Buffer(std::size_t sz_) {
    ass<"Size has to be larger than 0!">(sz_ > 0);
    init(sz_);
  }

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s) {
    if (allocated) {
      ass<"try to delete nullptr">(p != nullptr);
      delete[] p;
      reset();
    }
    // The generic template is only instantiated for Variable<Double>; the
    // scalar types have their own specializations. Keep the Variable path.
    if constexpr (IS<value_type, Variable<Double>>) {
      ass<"R object is not of type numeric">(Rf_isReal(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      ass<"R object seems to be empty">(sz >= 1);
      capacity = static_cast<std::size_t>(sz);
      p = new T[capacity];
      for (std::size_t i = 0; i < sz; i++) {
        p[i] = REAL(s)[i];
      }
      allocated = true;
    } else {
      ass<"Unsupported type found in generic Buffer::initSEXP">(false);
    }
  }
  Buffer(SEXP s) {
    initSEXP(s);
  };
#endif

  template <typename TInp>
  requires(IsCppArithV<Decayed<TInp>>)
  void fill(TInp &&val) {
    if constexpr (IS<T, TInp>) {
      std::fill(p, p + sz, val);
    } else {
      auto temp = static_cast<T>(val);
      std::fill(p, p + sz, temp);
    }
  }
  template <typename TInp>
  requires(IS<TInp, Variable<Double>> || IsScalarOrScalarRef<Decayed<TInp>>)
  void fill(TInp &&val) {
    if constexpr (IS<T, Double>) {
      std::fill(p, p + sz, get_val(val));
    } else {
      auto temp = static_cast<T>(get_val(val));
      std::fill(p, p + sz, temp);
    }
  }

  ~Buffer() {
    if (p != nullptr) {
      if (allocated) {
        delete[] p;
        reset();
      }
    }
  }

  void init(std::size_t size) {
    if (allocated) {
      ass<"try to delete nullptr">(p != nullptr);
      delete[] p;
      p = nullptr;
    }
    sz = size;
    capacity = static_cast<std::size_t>(1.15 * static_cast<double>(sz));
    p = new T[capacity];
    fill(T());
    allocated = true;
  }
  void resize(std::size_t newSize) {
    ass<"Size has to be larger than 0!">(newSize > 0);
    if (!allocated) {
      init(newSize);
      return;
    }
    if (newSize > capacity) {
      realloc(newSize);
      return;
    }
    sz = newSize;
  }

  const value_type& get(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    return p[idx];
  }
  void set(std::size_t idx, const value_type& val) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p[idx] = val;
  }

  template <typename L2> void moveit(L2 &other) {
    T *temporary = other.p;
    std::size_t tempSize = other.sz;
    std::size_t tempCapacity = other.capacity;
    other.p = this->p;
    other.sz = this->sz;
    other.capacity = this->capacity;
    other.allocated = this->allocated;
    this->p = temporary;
    this->sz = tempSize;
    this->capacity = tempCapacity;
    allocated = true;
  }

  void swap(Buffer &other) noexcept {
    if (this != &other) {
      std::swap(p, other.p);
      std::swap(sz, other.sz);
      std::swap(capacity, other.capacity);
      std::swap(allocated, other.allocated);
    }
  }

  auto begin() const {
    ass<"No memory was allocated">(allocated);
    return BufferIt<T>{ p };
  }
  auto end() const {
    ass<"No memory was allocated">(allocated);
    return BufferIt<T>{ p + sz };
  }

  void realloc(std::size_t new_size) {
    T *temp;
    capacity = static_cast<int>(new_size * 1.15);
    temp = new T[capacity];
    for (std::size_t i = 0; i < sz; i++)
      temp[i] = p[i];

    for (std::size_t i = sz; i < new_size; i++)
      temp[i] = T();

    delete[] p;
    p = temp;

    sz = new_size;
    allocated = true;
    temp = nullptr;
  }

  void push_back(T input) {
    if (sz == capacity) {
      int szOld = sz;
      if (sz == 0) {
        realloc(1);
      } else if (sz > 0) {
        realloc(sz * 2);
      } else {
        ass<"negative size found.">(false);
      }
      capacity = sz;
      p[szOld] = input;
      sz = szOld + 1;
    } else if (sz < capacity) {
      p[sz] = input;
      sz++;
    }
  }
};

//==============================================================================
// SoA specialization for the simple scalar types (Double / Integer / Logical).
// Storage is split into a raw data array (p_val) and a parallel NA-flag array
// (p_na). This lets callers hand `p_val` directly to numeric kernels that
// expect a plain double* / int* / bool* buffer.
//==============================================================================
template <typename Scalar, typename BufferTrait>
struct SoABuffer {
  using value_type = Scalar;
  using raw_type   = from_ast_scalar_t<Scalar>;
  using Trait      = BufferTrait;
  using TypeTrait  = BufferTrait;

  raw_type*   p_val = nullptr;
  bool*       p_na  = nullptr;
  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;

  std::size_t size() const noexcept { return allocated ? sz : 0; }

  raw_type*       data()       noexcept { return p_val; }
  const raw_type* data() const noexcept { return p_val; }
  bool*           na_data()       noexcept { return p_na; }
  const bool*     na_data() const noexcept { return p_na; }

  void reset() noexcept {
    p_val = nullptr;
    p_na  = nullptr;
    sz = 0;
    capacity = 0;
    allocated = false;
  }

  SoABuffer() = default;

  SoABuffer(const SoABuffer& other)
  : sz(other.sz), capacity(other.capacity), allocated(other.allocated) {
    if (!allocated || capacity == 0) { reset(); return; }
    ass<"Buffer invariant violated: sz > capacity">(!(sz > capacity));
    p_val = new raw_type[capacity];
    p_na  = new bool[capacity];
    std::copy_n(other.p_val, sz, p_val);
    std::copy_n(other.p_na,  sz, p_na);
  }

  SoABuffer(SoABuffer&& other) noexcept
  : p_val(std::exchange(other.p_val, nullptr)),
    p_na (std::exchange(other.p_na,  nullptr)),
    sz   (std::exchange(other.sz, 0)),
    capacity(std::exchange(other.capacity, 0)),
    allocated(std::exchange(other.allocated, false)) {}

  SoABuffer& operator=(const SoABuffer& other) {
    if (this == &other) return *this;
    if (other.size() > capacity) resize(other.size());
    sz = other.size();
    if (sz) {
      std::copy_n(other.p_val, sz, p_val);
      std::copy_n(other.p_na,  sz, p_na);
    }
    return *this;
  }

  SoABuffer& operator=(SoABuffer&& other) noexcept {
    swap(other);
    return *this;
  }

  SoABuffer(std::size_t sz_) {
    ass<"Size has to be larger than 0!">(sz_ > 0);
    init(sz_);
  }

  ~SoABuffer() {
    if (allocated) {
      delete[] p_val;
      delete[] p_na;
      reset();
    }
  }

  void init(std::size_t size_) {
    if (allocated) {
      ass<"try to delete nullptr">(p_val != nullptr);
      delete[] p_val;
      delete[] p_na;
      p_val = nullptr;
      p_na  = nullptr;
    }
    sz = size_;
    capacity = static_cast<std::size_t>(1.15 * static_cast<double>(sz));
    p_val = new raw_type[capacity];
    p_na  = new bool[capacity];
    fill(Scalar());
    allocated = true;
  }

  void realloc(std::size_t new_size) {
    std::size_t new_capacity = static_cast<std::size_t>(new_size * 1.15);
    raw_type* new_val = new raw_type[new_capacity];
    bool*     new_na  = new bool[new_capacity];

    const std::size_t keep = std::min(sz, new_size);
    for (std::size_t i = 0; i < keep; ++i) {
      new_val[i] = p_val[i];
      new_na[i]  = p_na[i];
    }
    for (std::size_t i = keep; i < new_size; ++i) {
      new_val[i] = raw_type{};
      new_na[i]  = false;
    }

    delete[] p_val;
    delete[] p_na;
    p_val = new_val;
    p_na  = new_na;
    capacity = new_capacity;
    sz = new_size;
    allocated = true;
  }

  void resize(std::size_t newSize) {
    ass<"Size has to be larger than 0!">(newSize > 0);
    if (!allocated) {
      init(newSize);
      return;
    }
    if (newSize > capacity) {
      realloc(newSize);
      // Newly grown region is already zero-initialised by realloc.
      return;
    }
    sz = newSize;
  }

  template <typename V>
  requires (IsCppArithV<Decayed<V>> || IsScalarOrScalarRef<Decayed<V>>
            || IS<Decayed<V>, Scalar>)
  void fill(V&& v) {
    raw_type base_val;
    bool     base_na;
    if constexpr (IS<Decayed<V>, Scalar>) {
      base_val = v.val;
      base_na  = v.is_na;
    } else if constexpr (IsScalarOrScalarRef<Decayed<V>>) {
      Scalar tmp = static_cast<Scalar>(get_val(v));
      base_val = tmp.val;
      base_na  = tmp.is_na;
    } else {
      base_val = static_cast<raw_type>(v);
      base_na  = false;
    }
    std::fill(p_val, p_val + sz, base_val);
    std::fill(p_na,  p_na  + sz, base_na);
  }

  value_type get(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    Scalar s;
    s.val   = p_val[idx];
    s.is_na = p_na[idx];
    return s;
  }

  void set(std::size_t idx, const value_type& val) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p_val[idx] = val.val;
    p_na[idx]  = val.is_na;
  }

  void push_back(const value_type& input) {
    if (sz == capacity) {
      const std::size_t szOld = sz;
      realloc(sz == 0 ? 1 : sz * 2);
      capacity = sz;
      sz = szOld;
    }
    p_val[sz] = input.val;
    p_na[sz]  = input.is_na;
    ++sz;
  }

  template <typename L2> void moveit(L2& other) {
    std::swap(p_val,     other.p_val);
    std::swap(p_na,      other.p_na);
    std::swap(sz,        other.sz);
    std::swap(capacity,  other.capacity);
    std::swap(allocated, other.allocated);
    allocated = true;
  }

  void swap(SoABuffer& other) noexcept {
    if (this == &other) return;
    std::swap(p_val,     other.p_val);
    std::swap(p_na,      other.p_na);
    std::swap(sz,        other.sz);
    std::swap(capacity,  other.capacity);
    std::swap(allocated, other.allocated);
  }

  struct Iterator {
    const raw_type* v;
    const bool*     n;
    std::size_t     idx;

    using value_type        = Scalar;
    using reference         = Scalar;
    using difference_type   = std::ptrdiff_t;
    using iterator_category = std::forward_iterator_tag;

    reference operator*() const {
      Scalar s;
      s.val   = v[idx];
      s.is_na = n[idx];
      return s;
    }
    Iterator& operator++() { ++idx; return *this; }
    bool operator!=(const Iterator& o) const { return idx != o.idx; }
  };

  Iterator begin() const {
    ass<"No memory was allocated">(allocated);
    return Iterator{ p_val, p_na, 0 };
  }
  Iterator end() const {
    ass<"No memory was allocated">(allocated);
    return Iterator{ p_val, p_na, sz };
  }
};

//------------------------------------------------------------------------------
// Buffer<Double, Trait>
//------------------------------------------------------------------------------
template <typename BufferTrait>
struct Buffer<Double, BufferTrait> : SoABuffer<Double, BufferTrait> {
  using Base = SoABuffer<Double, BufferTrait>;
  using Base::Base;
  using Base::p_val;
  using Base::p_na;
  using Base::sz;
  using Base::capacity;
  using Base::allocated;
  using Base::reset;
  using typename Base::value_type;

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s) {
    if (allocated) {
      delete[] p_val;
      delete[] p_na;
      reset();
    }
    ass<"R object is not of type numeric">(Rf_isReal(s));
    sz = static_cast<std::size_t>(Rf_length(s));
    ass<"R object seems to be empty">(sz >= 1);
    capacity = sz;
    p_val = new double[capacity];
    p_na  = new bool[capacity];
    const double* src = REAL(s);
    for (std::size_t i = 0; i < sz; ++i) {
      p_val[i] = src[i];
      p_na[i]  = ISNA(src[i]);
    }
    allocated = true;
  }
  Buffer(SEXP s) { initSEXP(s); }
#endif
};

//------------------------------------------------------------------------------
// Buffer<Integer, Trait>
//------------------------------------------------------------------------------
template <typename BufferTrait>
struct Buffer<Integer, BufferTrait> : SoABuffer<Integer, BufferTrait> {
  using Base = SoABuffer<Integer, BufferTrait>;
  using Base::Base;
  using Base::p_val;
  using Base::p_na;
  using Base::sz;
  using Base::capacity;
  using Base::allocated;
  using Base::reset;
  using typename Base::value_type;

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s) {
    if (allocated) {
      delete[] p_val;
      delete[] p_na;
      reset();
    }
    ass<"R object is not of type integer">(Rf_isInteger(s));
    sz = static_cast<std::size_t>(Rf_length(s));
    ass<"R object seems to be empty">(sz >= 1);
    capacity = sz;
    p_val = new int[capacity];
    p_na  = new bool[capacity];
    const int* src = INTEGER(s);
    for (std::size_t i = 0; i < sz; ++i) {
      p_val[i] = src[i];
      p_na[i]  = (src[i] == NA_INTEGER);
    }
    allocated = true;
  }
  Buffer(SEXP s) { initSEXP(s); }
#endif
};

//------------------------------------------------------------------------------
// Buffer<Logical, Trait>
//------------------------------------------------------------------------------
template <typename BufferTrait>
struct Buffer<Logical, BufferTrait> : SoABuffer<Logical, BufferTrait> {
  using Base = SoABuffer<Logical, BufferTrait>;
  using Base::Base;
  using Base::p_val;
  using Base::p_na;
  using Base::sz;
  using Base::capacity;
  using Base::allocated;
  using Base::reset;
  using typename Base::value_type;

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s) {
    if (allocated) {
      delete[] p_val;
      delete[] p_na;
      reset();
    }
    ass<"R object is not of type logical">(Rf_isLogical(s));
    sz = static_cast<std::size_t>(Rf_length(s));
    ass<"R object seems to be empty">(sz >= 1);
    capacity = sz;
    p_val = new bool[capacity];
    p_na  = new bool[capacity];
    const int* src = LOGICAL(s);
    for (std::size_t i = 0; i < sz; ++i) {
      p_na[i]  = (src[i] == NA_LOGICAL);
      p_val[i] = p_na[i] ? false : static_cast<bool>(src[i]);
    }
    allocated = true;
  }
  Buffer(SEXP s) { initSEXP(s); }
#endif
};

//==============================================================================
// Buffer<Dual, Trait>
//
// Dual carries both a value and its derivative ("dot"), each with its own
// NA flag, so we store four parallel arrays.
//==============================================================================
template <typename BufferTrait>
struct Buffer<Dual, BufferTrait> {
  using value_type = Dual;
  using Trait      = BufferTrait;
  using TypeTrait  = BufferTrait;

  double* p_val     = nullptr;
  bool*   p_na      = nullptr;
  double* p_dot     = nullptr;
  bool*   p_na_dot  = nullptr;
  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;

  std::size_t size() const noexcept { return allocated ? sz : 0; }

  double*       data()          noexcept { return p_val; }
  const double* data()    const noexcept { return p_val; }
  double*       dot_data()      noexcept { return p_dot; }
  const double* dot_data() const noexcept { return p_dot; }
  bool*         na_data()         noexcept { return p_na; }
  const bool*   na_data()   const noexcept { return p_na; }
  bool*         dot_na_data()       noexcept { return p_na_dot; }
  const bool*   dot_na_data() const noexcept { return p_na_dot; }

  void reset() noexcept {
    p_val = nullptr; p_dot = nullptr;
    p_na  = nullptr; p_na_dot = nullptr;
    sz = 0; capacity = 0; allocated = false;
  }

  Buffer() = default;

  Buffer(const Buffer& other)
  : sz(other.sz), capacity(other.capacity), allocated(other.allocated) {
    if (!allocated || capacity == 0) { reset(); return; }
    ass<"Buffer invariant violated: sz > capacity">(!(sz > capacity));
    p_val    = new double[capacity];
    p_dot    = new double[capacity];
    p_na     = new bool[capacity];
    p_na_dot = new bool[capacity];
    for (std::size_t i = 0; i < sz; ++i) {
      p_val[i]    = other.p_val[i];
      p_dot[i]    = other.p_dot[i];
      p_na[i]     = other.p_na[i];
      p_na_dot[i] = other.p_na_dot[i];
    }
  }

  Buffer(Buffer&& other) noexcept
  : p_val   (std::exchange(other.p_val,    nullptr)),
    p_na    (std::exchange(other.p_na,     nullptr)),
    p_dot   (std::exchange(other.p_dot,    nullptr)),
    p_na_dot(std::exchange(other.p_na_dot, nullptr)),
    sz      (std::exchange(other.sz, 0)),
    capacity(std::exchange(other.capacity, 0)),
    allocated(std::exchange(other.allocated, false)) {}

  Buffer& operator=(const Buffer& other) {
    if (this == &other) return *this;
    if (other.size() > capacity) resize(other.size());
    sz = other.size();
    for (std::size_t i = 0; i < sz; ++i) {
      p_val[i]    = other.p_val[i];
      p_dot[i]    = other.p_dot[i];
      p_na[i]     = other.p_na[i];
      p_na_dot[i] = other.p_na_dot[i];
    }
    return *this;
  }

  Buffer& operator=(Buffer&& other) noexcept {
    swap(other);
    return *this;
  }

  Buffer(std::size_t sz_) {
    ass<"Size has to be larger than 0!">(sz_ > 0);
    init(sz_);
  }

  ~Buffer() {
    if (allocated) {
      delete[] p_val;
      delete[] p_dot;
      delete[] p_na;
      delete[] p_na_dot;
      reset();
    }
  }

  void init(std::size_t size_) {
    if (allocated) {
      delete[] p_val;    p_val    = nullptr;
      delete[] p_dot;    p_dot    = nullptr;
      delete[] p_na;     p_na     = nullptr;
      delete[] p_na_dot; p_na_dot = nullptr;
    }
    sz = size_;
    capacity = static_cast<std::size_t>(1.15 * static_cast<double>(sz));
    p_val    = new double[capacity];
    p_dot    = new double[capacity];
    p_na     = new bool[capacity];
    p_na_dot = new bool[capacity];
    fill(Dual());
    allocated = true;
  }

  void realloc(std::size_t new_size) {
    std::size_t new_capacity = static_cast<std::size_t>(new_size * 1.15);
    double* nv   = new double[new_capacity];
    double* nd   = new double[new_capacity];
    bool*   nn   = new bool[new_capacity];
    bool*   nnd  = new bool[new_capacity];

    const std::size_t keep = std::min(sz, new_size);
    for (std::size_t i = 0; i < keep; ++i) {
      nv[i]  = p_val[i];
      nd[i]  = p_dot[i];
      nn[i]  = p_na[i];
      nnd[i] = p_na_dot[i];
    }
    for (std::size_t i = keep; i < new_size; ++i) {
      nv[i]  = 0.0;
      nd[i]  = 0.0;
      nn[i]  = false;
      nnd[i] = false;
    }

    delete[] p_val;    delete[] p_dot;
    delete[] p_na;     delete[] p_na_dot;
    p_val    = nv;
    p_dot    = nd;
    p_na     = nn;
    p_na_dot = nnd;
    capacity = new_capacity;
    sz = new_size;
    allocated = true;
  }

  void resize(std::size_t newSize) {
    ass<"Size has to be larger than 0!">(newSize > 0);
    if (!allocated) { init(newSize); return; }
    if (newSize > capacity) { realloc(newSize); return; }
    sz = newSize;
  }

  template <typename V>
  requires (IsCppArithV<Decayed<V>> || IsScalarOrScalarRef<Decayed<V>>
            || IS<Decayed<V>, Dual>)
  void fill(V&& v) {
    Dual d = [&] {
      if constexpr (IS<Decayed<V>, Dual>) return v;
      else if constexpr (IsScalarOrScalarRef<Decayed<V>>) return static_cast<Dual>(get_val(v));
      else return Dual(static_cast<double>(v), 0.0);
    }();
    std::fill(p_val,    p_val    + sz, d.val);
    std::fill(p_dot,    p_dot    + sz, d.dot);
    std::fill(p_na,     p_na     + sz, d.is_na);
    std::fill(p_na_dot, p_na_dot + sz, d.is_na_dot);
  }

  value_type get(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    Dual d(p_val[idx], p_dot[idx]);
    d.is_na     = p_na[idx];
    d.is_na_dot = p_na_dot[idx];
    return d;
  }
  Double get_dot(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    Double out(p_dot[idx]);
    out.is_na = p_na_dot[idx];
    return out;
  }

  void set(std::size_t idx, const value_type& val) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p_val[idx]    = val.val;
    p_dot[idx]    = val.dot;
    p_na[idx]     = val.is_na;
    p_na_dot[idx] = val.is_na_dot;
  }
  void set_dot(std::size_t idx, const double& val_dot) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p_dot[idx]    = val_dot;
    p_na_dot[idx] = false;
  }

  void push_back(const Dual& input) {
    if (sz == capacity) {
      const std::size_t szOld = sz;
      realloc(sz == 0 ? 1 : sz * 2);
      capacity = sz;
      sz = szOld;
    }
    p_val[sz]    = input.val;
    p_dot[sz]    = input.dot;
    p_na[sz]     = input.is_na;
    p_na_dot[sz] = input.is_na_dot;
    ++sz;
  }

  template <typename L2> void moveit(L2& other) {
    std::swap(p_val,    other.p_val);
    std::swap(p_dot,    other.p_dot);
    std::swap(p_na,     other.p_na);
    std::swap(p_na_dot, other.p_na_dot);
    std::swap(sz,       other.sz);
    std::swap(capacity, other.capacity);
    std::swap(allocated,other.allocated);
    allocated = true;
  }

  void swap(Buffer& other) noexcept {
    if (this == &other) return;
    std::swap(p_val,    other.p_val);
    std::swap(p_dot,    other.p_dot);
    std::swap(p_na,     other.p_na);
    std::swap(p_na_dot, other.p_na_dot);
    std::swap(sz,       other.sz);
    std::swap(capacity, other.capacity);
    std::swap(allocated,other.allocated);
  }

  struct Iterator {
    const double* v;
    const double* d;
    const bool*   nv;
    const bool*   nd;
    std::size_t   idx;

    using value_type        = Dual;
    using reference         = Dual;
    using difference_type   = std::ptrdiff_t;
    using iterator_category = std::forward_iterator_tag;

    Dual operator*() const {
      Dual out(v[idx], d[idx]);
      out.is_na     = nv[idx];
      out.is_na_dot = nd[idx];
      return out;
    }
    Iterator& operator++() { ++idx; return *this; }
    bool operator!=(const Iterator& o) const { return idx != o.idx; }
  };

  Iterator begin() const {
    ass<"No memory was allocated">(allocated);
    return Iterator{ p_val, p_dot, p_na, p_na_dot, 0 };
  }
  Iterator end() const {
    ass<"No memory was allocated">(allocated);
    return Iterator{ p_val, p_dot, p_na, p_na_dot, sz };
  }

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s) {
    if (allocated) {
      delete[] p_val;    delete[] p_dot;
      delete[] p_na;     delete[] p_na_dot;
      reset();
    }
    ass<"R object is not of type numeric">(Rf_isReal(s));
    sz = static_cast<std::size_t>(Rf_length(s));
    ass<"R object seems to be empty">(sz >= 1);
    capacity = sz;
    p_val    = new double[capacity];
    p_dot    = new double[capacity];
    p_na     = new bool[capacity];
    p_na_dot = new bool[capacity];
    const double* src = REAL(s);
    for (std::size_t i = 0; i < sz; ++i) {
      p_val[i]    = src[i];
      p_dot[i]    = 0.0;
      p_na[i]     = ISNA(src[i]);
      p_na_dot[i] = false;
    }
    allocated = true;
  }
  Buffer(SEXP s) { initSEXP(s); }
#endif
};

} // namespace etr

#endif
