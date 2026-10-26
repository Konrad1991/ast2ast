#ifndef BUFFER_ETR_H
#define BUFFER_ETR_H

namespace etr {

// Index-list buffer for preserving-subset views: the only non-wrapper Buffer.
// Holds subset positions as plain ints. No L/R buffer trait -- an index list is
// never an expression-template operand. The primary template stays undefined,
// so Buffer<T> with any other unspecialized T is a compile error.
//
// Raw-pointer storage (not std::vector<int>): the only real construction site
// is create_indices's `out` buffer, which fully overwrites every slot in the
// very next loop -- std::vector::resize/assign would zero-fill first for no
// reason (a guarantee it can't skip), so `new int[n]` (genuinely
// uninitialized for a built-in type) avoids that wasted write.
template <> struct Buffer<int, LBufferTrait> {

  using value_type = int;

  int* p = nullptr;
  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;

  std::size_t size() const noexcept { return allocated ? sz : 0; }

  void reset() noexcept { p = nullptr; sz = 0; capacity = 0; allocated = false; }

  Buffer() {}
  Buffer(const Buffer& other) : sz(other.sz), capacity(other.capacity), allocated(other.allocated) {
    if (!allocated || capacity == 0) { reset(); return; }
    p = new int[capacity];
    std::copy_n(other.p, sz, p);
  }
  Buffer(Buffer&& other) noexcept
  : p(std::exchange(other.p, nullptr)),
    sz(std::exchange(other.sz, 0)),
    capacity(std::exchange(other.capacity, 0)),
    allocated(std::exchange(other.allocated, false)) {}
  Buffer& operator=(const Buffer& other) {
    if (this == &other) return *this;
    if (other.size() > capacity) {
      delete[] p;
      p = other.size() ? new int[other.size()] : nullptr;
      capacity = other.size();
    }
    sz = other.size();
    if (sz) std::copy_n(other.p, sz, p);
    allocated = other.allocated;
    return *this;
  }
  Buffer& operator=(Buffer&& other) noexcept {
    if (this == &other) return *this;
    swap(other);
    return *this;
  }
  Buffer(std::size_t sz_) { init(sz_); }

  ~Buffer() { delete[] p; }

  void init(std::size_t size) {
    delete[] p;
    p = new int[size];
    sz = size;
    capacity = size;
    allocated = true;
  }

  void resize(std::size_t newSize) {
    if (newSize > capacity) {
      int* new_p = new int[newSize];
      std::copy_n(p, sz, new_p);
      delete[] p;
      p = new_p;
      capacity = newSize;
    }
    sz = newSize;
    allocated = true;
  }

  void realloc(std::size_t new_size) {
    ass<"realloc size has to be > 0">(new_size > 0);
    if (!allocated) { init(new_size); return; }
    resize(new_size);
  }

  value_type get(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < size());
    return p[idx];
  }

  void set(std::size_t idx, const value_type& val) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < size());
    p[idx] = val;
  }

  // Skip both checks -- for callers (e.g. the subsetting offset pipeline)
  // that already proved idx is valid once, up front, for the whole buffer.
  value_type get_unchecked(std::size_t idx) const { return p[idx]; }
  void set_unchecked(std::size_t idx, const value_type& val) { p[idx] = val; }

  template <typename L2> void moveit(L2& other) {
    swap(other);
    allocated = true;
  }

  void swap(Buffer& other) noexcept {
    std::swap(p, other.p);
    std::swap(sz, other.sz);
    std::swap(capacity, other.capacity);
    std::swap(allocated, other.allocated);
  }

  struct iterator {
    const Buffer* ptr = nullptr;
    std::size_t idx = 0;
    iterator(const Buffer* p, std::size_t i) : ptr(p), idx(i) {}
    value_type operator*() const { return ptr->get(idx); }
    iterator& operator++() { ++idx; return *this; }
    bool operator!=(const iterator& other) const { return idx != other.idx; }
  };
  auto begin() const { return iterator(this, 0); }
  auto end() const { return iterator(this, size()); }

  template <typename T> void push_back(T input) {
    if (sz == capacity) {
      const std::size_t new_cap = capacity == 0 ? 1 : capacity * 2;
      int* new_p = new int[new_cap];
      std::copy_n(p, sz, new_p);
      delete[] p;
      p = new_p;
      capacity = new_cap;
    }
    p[sz++] = static_cast<int>(get_val(input));
    allocated = true;
  }
};

//==============================================================================
// Buffer<ReverseDouble>: one ReverseDouble handle (id + is_na, 8 bytes) per
// element, like Stan's std::vector<var>. get() returns a reference so
// `at(a, i) = expr` rebinds the stored handle's tape id via the defaulted
// ReverseDouble::operator= — which is what reverse-mode AD needs.
//==============================================================================
template <typename BufferTrait> struct Buffer<ReverseDouble, BufferTrait> {
  using value_type = ReverseDouble;
  using Trait      = BufferTrait;
  using TypeTrait  = BufferTrait;

  std::vector<ReverseDouble> data;
  bool allocated = false;

  std::size_t size() const noexcept { return allocated ? data.size() : 0; }

  Buffer() {}
  Buffer(const Buffer& other) : data(other.data), allocated(other.allocated) {}
  Buffer(Buffer&& other) noexcept
  : data(std::move(other.data)), allocated(other.allocated) { other.allocated = false; }
  Buffer& operator=(const Buffer& other) {
    if (this == &other) return *this;
    data = other.data;
    allocated = other.allocated;
    return *this;
  }
  Buffer& operator=(Buffer&& other) noexcept {
    if (this == &other) return *this;
    data = std::move(other.data);
    allocated = other.allocated;
    other.allocated = false;
    return *this;
  }
  Buffer(std::size_t sz_) { init(sz_); }

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s) {
    ass<"R object is not of type numeric">(Rf_isReal(s));
    std::size_t sz_ = static_cast<std::size_t>(Rf_length(s));
    data.clear();
    data.reserve(sz_);
    for (std::size_t i = 0; i < sz_; i++) data.emplace_back(REAL(s)[i]);
    allocated = true;
  }
  Buffer(SEXP s) { initSEXP(s); }
#endif

  template <typename TInp> requires IS<TInp, ReverseDouble>
  void fill(TInp&& val) {
    for (std::size_t i = 0; i < data.size(); i++) data[i] = val;
    allocated = true;
  }

  ~Buffer() { data.clear(); }

  // Each default-constructed ReverseDouble pushes its own const-0 node, so
  // every element starts on an independent tape node.
  void init(std::size_t size) {
    data.clear();
    data.resize(size);   // resize value-inits each element separately -> own node
    allocated = true;
  }

  void resize(std::size_t newSize) {
    data.resize(newSize);
    allocated = true;
  }

  void realloc(std::size_t new_size) {
    ass<"realloc size has to be > 0">(new_size > 0);
    if (!allocated) { init(new_size); return; }
    resize(new_size);
  }

  ReverseDouble& get(std::size_t idx) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < size());
    return data[idx];
  }
  const ReverseDouble& get(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < size());
    return data[idx];
  }
  // rebind: element idx now refers to val's tape node (edges preserved)
  void set(std::size_t idx, const ReverseDouble& val) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < size());
    data[idx] = val;
  }

  // Skip both checks -- for callers that already proved idx is valid once,
  // up front, for the whole buffer.
  ReverseDouble& get_unchecked(std::size_t idx) { return data[idx]; }
  const ReverseDouble& get_unchecked(std::size_t idx) const { return data[idx]; }
  void set_unchecked(std::size_t idx, const ReverseDouble& val) { data[idx] = val; }

  template <typename L2> void moveit(L2& other) {
    data.swap(other.data);
    std::swap(allocated, other.allocated);
    allocated = true;
  }

  void swap(Buffer& other) noexcept {
    data.swap(other.data);
    std::swap(allocated, other.allocated);
  }

  struct iterator {
    const Buffer* ptr = nullptr;
    std::size_t idx = 0;
    iterator(const Buffer* p, std::size_t i) : ptr(p), idx(i) {}
    value_type operator*() const { return ptr->get(idx); }
    iterator& operator++() { ++idx; return *this; }
    bool operator!=(const iterator& other) const { return idx != other.idx; }
  };
  auto begin() const { return iterator(this, 0); }
  auto end()   const { return iterator(this, size()); }

  template <typename T> void push_back(T input) {
    data.emplace_back(input);
    allocated = true;
  }
};

//==============================================================================
// Buffer for the simple scalar types (Double / Integer / Logical).
// One raw array in R's layout (double* / int*); NA is R's sentinel inside the
// data, so `p_val` goes directly to numeric kernels and to/from SEXPs.
//==============================================================================
template <typename Scalar, typename BufferTrait>
struct SoABuffer {
  using value_type = Scalar;
  using raw_type   = from_ast_scalar_t<Scalar>;
  using Trait      = BufferTrait;
  using TypeTrait  = BufferTrait;

  raw_type*   p_val = nullptr;
  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;

  std::size_t size() const noexcept { return allocated ? sz : 0; }

  raw_type*       data()       noexcept { return p_val; }
  const raw_type* data() const noexcept { return p_val; }

  void reset() noexcept {
    p_val = nullptr;
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
    std::copy_n(other.p_val, sz, p_val);
  }

  SoABuffer(SoABuffer&& other) noexcept
  : p_val(std::exchange(other.p_val, nullptr)),
    sz   (std::exchange(other.sz, 0)),
    capacity(std::exchange(other.capacity, 0)),
    allocated(std::exchange(other.allocated, false)) {}

  SoABuffer& operator=(const SoABuffer& other) {
    if (this == &other) return *this;
    if (other.size() > capacity) resize(other.size());
    sz = other.size();
    if (sz) {
      std::copy_n(other.p_val, sz, p_val);
    }
    return *this;
  }

  SoABuffer& operator=(SoABuffer&& other) noexcept {
    swap(other);
    return *this;
  }

  SoABuffer(std::size_t sz_) {
    init(sz_);
  }

  ~SoABuffer() {
    if (allocated) {
      delete[] p_val;
      reset();
    }
  }

  void init(std::size_t size_) {
    if (allocated) {
      ass<"try to delete nullptr">(p_val != nullptr);
      delete[] p_val;
      p_val = nullptr;
    }
    sz = size_;
    capacity = static_cast<std::size_t>(1.15 * static_cast<double>(sz));
    p_val = new raw_type[capacity];
    fill(Scalar());
    allocated = true;
  }

  void realloc(std::size_t new_size) {
    std::size_t new_capacity = static_cast<std::size_t>(new_size * 1.15);
    raw_type* new_val = new raw_type[new_capacity];

    const std::size_t keep = std::min(sz, new_size);
    for (std::size_t i = 0; i < keep; ++i) {
      new_val[i] = p_val[i];
    }
    for (std::size_t i = keep; i < new_size; ++i) {
      new_val[i] = raw_type{};
    }

    delete[] p_val;
    p_val = new_val;
    capacity = new_capacity;
    sz = new_size;
    allocated = true;
  }

  void resize(std::size_t newSize) {
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
    if constexpr (IS<Decayed<V>, Scalar>) {
      base_val = v.val;
    } else if constexpr (IsScalarOrScalarRef<Decayed<V>>) {
      base_val = Scalar(get_scalar_val(v)).val;
    } else {
      base_val = static_cast<raw_type>(v);
    }
    std::fill(p_val, p_val + sz, base_val);
  }

  value_type get(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    Scalar s;
    s.val = p_val[idx];
    return s;
  }

  void set(std::size_t idx, const value_type& val) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p_val[idx] = val.val;
  }

  // Skip both checks -- for callers that already proved idx is valid once,
  // up front, for the whole buffer.
  value_type get_unchecked(std::size_t idx) const {
    Scalar s;
    s.val = p_val[idx];
    return s;
  }
  void set_unchecked(std::size_t idx, const value_type& val) {
    p_val[idx] = val.val;
  }

  void push_back(const value_type& input) {
    if (sz == capacity) {
      const std::size_t szOld = sz;
      realloc(sz == 0 ? 1 : sz * 2);
      capacity = sz;
      sz = szOld;
    }
    p_val[sz] = input.val;
    ++sz;
  }

  template <typename L2> void moveit(L2& other) {
    std::swap(p_val,     other.p_val);
    std::swap(sz,        other.sz);
    std::swap(capacity,  other.capacity);
    std::swap(allocated, other.allocated);
    allocated = true;
  }

  void swap(SoABuffer& other) noexcept {
    if (this == &other) return;
    std::swap(p_val,     other.p_val);
    std::swap(sz,        other.sz);
    std::swap(capacity,  other.capacity);
    std::swap(allocated, other.allocated);
  }

  struct Iterator {
    const raw_type* v;
    std::size_t     idx;

    using value_type        = Scalar;
    using reference         = Scalar;
    using difference_type   = std::ptrdiff_t;
    using iterator_category = std::forward_iterator_tag;

    reference operator*() const {
      Scalar s;
      s.val = v[idx];
      return s;
    }
    Iterator& operator++() { ++idx; return *this; }
    bool operator!=(const Iterator& o) const { return idx != o.idx; }
  };

  Iterator begin() const {
    ass<"No memory was allocated">(allocated);
    return Iterator{ p_val, 0 };
  }
  Iterator end() const {
    ass<"No memory was allocated">(allocated);
    return Iterator{ p_val, sz };
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
      reset();
    }
    ass<"R object is not of type numeric">(Rf_isReal(s));
    sz = static_cast<std::size_t>(Rf_length(s));
    capacity = sz;
    p_val = new double[capacity];
    // R-devel returns dummy pointer 0x1 for zero-length vectors (UBSan)
    if (sz > 0) std::copy_n(REAL(s), sz, p_val);
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
      reset();
    }
    ass<"R object is not of type integer">(Rf_isInteger(s));
    sz = static_cast<std::size_t>(Rf_length(s));
    capacity = sz;
    p_val = new int[capacity];
    if (sz > 0) std::copy_n(INTEGER(s), sz, p_val);
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
      reset();
    }
    ass<"R object is not of type logical">(Rf_isLogical(s));
    sz = static_cast<std::size_t>(Rf_length(s));
    capacity = sz;
    p_val = new int[capacity];
    if (sz > 0) std::copy_n(LOGICAL(s), sz, p_val);
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

  // NA is na_real() inside p_val / p_dot (R's layout, no flag arrays)
  double* p_val     = nullptr;
  double* p_dot     = nullptr;
  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;

  std::size_t size() const noexcept { return allocated ? sz : 0; }

  double*       data()          noexcept { return p_val; }
  const double* data()    const noexcept { return p_val; }
  double*       dot_data()      noexcept { return p_dot; }
  const double* dot_data() const noexcept { return p_dot; }

  void reset() noexcept {
    p_val = nullptr; p_dot = nullptr;
    sz = 0; capacity = 0; allocated = false;
  }

  Buffer() = default;

  Buffer(const Buffer& other)
  : sz(other.sz), capacity(other.capacity), allocated(other.allocated) {
    if (!allocated || capacity == 0) { reset(); return; }
    ass<"Buffer invariant violated: sz > capacity">(!(sz > capacity));
    p_val    = new double[capacity];
    p_dot    = new double[capacity];
    std::copy_n(other.p_val, sz, p_val);
    std::copy_n(other.p_dot, sz, p_dot);
  }

  Buffer(Buffer&& other) noexcept
  : p_val   (std::exchange(other.p_val,    nullptr)),
    p_dot   (std::exchange(other.p_dot,    nullptr)),
    sz      (std::exchange(other.sz, 0)),
    capacity(std::exchange(other.capacity, 0)),
    allocated(std::exchange(other.allocated, false)) {}

  Buffer& operator=(const Buffer& other) {
    if (this == &other) return *this;
    if (other.size() > capacity) resize(other.size());
    sz = other.size();
    std::copy_n(other.p_val, sz, p_val);
    std::copy_n(other.p_dot, sz, p_dot);
    return *this;
  }

  Buffer& operator=(Buffer&& other) noexcept {
    swap(other);
    return *this;
  }

  Buffer(std::size_t sz_) {
    init(sz_);
  }

  ~Buffer() {
    if (allocated) {
      delete[] p_val;
      delete[] p_dot;
      reset();
    }
  }

  void init(std::size_t size_) {
    if (allocated) {
      delete[] p_val;    p_val    = nullptr;
      delete[] p_dot;    p_dot    = nullptr;
    }
    sz = size_;
    capacity = static_cast<std::size_t>(1.15 * static_cast<double>(sz));
    p_val    = new double[capacity];
    p_dot    = new double[capacity];
    fill(Dual());
    allocated = true;
  }

  void realloc(std::size_t new_size) {
    std::size_t new_capacity = static_cast<std::size_t>(new_size * 1.15);
    double* nv   = new double[new_capacity];
    double* nd   = new double[new_capacity];

    const std::size_t keep = std::min(sz, new_size);
    for (std::size_t i = 0; i < keep; ++i) {
      nv[i]  = p_val[i];
      nd[i]  = p_dot[i];
    }
    for (std::size_t i = keep; i < new_size; ++i) {
      nv[i]  = 0.0;
      nd[i]  = 0.0;
    }

    delete[] p_val;    delete[] p_dot;
    p_val    = nv;
    p_dot    = nd;
    capacity = new_capacity;
    sz = new_size;
    allocated = true;
  }

  void resize(std::size_t newSize) {
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
      else if constexpr (IsScalarOrScalarRef<Decayed<V>>) return Dual(get_scalar_val(v));
      else return Dual(static_cast<double>(v), 0.0);
    }();
    std::fill(p_val,    p_val    + sz, d.val);
    std::fill(p_dot,    p_dot    + sz, d.dot);
  }

  value_type get(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    return Dual(p_val[idx], p_dot[idx]);
  }
  Double get_dot(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    return Double(p_dot[idx]);
  }

  void set(std::size_t idx, const value_type& val) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p_val[idx]    = val.val;
    p_dot[idx]    = val.dot;
  }

  // Skip both checks -- for callers that already proved idx is valid once,
  // up front, for the whole buffer.
  value_type get_unchecked(std::size_t idx) const {
    return Dual(p_val[idx], p_dot[idx]);
  }
  void set_unchecked(std::size_t idx, const value_type& val) {
    p_val[idx]    = val.val;
    p_dot[idx]    = val.dot;
  }

  void set_dot(std::size_t idx, const double& val_dot) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p_dot[idx]    = val_dot;
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
    ++sz;
  }

  template <typename L2> void moveit(L2& other) {
    std::swap(p_val,    other.p_val);
    std::swap(p_dot,    other.p_dot);
    std::swap(sz,       other.sz);
    std::swap(capacity, other.capacity);
    std::swap(allocated,other.allocated);
    allocated = true;
  }

  void swap(Buffer& other) noexcept {
    if (this == &other) return;
    std::swap(p_val,    other.p_val);
    std::swap(p_dot,    other.p_dot);
    std::swap(sz,       other.sz);
    std::swap(capacity, other.capacity);
    std::swap(allocated,other.allocated);
  }

  struct Iterator {
    const double* v;
    const double* d;
    std::size_t   idx;

    using value_type        = Dual;
    using reference         = Dual;
    using difference_type   = std::ptrdiff_t;
    using iterator_category = std::forward_iterator_tag;

    Dual operator*() const {
      return Dual(v[idx], d[idx]);
    }
    Iterator& operator++() { ++idx; return *this; }
    bool operator!=(const Iterator& o) const { return idx != o.idx; }
  };

  Iterator begin() const {
    ass<"No memory was allocated">(allocated);
    return Iterator{ p_val, p_dot, 0 };
  }
  Iterator end() const {
    ass<"No memory was allocated">(allocated);
    return Iterator{ p_val, p_dot, sz };
  }

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s) {
    if (allocated) {
      delete[] p_val;    delete[] p_dot;
      reset();
    }
    ass<"R object is not of type numeric">(Rf_isReal(s));
    sz = static_cast<std::size_t>(Rf_length(s));
    capacity = sz;
    p_val    = new double[capacity];
    p_dot    = new double[capacity];
    if (sz > 0) std::copy_n(REAL(s), sz, p_val);
    std::fill(p_dot, p_dot + sz, 0.0);
    allocated = true;
  }
  Buffer(SEXP s) { initSEXP(s); }
#endif
};

} // namespace etr

#endif
