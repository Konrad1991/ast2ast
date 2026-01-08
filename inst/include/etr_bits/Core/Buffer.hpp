#ifndef BUFFER_ETR_H
#define BUFFER_ETR_H

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

namespace etr {
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

  // empty constructor
  Buffer() {}

  // Constructors for other Buffers
  // Copy constructor
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
  // Move constructor
  Buffer(Buffer &&other) noexcept
    : p(std::exchange(other.p, nullptr)), sz(std::exchange(other.sz, 0)),
    capacity(std::exchange(other.capacity, 0)),
    allocated(std::exchange(other.allocated, false)) {}
  // copy assignment
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
  // move assignment
  Buffer &operator=(Buffer &&other) noexcept {
    swap(other);
    return *this;
  }
  // Constructors for std::size_t
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
    if constexpr (IS<value_type, Double>) {
      ass<"R object is not of type numeric">(Rf_isReal(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      ass<"R object seems to be empty">(sz >= 1);
      capacity = static_cast<std::size_t>(sz);
      p = new T[capacity];
      for (int i = 0; i < sz; i++) {
        p[i] = REAL(s)[i];
      }
      allocated = true;
    } else if constexpr (IS<value_type, Integer>) {
      ass<"R object is not of type integer">(Rf_isInteger(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      ass<"R object seems to be empty">(sz >= 1);
      capacity = static_cast<std::size_t>(sz);
      p = new T[capacity];
      for (int i = 0; i < sz; i++) {
        p[i] = INTEGER(s)[i];
      }
      allocated = true;
    } else if constexpr (IS<value_type, Logical>) {
      ass<"R object is not of type logical">(Rf_isLogical(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      ass<"R object seems to be empty">(sz >= 1);
      capacity = static_cast<std::size_t>(sz);
      p = new T[capacity];
      for (int i = 0; i < sz; i++) {
        p[i] = static_cast<bool>(LOGICAL(s)[i]);
      }
      allocated = true;
    } else {
      ass<"Unsupported type found">(false);
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
  requires(IS<TInp, Variable<Double>> || IsArithV<Decayed<TInp>>)
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
      fill(T());
      return;
    } else {
      if (newSize > capacity) {
        ass<"try to delete nullptr">(p != nullptr);
        delete[] p;
        capacity =
          static_cast<std::size_t>(static_cast<double>(newSize) * 1.15);
        p = new T[capacity];
        sz = newSize;
        allocated = true;
        fill(T());
      } else {
        sz = newSize;
        return;
      }
    }
  }

  const value_type& get(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    return p[idx];
  }
  template<typename> requires IS<T, Dual>
  const Double& get_dot(std::size_t idx) const {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    return Double(p[idx].dot);
  }
  void set(std::size_t idx, const value_type& val) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p[idx] = val;
  }
  template<typename> requires IS<T, Dual>
  void set_dot(std::size_t idx, const double& val_dot) {
    ass<"No memory was allocated">(allocated);
    ass<"Error: out of boundaries">(idx < sz);
    p[idx].dot = val_dot;
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
      T* temp_p = p;
      std::size_t temp_sz = sz;
      std::size_t temp_capacity = capacity;
      bool temp_allocated = allocated;
      p = other.p;
      other.p = temp_p;
      sz = other.sz;
      other.sz = temp_sz;
      capacity = other.capacity;
      other.capacity = temp_capacity;
      allocated = other.allocated;
      other.allocated = temp_allocated;
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

} // namespace etr

#endif
