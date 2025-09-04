#ifndef BUFFER_ETR_H
#define BUFFER_ETR_H

#include "Header.hpp"
#include "Reflection.hpp"
#include "Types.hpp"
#include "Utils.hpp"

namespace etr {
template <typename T, typename BufferTrait> struct Buffer {

  using RetType = T;
  using Type = T;
  using Trait = BufferTrait;
  using TypeTrait = BufferTrait;

  T *p = nullptr;
  std::size_t sz = 0;
  std::size_t capacity = 0;
  bool allocated = false;

  bool is_matrix = false;
  std::size_t nrow = 0;
  std::size_t ncol = 0;

  std::size_t size() const {
    if (!allocated) return 0;
    return sz;
  }
  std::size_t nr() const {
    ass<"No memory was allocated">(allocated);
    return nrow;
  }
  std::size_t nc() const {
    ass<"No memory was allocated">(allocated);
    return ncol;
  }
  bool im() const {
    ass<"No memory was allocated">(allocated);
    return is_matrix;
  }
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

  void reset() noexcept {
    this -> allocated = false;
    this -> p = nullptr;
    this -> sz = 0;
    this -> capacity = 0;
    set_matrix_default();
  }

  // empty constructor
  Buffer() {}

  // Constructors for other Buffers
  // Copy constructor
  Buffer(const Buffer &other)
    : sz(other.sz), capacity(other.capacity), allocated(other.allocated),
    is_matrix(other.is_matrix), nrow(other.nrow), ncol(other.ncol) {
    if (other.allocated) {
      p = new T[capacity];
      std::copy_n(other.p, sz, p);
    } else {
      reset();
    }
  }

  // Move constructor
  Buffer(Buffer &&other) noexcept
    : p(std::exchange(other.p, nullptr)), sz(std::exchange(other.sz, 0)),
    capacity(std::exchange(other.capacity, 0)),
    allocated(std::exchange(other.allocated, false)),
    is_matrix(std::exchange(other.is_matrix, false)),
    nrow(std::exchange(other.nrow, 0)), ncol(std::exchange(other.ncol, 0)) {}

  // copy assignment
  Buffer &operator=(const Buffer<T> &other) {
    if (this == &other)
      return *this;
    if (other.size() > this->sz) {
      if (allocated) {
        std::size_t diff = other.size() - this->sz;
        this->realloc(this->sz + diff);
      } else {
        resize(other.size());
      }
    }
    if (other.is_matrix) {
      is_matrix = true;
      nrow = other.nrow;
      ncol = other.ncol;
    } else {
      set_matrix_default();
    }
    for (std::size_t i = 0; i < this->sz; i++) {
      p[i] = other[i];
    }
    return *this;
  }

  // move assignment
  Buffer &operator=(Buffer &&other) noexcept {
    if (this != &other) {
      delete[] p;
      p = std::exchange(other.p, nullptr);
      sz = std::exchange(other.sz, 0);
      capacity = std::exchange(other.capacity, 0);
      allocated = std::exchange(other.allocated, false);
      is_matrix = std::exchange(other.is_matrix, false);
      nrow = std::exchange(other.nrow, 0);
      ncol = std::exchange(other.ncol, 0);
    }
    return *this;
  }

  // Constructors for std::size_t
  Buffer(std::size_t sz_) {
    ass<"Size has to be larger than 0!">(sz_ > 0);
    init(sz_);
  }

  // Constructors for nrow, ncol
  Buffer(std::size_t nrow, std::size_t ncol) {
    ass<"Size has to be larger than 0!">((nrow*ncol) > 0);
    init(nrow*ncol);
    set_matrix(nrow, ncol);
  }

#ifdef STANDALONE_ETR
#else
  void initSEXP(SEXP s) {
    if (allocated) {
      ass<"try to delete nullptr">(p != nullptr);
      delete[] p;
      reset();
    }
    if constexpr (is<RetType, double>) {
      ass<"R object is not of type numeric">(Rf_isReal(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      ass<"R object seems to be empty">(sz >= 1);
      capacity = static_cast<std::size_t>(sz);
      p = new T[capacity];
      for (int i = 0; i < sz; i++) {
        p[i] = REAL(s)[i];
      }
      allocated = true;
      if (Rf_isMatrix(s)) {
        set_matrix(Rf_nrows(s), Rf_ncols(s));
      }
    } else if constexpr (is<RetType, int>) {
      ass<"R object is not of type integer">(Rf_isInteger(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      ass<"R object seems to be empty">(sz >= 1);
      capacity = static_cast<std::size_t>(sz);
      p = new T[capacity];
      for (int i = 0; i < sz; i++) {
        p[i] = INTEGER(s)[i];
      }
      allocated = true;
      if (Rf_isMatrix(s)) {
        set_matrix(Rf_nrows(s), Rf_ncols(s));
      }
    } else if constexpr (is<RetType, bool>) {
      ass<"R object is not of type logical">(Rf_isLogical(s));
      sz = static_cast<std::size_t>(Rf_length(s));
      ass<"R object seems to be empty">(sz >= 1);
      capacity = static_cast<std::size_t>(sz);
      p = new T[capacity];
      for (int i = 0; i < sz; i++) {
        p[i] = LOGICAL(s)[i];
      }
      allocated = true;
      if (Rf_isMatrix(s)) {
        set_matrix(Rf_nrows(s), Rf_ncols(s));
      }
    } else {
      static_assert(sizeof(T) == 0, "Unsupported type found");
    }
  }
  Buffer(SEXP s) {
    initSEXP(s);
  };
#endif


  template <typename TInp>
  requires(IsArithV<Decayed<TInp>>)
  void fill(TInp &&val) {
    if constexpr (IS<T, TInp>) {
      std::fill(p, p + sz, val);
    } else {
      auto temp = static_cast<T>(val);
      std::fill(p, p + sz, temp);
    }
  }

  template <typename TInp>
  requires(!IsArithV<Decayed<TInp>>)
  void fill(TInp &&inp) {
    ass<"cannot use fill with vectors of different lengths">(inp.size() == sz);
    using DataType = typename ReRef<TInp>::type::RetType;
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

  ~Buffer() {
    if (p != nullptr) {
      if (allocated) {
        delete[] p;
        reset();
        set_matrix_default();
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
    set_matrix_default();
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

  template <typename L2> void moveit(L2 &other) {
    T *temporary = other.p;
    std::size_t tempSize = other.sz;
    std::size_t tempCapacity = other.capacity;
    bool tempIsMatrix = other.is_matrix;
    std::size_t tempNRow = other.nrow;
    std::size_t tempNCol = other.ncol;
    other.p = this->p;
    other.sz = this->sz;
    other.capacity = this->capacity;
    other.allocated = this->allocated;
    other.is_matrix = this->is_matrix;
    other.nrow = this->nrow;
    other.ncol = this->ncol;
    this->p = temporary;
    this->sz = tempSize;
    this->capacity = tempCapacity;
    this->is_matrix = tempIsMatrix;
    this->nrow = tempNRow;
    this->ncol = tempNCol;
    allocated = true;
  }

  auto begin() const { return It<T>{p}; }
  auto end() const { return It<T>{p + sz}; }
  T &back() {
    ass<"Size is 0">(this->sz >= 1);
    return p[sz - 1];
  }
  T *data() const { return p; }

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

struct Indices : public Buffer<std::size_t> {
  using RetType = std::size_t;
};

} // namespace etr

#endif
