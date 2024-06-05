#ifndef BORROWSEXP_ETR_H
#define BORROWSEXP_ETR_H

namespace etr {
#ifdef STANDALONE_ETR
#else
// Points to a SEXP and stores size
template <typename T, typename BorrowSEXPSEXPTrait> struct BorrowSEXP {
  using RetType = BaseType;
  using Type = T;
  using TypeTrait = BorrowSEXPSEXPTrait;
  using CaseTrait = BorrowSEXPSEXPTrait;
  T *p = nullptr;
  bool todelete = false;
  bool allocated = false;
  std::size_t sz = 0;
  std::size_t capacity = 0;
  MatrixParameter mp;

  std::size_t size() const { return sz; }
  bool im() const { return mp.im(); }
  std::size_t nc() const { return mp.nc(); }
  std::size_t nr() const { return mp.nr(); }
  void setMatrix(bool i, std::size_t nrow, std::size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }

  BorrowSEXP(const BorrowSEXP<T> &other) {
    resize(other.size());
    for (std::size_t i = 0; i < sz; i++)
      p[i] = other[i];
    mp.setMatrix(other.mp);
  }
  BorrowSEXP(SEXP inp) {
    if (allocated == true) {
      ass(p != nullptr, "try to delete nullptr");
      delete[] p;
      this->p = nullptr;
    }
    if constexpr (is<RetType, double>) {
      bool isreal = Rf_isReal(inp);
      std::cout << "isreal " << isreal << std::endl;
      ass(isreal, "R object is not of type numeric");
      p = REAL(inp);
      sz = Rf_length(inp);
      capacity = Rf_length(inp);
      bool isM = Rf_isMatrix(inp);
      if (isM) {
        mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      }
      todelete = false;
      allocated = true;
    } else if constexpr (is<RetType, int>) {
      bool isint = Rf_isInteger(inp);
      ass(isint, "R object is not of type numeric");
      p = INTEGER(inp);
      sz = Rf_length(inp);
      capacity = Rf_length(inp);
      bool isM = Rf_isMatrix(inp);
      if (isM) {
        mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      }
      todelete = false;
      allocated = true;
    } else if constexpr (is<RetType, bool>) {
      bool isbool = Rf_isLogical(inp);
      ass(isbool, "R object is not of type numeric");
      p = LOGICAL(inp);
      sz = Rf_length(inp);
      capacity = Rf_length(inp);
      bool isM = Rf_isMatrix(inp);
      if (isM) {
        mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      }
      todelete = false;
      allocated = true;
    } else {
      static_assert(sizeof(T) == 0, "Unsupported type found");
    }
  }
  BorrowSEXP(std::size_t i) = delete;
  BorrowSEXP(int i) = delete;
  BorrowSEXP() {}
  BorrowSEXP(std::size_t r, std::size_t c) = delete;
  BorrowSEXP(std::size_t r, std::size_t c, const double value) = delete;

  ~BorrowSEXP() {
    if (todelete) {
      delete[] p;
      p = nullptr;
    }
  }

  BorrowSEXP &operator=(const BorrowSEXP<T> &other) {
    if (this == &other)
      return *this;
    resize(other.size());
    for (std::size_t i = 0; i < sz; i++)
      p[i] = other[i];
    mp.setMatrix(other.mp);
    return *this;
  }

  BorrowSEXP &operator=(SEXP inp) {
    if (allocated == true) {
      ass(p != nullptr, "try to delete nullptr");
      delete[] p;
      this->p = nullptr;
    }
    if constexpr (is<RetType, double>) {
      ass(Rf_isReal(inp), "R object is not of type numeric");
      p = REAL(inp);
      sz = Rf_length(inp);
      capacity = Rf_length(inp);
      if (Rf_isMatrix(inp) == true) {
        mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      }
      todelete = false;
      allocated = true;
    } else if constexpr (is<RetType, int>) {
      ass(Rf_isInteger(inp), "R object is not of type numeric");
      p = INTEGER(inp);
      sz = Rf_length(inp);
      capacity = Rf_length(inp);
      if (Rf_isMatrix(inp) == true) {
        mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      }
      todelete = false;
      allocated = true;
    } else if constexpr (is<RetType, bool>) {
      ass(Rf_isLogical(inp), "R object is not of type numeric");
      p = LOGICAL(inp);
      sz = Rf_length(inp);
      capacity = Rf_length(inp);
      if (Rf_isMatrix(inp) == true) {
        mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      }
      todelete = false;
      allocated = true;
    } else {
      static_assert(sizeof(T) == 0, "Unsupported type found");
    }
    return *this;
  }

  void initSEXP(SEXP inp) {
    if (allocated == true) {
      ass(p != nullptr, "try to delete nullptr");
      delete[] p;
      this->p = nullptr;
    }
    if constexpr (is<RetType, double>) {
      bool isreal = Rf_isReal(inp);
      std::cout << "isreal " << isreal << std::endl;
      ass(isreal, "R object is not of type numeric");
      p = REAL(inp);
      sz = Rf_length(inp);
      capacity = Rf_length(inp);
      bool isM = Rf_isMatrix(inp);
      if (isM) {
        mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      }
      todelete = false;
      allocated = true;
    } else if constexpr (is<RetType, int>) {
      bool isint = Rf_isInteger(inp);
      ass(isint, "R object is not of type numeric");
      p = INTEGER(inp);
      sz = Rf_length(inp);
      capacity = Rf_length(inp);
      bool isM = Rf_isMatrix(inp);
      if (isM) {
        mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      }
      todelete = false;
      allocated = true;
    } else if constexpr (is<RetType, bool>) {
      bool isbool = Rf_isLogical(inp);
      ass(isbool, "R object is not of type numeric");
      p = LOGICAL(inp);
      sz = Rf_length(inp);
      capacity = Rf_length(inp);
      bool isM = Rf_isMatrix(inp);
      if (isM) {
        mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      }
      todelete = false;
      allocated = true;
    } else {
      static_assert(sizeof(T) == 0, "Unsupported type found");
    }
  }

  RetType &operator[](std::size_t pos) {
    ass(p != nullptr, "Subset is pointing to nothing!");
    ass(pos >= 0, "Error: out of boundaries --> value below 1");
    ass(pos < sz, "Error: out of boundaries --> value beyond size of vector");
    return this->p[pos];
  }

  RetType operator[](std::size_t pos) const {
    ass(this->p != nullptr, "Subset is pointing to nothing!");
    ass(pos >= 0, "Error: out of boundaries --> value below 1");
    ass(pos < sz, "Error: out of boundaries --> value beyond size of vector");
    return this->p[pos];
  }

  void init(std::size_t size) {
    if (allocated && todelete) {
      ass(p != nullptr, "try to delete nullptr");
      delete[] p;
      p = nullptr;
    }
    sz = size;
    capacity = static_cast<std::size_t>(1.15 * sz);
    p = new T[capacity];
    allocated = true;
    todelete = true;
  }

  void resize(std::size_t newSize) {
    ass(newSize >= 1, "Size has to be larger than 0");
    if (!allocated) {
      init(newSize);
      fill(T());
      return;
    } else {
      if (newSize > capacity) {
        ass(p != nullptr, "try to delete nullptr");
        delete[] p;
        sz = newSize;
        capacity = static_cast<std::size_t>(newSize * 1.15);
        p = new T[capacity];
        allocated = true;
        todelete = true;
        sz = newSize;
        fill(T());
      } else {
        sz = newSize;
        return;
      }
    }
  }

  template <typename L2> BorrowSEXP &moveit(L2 &other) {
    if (!todelete) {
      resize(other.size());
      T *temporary = other.p;
      int tempSize = other.sz;
      int tempCapacity = other.capacity;
      other.p = this->p;
      other.sz = this->sz;
      other.capacity = this->capacity;
      this->p = temporary;
      this->sz = tempSize;
      this->capacity = tempCapacity;
    } else {
      T *temporary = other.p;
      int tempSize = other.sz;
      int tempCapacity = other.capacity;
      other.p = this->p;
      other.sz = this->sz;
      other.capacity = this->capacity;
      this->p = temporary;
      this->sz = tempSize;
      this->capacity = tempCapacity;
    }
    return *this;
  }

  auto begin() const { return It<T>{p}; }
  auto end() const { return It<T>{p + sz}; }
  T &back() { return p[sz]; }
  T *data() const { return p; }
  void realloc(int new_size) {
    if (new_size <= sz) {
      return;
    }
    capacity = static_cast<int>(new_size * 1.15);
    T *temp = new T[capacity];
    for (std::size_t i = 0; i < sz; i++)
      temp[i] = p[i];
    for (std::size_t i = sz; i < new_size; i++)
      temp[i] = T();
    sz = new_size;
    delete[] p;
    p = temp;
    allocated = true;
    temp = nullptr;

    if (!todelete) {
      todelete = true;
    }
  }
  void push_back(T input) {
    if (sz == capacity) {
      int szOld = sz;
      if (sz == 0) {
        realloc(1);
      } else if (sz > 0) {
        realloc(sz * 2);
      } else {
        ass(false, "negative size found.");
      }
      capacity = sz;
      p[szOld] = input;
      sz = szOld + 1;
    } else if (sz < capacity) {
      p[sz] = input;
      sz++;
    }
  }
  friend std::ostream &operator<<(std::ostream &os, const BorrowSEXP &b) {
    os << "Vec [ ";
    for (std::size_t i = 0; i < b.size(); i++) {
      os << b.p[i] << " ";
    }
    os << "]";
    return os;
  }
  template <typename TInp>
    requires(std::is_arithmetic_v<std::remove_reference_t<TInp>>)
  void fill(TInp &&val) {
    if constexpr (std::is_same_v<T, TInp>) {
      std::fill(p, p + sz, val);
    } else {
      auto temp = static_cast<T>(val);
      std::fill(p, p + sz, temp);
    }
  }

  template <typename TInp> void fill(TInp &&inp) {
    ass(inp.size() == sz, "cannot use fill with vectors of different lengths");
    using DataType = ExtractDataType<std::remove_reference_t<TInp>>::RetType;
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
};
#endif

} // namespace etr

#endif
