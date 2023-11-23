#ifndef UTILSTRAITS_H
#define UTILSTRAITS_H

#include <cmath>
#include <cxxabi.h>
#include <iostream>
#include <type_traits>

// [[Rcpp::depends(RcppArmadillo)]]
#include "RcppArmadillo.h"
#include <iostream>
#include <iterator>
#include <math.h>
#include <memory>
#include <type_traits>
#include <vector>
#include <mutex>

/*
./ast2ast/inst/include/ad_etr/                      
├── add_ad.hpp               done                                  
├── allocation.hpp           done
├── backward.hpp           
├── checks_na_inf.hpp        done
├── colon.hpp                done                                        
├── comparison.hpp           done                                     
├── concatenate.hpp          done                                    
├── conversion.hpp           done                                                
├── distri.hpp  						 done
├── divide_ad.hpp            done                                  
├── exponent.hpp						 done
├── header.hpp               done            
├── interpolation.hpp				 done
├── mul_ad.hpp               done            
├── pointer_storage.hpp      done
├── print.hpp                done          
├── subset_ad.hpp						 done
├── subtract_ad.hpp          done
├── traits.hpp               done
├── trigo_ad.hpp             done
├── util.hpp                 done          
└── vec.hpp                  done
*/

std::string demangle(const char *mangledName) {
  int status;
  char *demangled = abi::__cxa_demangle(mangledName, nullptr, nullptr, &status);
  std::string result(demangled);
  std::free(demangled);
  return result;
}

template <typename T> void printType(T inp) {
  std::cout << demangle(typeid(inp).name()) << std::endl;
}

namespace etr {

typedef double BaseType;

template <bool B>
using BoolConstant = std::integral_constant<bool, B>;
using TRUE = BoolConstant<true>;
using T = BoolConstant<true>;
using FALSE = BoolConstant<false>;
using F = BoolConstant<false>;

inline int d2i(double inp) { return static_cast<int>(inp); }

inline double i2d(int inp) { return static_cast<double>(inp); }

inline bool d2b(double inp) { return static_cast<bool>(inp); }

inline bool i2b(double inp) { return static_cast<bool>(inp); }

inline double b2d(bool inp) { return static_cast<double>(inp); }

template <typename T> struct It {
  T *p;
  T &operator*() { return *p; }
  bool operator!=(const It &rhs) { return p != rhs.p; }
  void operator++() { ++p; }
};

struct DoubleTrait {};
struct IntTrait {};
struct BoolTrait {};
struct ComparisonTrait {};

struct BaseStoreTrait {};
struct BufferTrait {};
struct VectorTrait {};
struct VariableTrait {};
struct SubsetTrait {};
struct BorrowTrait {};
struct BorrowSEXPTrait {};

struct UnaryTrait {};
struct BinaryTrait {};

struct PlusTrait {};
struct MinusTrait {};
struct TimesTrait {};
struct DivideTrait {};
struct PowTrait {};
struct EqualTrait {};
struct SmallerTrait {};
struct SmallerEqualTrait {};
struct LargerTrait {};
struct LargerEqualTrait {};
struct UnEqualTrait {};

struct SinusTrait {};
struct ASinusTrait {};
struct SinusHTrait {};
struct CosinusTrait {};
struct ACosinusTrait {};
struct CosinusHTrait {};
struct TangensTrait {};
struct ATangensTrait {};
struct TangensHTrait {};
struct ExpTrait {};
struct LogTrait {};
struct SquareRootTrait {};

template <typename T>
concept isBID = requires {
	requires std::is_same_v<T, bool> || std::is_same_v<T, int> || std::is_same_v<T, double>;
};

template <typename R>
concept IsVecDouble = requires {
    typename R::TypeTrait;
    typename R::Type;
    requires std::is_same_v<typename R::TypeTrait, VectorTrait>;
    requires std::is_same_v<typename R::Type, BaseType>;
};


template <typename R>
concept IsVecBool = requires {
    typename R::TypeTrait;
    typename R::Type;
    requires std::is_same_v<typename R::TypeTrait, VectorTrait>;
    requires std::is_same_v<typename R::Type, bool>;
};

inline void ass(bool inp, std::string message) {
  if (!inp)
    Rf_error(message.c_str());
}

struct MatrixParameter {
  bool ismatrix = false;
  size_t rows = 0;
  size_t cols = 0;
  MatrixParameter() {}
  MatrixParameter(size_t rows_, size_t cols_)
      : ismatrix(true), rows(rows_), cols(cols_) {}
  MatrixParameter(const MatrixParameter &other)
      : ismatrix(other.im()), rows(other.nr()), cols(other.nc()) {}
  bool im() const { return this->ismatrix; }
  size_t nc() const { return cols; }
  size_t nr() const { return rows; }
  void setMatrix(bool i, size_t nrow, size_t ncol) {
    this->ismatrix = i;
    this->cols = ncol;
    this->rows = nrow;
  }
  void setMatrix(const MatrixParameter &mp_) {
    this->setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
};

template<typename Trait = DoubleTrait>
struct doubleWrapper {
	using TypeTrait = Trait;
	using Type = DoubleTrait;
  BaseType d;
};

template <typename T>
  requires std::is_same_v<T, double>
constexpr doubleWrapper<DoubleTrait> convert(const T &obj) {
  return doubleWrapper<DoubleTrait>(obj);
}

template <typename T>
  requires std::is_same_v<T, int>
constexpr doubleWrapper<IntTrait> convert(const T &obj) {
  return doubleWrapper<IntTrait>(obj);
}

template <typename T>
  requires std::is_same_v<T, bool>
constexpr doubleWrapper<BoolTrait> convert(const T &obj) {
  return doubleWrapper<BoolTrait>(obj);
}

template <typename T> constexpr T convert(const T &obj) {
  return obj;
  //return std::forward(obj);
}

inline double Addition(double l, double r) { return l + r; }
inline double Minus(double l, double r) { return l - r; }
inline double Times(double l, double r) { return l * r; }
inline double Divide(double l, double r) { return l / r; }
inline double Pow(double l, double r) { return std::pow(l, r); }
typedef double (*binaryFct)(double, double);

inline double Sinus(double obj) { return sin(obj); }
inline double SinusH(double obj) { return sinh(obj); }
inline double ASinus(double obj) { return asin(obj); }
inline double Cosinus(double obj) { return cos(obj); }
inline double CosinusH(double obj) { return cosh(obj); }
inline double ACosinus(double obj) { return acos(obj); }
inline double Tangens(double obj) { return tan(obj); }
inline double TangensH(double obj) { return tanh(obj); }
inline double ATangens(double obj) { return atan(obj); }
inline double Exp(double obj) { return exp(obj); }
inline double Log(double obj) { return log(obj); }
inline double SquareRoot(double obj) { return sqrt(obj); }
inline double Equal(double a, double b) { 
	if(fabs(a - b) < 1E-3) {
		return 1.0;
  } else {
   	return 0.0;
  }
}
inline double Smaller(double a, double b) { 
	if(a < b) {
		return 1.0;
  } else {
   	return 0.0;
  }
}
inline double SmallerEqual(double a, double b) { 
	if(a <= b) {
		return 1.0;
  } else {
   	return 0.0;
  }
}
inline double Larger(double a, double b) { 
	if(a > b) {
		return 1.0;
  } else {
   	return 0.0;
  }
}
inline double LargerEqual(double a, double b) { 
	if(a >= b) {
		return 1.0;
  } else {
   	return 0.0;
  }
}
inline double UnEqual(double a, double b) { 
	if(fabs(a - b) > 1E-3) {
		return 1.0;
  } else {
   	return 0.0;
  }
}

typedef double (*UnaryFct)(double);

template <typename T, typename BaseTrait = BaseStoreTrait> struct BaseStore;
template <typename T = double, typename BorrowSEXPTrait = BorrowSEXPTrait>
struct BorrowSEXP;
template <typename T, typename BorrowTrait = BorrowTrait> struct Borrow;
template <typename T, typename SubsetTrait = SubsetTrait> struct Subset;
template <typename T, typename Trait = BufferTrait,
          typename CTrait = VariableTrait>
struct Buffer;
template <typename T, typename R = Buffer<T>, typename Trait = VectorTrait>
struct Vec;
template <typename L, typename R, binaryFct f, typename Trait = BinaryTrait,
          typename CTrait = BinaryTrait>
struct BinaryOperation;
template <typename I, UnaryFct f, typename Trait = UnaryTrait,
          typename CTrait = UnaryTrait>
struct UnaryOperation;

struct BaseCalc {
  bool ismatrix;
  size_t rows;
  size_t cols;
  bool im() const { return this->ismatrix; }
  size_t nc() const { return cols; }
  size_t nr() const { return rows; }
  void set_matrix(bool i, size_t nrow, size_t ncol) {
    this->ismatrix = i;
    this->cols = ncol;
    this->rows = nrow;
  }
};

template <typename T, typename BaseTrait> struct BaseStore {
  using Type = T;
  using TypeTrait = BaseTrait;
  T *p = nullptr;
  size_t sz = 0;
  size_t capacity = 0;
  bool allocated = false;
  MatrixParameter mp;

  size_t size() const { return sz; }
  bool im() const { return mp.im(); }
  size_t nc() const { return mp.nc(); }
  size_t nr() const { return mp.nr(); }
  void setMatrix(bool i, size_t nrow, size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }
  void setMatrix(MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }

  // constructors
  BaseStore(const BaseStore<T> &other)
      : sz(other.sz), capacity(other.capacity), allocated(other.allocated) {
    if (other.allocated) {
      p = new T[capacity]; // issue: new (use std.:align_val_t(16)) T[capacity]
      std::copy(other.p, other.p + capacity, p);
    } else {
      p = nullptr;
    }
  }
  BaseStore(BaseStore<T> &&other) noexcept
      : sz(other.sz), capacity(other.capacity), allocated(other.allocated),
        p(other.p) {
    other.capacity = 0;
    other.sz = 0;
    other.allocated = false;
    other.p = nullptr;
  }
  BaseStore(SEXP s) = delete;
  BaseStore(size_t sz_) : sz(sz_), capacity(static_cast<size_t>(sz_ * 1.15)) {
    ass(sz_ > 0, "Size has to be larger than 0");
    p = new T[capacity];
    for (size_t i = 0; i < sz; i++)
      p[i] = 0.0;
    allocated = true;
  }
  BaseStore(int sz_)
      : sz(static_cast<size_t>(sz_)),
        capacity(static_cast<size_t>(sz_ * 1.15)) {
    ass(sz_ > 0, "Size has to be larger than 0");
    p = new T[capacity];
    for (size_t i = 0; i < sz; i++)
      p[i] = 0.0;
    allocated = true;
  }
  BaseStore() {}
  BaseStore(size_t r, size_t c) = delete;
  BaseStore(size_t r, size_t c, const double value) = delete;

  BaseStore &operator=(const BaseStore<T> &other) { // deep copy
    if (this == &other)
      return *this;
    if (other.size() > this->sz) {
      int diff = other.size() - this->sz;
      this->realloc(this->sz + diff);
    }
    for (int i = 0; i < this->sz; i++) {
      p[i] = other[i];
    }
    return *this;
  }

  void fill(T val) {
    for (size_t i = 0; i < sz; i++) {
      p[i] = val;
    }
  }

  ~BaseStore() {
    if (p != nullptr) {
      if (allocated) {
        delete[] p;
        sz = 0;
        capacity = 0;
        p = nullptr;
        allocated = false;
      }
    }
  }

  void init(size_t size) {
    if (allocated == true) {
      ass(p != nullptr, "try to delete nullptr");
      delete[] p;
      p = nullptr;
    }
    sz = size;
    capacity = static_cast<size_t>(1.15 * sz);
    p = new T[capacity];
    allocated = true;
  }
  void resize(size_t newSize) {
    if (!allocated) {
      init(newSize);
      return;
    } else {
      ass(p != nullptr, "try to delete nullptr");
      delete[] p;
      sz = newSize;
      capacity = static_cast<size_t>(newSize * 1.15);
      p = new T[capacity];
      allocated = true;
    }
    fill(0.0);
  }

  T operator[](size_t idx) const {
    ass(allocated, "No memory was allocated");
    ass(idx >= 0, "Error: out of boundaries --> value below 1");
    ass(idx < sz, "Error: out of boundaries --> value beyond size of vector");
    return p[idx];
  }

  T &operator[](size_t idx) {
    ass(allocated, "No memory was allocated");
    ass(idx >= 0, "Error: out of boundaries --> value below 1");
    ass(idx < sz, "Error: out of boundaries --> value beyond size of vector");
    return p[idx];
  }

  template<typename L2>
  BaseStore &moveit(L2 &other) {
    T *temporary = other.p;
    int tempSize = other.sz;
    int tempCapacity = other.capacity;
    other.p = this->p;
    other.sz = this->sz;
    other.capacity = this->capacity;
    this->p = temporary;
    this->sz = tempSize;
    this->capacity = tempCapacity;
    allocated = true;
    return *this;
  }
  auto begin() const { return It<T>{p}; }
  auto end() const { return It<T>{p + sz}; }
  T &back() { return p[sz]; }
  T *data() const { return p; }

  void realloc(int new_size) {
    T *temp;
    temp = new T[sz];
    for (int i = 0; i < sz; i++)
      temp[i] = p[i];
    ass(p != nullptr, "try to delete nullptr");
    delete[] p;
    p = new T[new_size];
    for (int i = 0; i < sz; i++)
      p[i] = temp[i];
    ass(temp != nullptr, "try to delete nullptr");
    delete[] temp;
    for (size_t i = sz; i < new_size; i++)
      p[i] = 0.0;
    sz = new_size;
    capacity = sz;
    temp = nullptr;
    allocated = true;
  }

  void push_back(T input) {
    if (sz == capacity) {
      realloc(sz * 2);
      capacity = sz;
    } else if (sz < capacity) {
      p[sz] = input; // p starts counting at 0!!!
    }
  }
  friend std::ostream &operator<<(std::ostream &os, const BaseStore &b) {
    os << "Vec [ ";
    for (size_t i = 0; i < b.size(); i++) {
      os << b.p[i] << " ";
    }
    os << "]";
    return os;
  }
};

struct Indices : public std::vector<size_t> {};

// Points to a Variable and stores indicces in ind
template <typename T, typename SubsetTrait> struct Subset {
  using Type = T;
  using TypeTrait = SubsetTrait;
  using CaseTrait = SubsetTrait;
  Indices ind;
  T *p = nullptr;
  MatrixParameter mp;
  mutable signed int ref_counter = 0;

  size_t size() const { return ind.size(); }
  bool im() const { return mp.im(); }
  size_t nc() const { return mp.nc(); }
  size_t nr() const { return mp.nr(); }
  void setMatrix(bool i, size_t nrow, size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }
  void setMatrix(MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void resize(size_t newSize) { p -> resize(newSize); }

  Subset(const Subset& other) {
  	this->p = other.p;
    this->setMatrix(other.mp);
    this -> ind = other.ind;
  }
  Subset(const Subset&& other) {
  	this->p = other.p;
    this->setMatrix(other.mp);
    this -> ind = other.ind;
  }
  template <typename T2, typename R2> Subset(Vec<T2, R2> &other) {
    this->p = &other.d;
  }
  Subset(SEXP) = delete;
  Subset(size_t i) = delete;
  Subset(int i) = delete;
  Subset() = delete;
  Subset(size_t r, size_t c) = delete;
  Subset(size_t r, size_t c, const double value) = delete;

  void resizeInd(int newSize) { ind.resize(newSize); }
  void setInd(int idx, int val) { ind[idx] = val; }
  void setPtr(const T *pOther) { this->p = pOther; }

  BaseType &operator[](size_t pos) {
    ass(this -> p != nullptr, "Subset is pointing to nothing!");
    return this -> p->operator[](ind[pos % p -> size()]);
  }

  BaseType operator[](size_t pos) const {
    ass(p != nullptr, "Subset is pointing to nothing!");
    return this -> p->operator[](ind[pos % p -> size()]);
  }

  ~Subset() {}
};


// Points to a Variable and stores size
template <typename T, typename BorrowTrait> struct Borrow {
  using Type = T;
  using TypeTrait = BorrowTrait;
  T *p = nullptr;
  size_t sz = 0;
  size_t capacity = 0;
  bool allocated = false;
  MatrixParameter mp;
  mutable signed int ref_counter = 0;

  size_t size() const { return sz; }
  bool im() const { return mp.im(); }
  size_t nc() const { return mp.nc(); }
  size_t nr() const { return mp.nr(); }
  void setMatrix(bool i, size_t nrow, size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }

  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }

  Borrow(const Borrow<T> &other)
      : sz(other.sz), capacity(other.capacity), allocated(false) {
    p = other.p;
    mp.setMatrix(other.mp);
  } // issue: deep copy required?
  Borrow(Borrow<T> &&other) noexcept
      : sz(other.sz), capacity(other.capacity), allocated(other.allocated),
        p(other.p) {
    other.capacity = 0;
    other.sz = 0;
    other.allocated = false;
    other.p = nullptr;
    mp.setMatrix(other.mp);
  }
  Borrow(SEXP s) = delete;
  Borrow(size_t i) = delete;
  Borrow(int i) = delete;
  Borrow() = delete;
  Borrow(size_t r, size_t c) = delete;
  Borrow(size_t r, size_t c, const double value) = delete;
  Borrow(T *p, size_t sz) {
    this->p = p;
    this->sz = sz;
    capacity = sz;
    this->allocated = false;
  }

  Borrow &operator=(const Borrow<T> &other) {
    p = other.p;
    sz = other.sz;
    capacity = other.capacity;
    allocated = false;
    mp.setMatrix(other.mp);
    return *this;
  }

  void fill(T val) {
    for (size_t i = 0; i < sz; i++) {
      p[i] = val;
    }
  }

  ~Borrow() {}

  void init(size_t size) = delete;
  void resize(size_t newSize) = delete;
  void set(size_t idx, T val) {
    ass(idx >= 0, "Index has to be a positive number");
    ass(idx < sz, "Index cannot be larger than size of Indices");
    p[idx] = val;
  }

  T operator[](size_t idx) const {
    ass(idx >= 0, "Error: out of boundaries --> value below 1");
    ass(idx < sz, "Error: out of boundaries --> value beyond size of vector");
    return p[idx];
  }

  T &operator[](size_t idx) {
    ass(idx >= 0, "Error: out of boundaries --> value below 1");
    ass(idx < sz, "Error: out of boundaries --> value beyond size of vector");
    return p[idx];
  }

  template<typename L2>
  Borrow &moveit(L2 &other) = delete;
  auto begin() const { return It<T>{p}; }
  auto end() const { return It<T>{p + sz}; }
  T &back() { return p[sz]; }
  T *data() const { return p; }
  void realloc(int new_size) = delete;
  void push_back(T input) = delete;
  void setSize(size_t sz_) { this->sz = sz_; }
  void setPtr(const T *pOther) { this->p = pOther; }

  friend std::ostream &operator<<(std::ostream &os, const Borrow &b) {
    os << "Vec [ ";
    for (size_t i = 0; i < b.size(); i++) {
      os << b.p[i] << " ";
    }
    os << "]";
    return os;
  }
};

// Points to a SEXP and stores size
template <typename T, typename BorrowSEXPSEXPTrait> struct BorrowSEXP {
  using Type = T;
  using TypeTrait = BorrowSEXPSEXPTrait;
  using CaseTrait = BorrowSEXPSEXPTrait;
  T *p = nullptr;
  bool todelete = false;
  bool allocated = false;
  size_t sz = 0;
  size_t capacity = 0;
  MatrixParameter mp;
  mutable signed int ref_counter = 0;

  size_t size() const { return sz; }
  bool im() const { return mp.im(); }
  size_t nc() const { return mp.nc(); }
  size_t nr() const { return mp.nr(); }
  void setMatrix(bool i, size_t nrow, size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }

  BorrowSEXP(const BorrowSEXP<T> &other) {
    resize(other.size());
    for (size_t i = 0; i < sz; i++)
      p[i] = other[i];
    mp.setMatrix(other.mp);
  }
  BorrowSEXP(SEXP inp) {
    if (allocated == true) {
      ass(p != nullptr, "try to delete nullptr");
      delete[] p;
      this->p = nullptr;
    }
    p = REAL(inp);
    sz = Rf_length(inp);
    capacity = Rf_length(inp);
    if (Rf_isMatrix(inp) == true) {
      mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
    }
    todelete = false;
    allocated = true;
  }
  BorrowSEXP(size_t i) = delete;
  BorrowSEXP(int i) = delete;
  BorrowSEXP() {}
  BorrowSEXP(size_t r, size_t c) = delete;
  BorrowSEXP(size_t r, size_t c, const double value) = delete;

  ~BorrowSEXP() {
    if (todelete) {
      delete[] p;
    }
  }

  BorrowSEXP &operator=(const BorrowSEXP<T> &other) {
    resize(other.size());
    for (size_t i = 0; i < sz; i++)
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
    p = REAL(inp);
    sz = Rf_length(inp);
    capacity = Rf_length(inp);
    if (Rf_isMatrix(inp) == true) {
      mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
    }
    todelete = false;
    allocated = true;
    return *this;
  }

  T &operator[](size_t pos) {
    ass(p != nullptr, "Subset is pointing to nothing!");
    ass(pos >= 0, "Error: out of boundaries --> value below 1");
    ass(pos < sz, "Error: out of boundaries --> value beyond size of vector");
    return this->p[pos];
  }

  T operator[](size_t pos) const {
    ass(this->p != nullptr, "Subset is pointing to nothing!");
    ass(pos >= 0, "Error: out of boundaries --> value below 1");
    ass(pos < sz, "Error: out of boundaries --> value beyond size of vector");
    return this->p[pos];
  }

  void init(size_t size) {
    if (allocated && todelete) {
      ass(p != nullptr, "try to delete nullptr");
      delete[] p;
      p = nullptr;
    }
    sz = size;
    capacity = static_cast<size_t>(1.15 * sz);
    p = new T[capacity];
    allocated = true;
    todelete = true;
  }

  void resize(size_t newSize) {
    if (!allocated) {
      init(newSize);
      return;
    } else {
      ass(p != nullptr, "try to delete nullptr");
      if (todelete)
        delete[] p;
      sz = newSize;
      capacity = static_cast<size_t>(newSize * 1.15);
      p = new T[capacity];
      allocated = true;
      todelete = true;
    }
    fill(0.0);
  }

  template<typename L2>
  BorrowSEXP &moveit(L2 &other) {
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
    if (new_size <= sz)
      return;
    if (!todelete) {
      T *temp;
      temp = new T[sz];
      for (size_t i = 0; i < sz; i++)
        temp[i] = p[i];
      p = new T[new_size];
      for (size_t i = 0; i < sz; i++)
        p[i] = temp[i];
      ass(temp != nullptr, "try to delete nullptr");
      delete[] temp;
      temp = nullptr;
      allocated = true;
      todelete = true;
      for (size_t i = sz; i < new_size; i++)
        p[i] = 0.0;
      sz = new_size;
      capacity = sz;
    } else {
      T *temp;
      temp = new T[sz];
      for (size_t i = 0; i < sz; i++)
        temp[i] = p[i];
      ass(p != nullptr, "try to delete nullptr");
      delete[] p;
      p = new T[new_size];
      for (size_t i = 0; i < sz; i++)
        p[i] = temp[i];
      ass(temp != nullptr, "try to delete nullptr");
      delete[] temp;
      temp = nullptr;
      allocated = true;
      for (size_t i = sz; i < new_size; i++)
        p[i] = 0.0;
      sz = new_size;
      capacity = sz;
    }
  }
  void push_back(T input) {
    if (sz == capacity) {
      realloc(sz * 2);
      capacity = sz;
    } else if (sz < capacity) {
      p[sz] = input; // p starts counting at 0!!!
    }
  }
  friend std::ostream &operator<<(std::ostream &os, const BorrowSEXP &b) {
    os << "Vec [ ";
    for (size_t i = 0; i < b.size(); i++) {
      os << b.p[i] << " ";
    }
    os << "]";
    return os;
  }
  void fill(T val) {
    for (size_t i = 0; i < sz; i++) {
      p[i] = val;
    }
  }
};

template<typename L, typename R>
void defineMatrix(const L& l, const R& r,
                  MatrixParameter &mp) {
	using typeTraitL = std::remove_reference<decltype(convert(l))>::type::Type;
  using typeTraitR = std::remove_reference<decltype(convert(r))>::type::Type;
  using isDoubleL = std::is_same<typeTraitL, DoubleTrait>;
  using isDoubleR = std::is_same<typeTraitR, DoubleTrait>;
  mp.setMatrix(false, 0, 0);
  if constexpr(!isDoubleL::value && isDoubleR::value) {
  	if(l.im()) {
  		mp.setMatrix(true, l.nr(), l.nc());
  	}
  } else if constexpr(isDoubleL::value && !isDoubleR::value) {
  	if(r.im()) {
  		mp.setMatrix(true, r.nr(), r.nc());
  	}
  } else {
  	if (l.im() && r.im()) {
      size_t nrows = (l.nr() > r.nr()) ? l.nr() : r.nr();
      size_t ncols = (l.nc() > r.nc()) ? l.nc() : r.nc();
      mp.setMatrix(true, nrows, ncols);
    } else if (!l.im() && r.im()) {
      size_t nrows = r.nr();
      size_t ncols = r.nc();
      mp.setMatrix(true, nrows, ncols);
    } else if (l.im() && !r.im()) {
      size_t nrows = r.nr();
      size_t ncols = r.nc();
      mp.setMatrix(true, nrows, ncols);
  	}	
  }
} 

} // namespace etr

#endif