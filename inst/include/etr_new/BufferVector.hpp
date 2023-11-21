#ifndef BUFFER_VECTOR_H
#define BUFFER_VECTOR_H

#include "UtilsTraits.hpp"
#include "binaryCalculations.hpp"
#include "unaryCalculations.hpp"

namespace etr {

template <typename T, typename Trait, typename CTrait>
struct Buffer : public BaseStore<T> {
  using TypeTrait = Trait;
  using CaseTrait = CTrait;
};

template <typename T, typename R, typename Trait> struct Vec {
  using Type = T;
  using TypeTrait = Trait;
  using CaseTrait = Trait;
  R d;
  using typeTraitD = std::remove_reference<decltype(d)>::type::TypeTrait;
  using isBuffer = std::is_same<typeTraitD, BufferTrait>;
  using isBorrow = std::is_same<typeTraitD, BorrowTrait>;
  using isBorrowSEXP = std::is_same<typeTraitD, BorrowSEXPTrait>;
  using isSubset = std::is_same<typeTraitD, SubsetTrait>;
  using isSubsetCalc = std::is_same<typeTraitD, SubsetCalcTrait>;
  using caseTraitD = std::remove_reference<decltype(d)>::type::CaseTrait;
  using isUnaryOP = std::is_same<caseTraitD, UnaryTrait>;
  using isBinaryOP = std::is_same<caseTraitD, BinaryTrait>;

  template <typename T2> Vec(T2 n) = delete;
  // move constructors
  //template <typename L2> explicit Vec(const SubsetCalc<L2> &&inp) : d(inp) {d.setMatrix(inp.mp);}
  template <typename L2> explicit Vec(const Subset<L2> &&inp) : d(inp) {d.setMatrix(inp.mp);}
  template <typename L2> explicit Vec(const Buffer<L2> &&inp) : d(inp) {d.setMatrix(inp.mp);}
  template <typename U = R, typename T2>
    requires std::is_same_v<U, Borrow<BaseType>>
  explicit Vec(const Borrow<T2> &&borrowed) : d(borrowed) {d.setMatrix(borrowed.mp);}
  template <typename L2, typename R2, binaryFct f, typename OperationTrait>
  explicit Vec(const BinaryOperation<L2, R2, f, OperationTrait> &&inp)
      : d(inp) {
    using TypeTrait = OperationTrait;
    d.setMatrix(inp.mp);
  }
  template <typename L2, typename R2, binaryFct f, typename OperationTrait, typename DetailTrait> // only for comparison!
  explicit Vec(const BinaryOperation<L2, R2, f, OperationTrait, DetailTrait> &&inp)
      : d(inp) {
    using TypeTrait = OperationTrait;
    d.setMatrix(inp.mp);
  }
  template <typename L2, UnaryFct f, typename OperationTrait>
  explicit Vec(const UnaryOperation<L2, f, OperationTrait> &&inp) : d(inp) {
    using TypeTrait = OperationTrait;
    d.setMatrix(inp.mp);
  }

  // copy constructors
  template <typename L2> explicit Vec(const SubsetCalc<T, L2> &inp) : d(inp) {d.setMatrix(inp.mp);} //= delete;
  template <typename L2> explicit Vec(Subset<L2> &inp) : d(inp) {d.setMatrix(inp.mp);}
  template <typename L2> explicit Vec(const Buffer<L2> &inp) : d(inp) {d.setMatrix(inp.mp);}
  template <typename U = R, typename T2>
    requires std::is_same_v<U, Borrow<BaseType>>
  explicit Vec(const Borrow<T2> &borrowed) : d(borrowed) {d.setMatrix(borrowed.mp);}
  template <typename L2, typename R2, binaryFct f, typename OperationTrait>
  explicit Vec(BinaryOperation<L2, R2, f, OperationTrait> &inp) : d(inp) {
    using TypeTrait = OperationTrait;
    d.setMatrix(inp.mp);
  }
  template <typename L2, UnaryFct f, typename OperationTrait>
  explicit Vec(UnaryOperation<L2, f, OperationTrait> &inp) : d(inp) {
    using TypeTrait = OperationTrait;
    d.setMatrix(inp.mp);
  }

  // other constructors
  template <typename U = R>
    requires std::is_same_v<U, BorrowSEXP<BaseType>>
  explicit Vec(SEXP &&inp) = delete;
  template <typename U = R>
    requires std::is_same_v<U, BorrowSEXP<BaseType>>
  explicit Vec(SEXP inp) : d(inp) {}
  explicit Vec(size_t sz) : d(sz) {}
  explicit Vec(int sz) : d(static_cast<size_t>(sz)) {}
  explicit Vec() : d() {}
  explicit Vec(size_t rows, size_t cols) : d(rows * cols) {
    d.setMatrix(true, rows, cols);
  }
  explicit Vec(size_t rows, size_t cols, const double value) : d(rows * cols) {
    d.setMatrix(true, rows, cols);
    d.fill(value);
  }
  // other vector constructors
  template <typename T2, typename R2, typename Trait2>
  explicit Vec(const Vec<T2, R2, Trait2> &other_vec) : d() {
    using TypeTrait = Trait2;
    using CaseTrait = Trait2;
    this->d.resize(other_vec.size());
    for (size_t i = 0; i < d.size(); i++) {
      d[i] = other_vec[i];
    }
    if (other_vec.d.im()) {
      d.setMatrix(true, other_vec.nr(), other_vec.nc());
    }
  }
  template <typename T2, typename R2, typename Trait2>
  explicit Vec(const Vec<T2, R2, Trait2> &&other_vec)
      : d(std::move(other_vec.d)) {
    using TypeTrait = Trait2;
    using CaseTrait = Trait2;
  }
  template <typename T2>
    requires std::is_same_v<T2, bool>
  explicit Vec(const Vec<T2> &other_vec) : d() {
    d.resize(other_vec.size());
    for (size_t i = 0; i < d.size(); i++)
      d[i] = other_vec[i];
  }

  T &operator[](size_t idx) { return d[idx]; }
  T operator[](size_t idx) const { return d[idx]; }

  template <typename TD>
    requires std::is_same_v<TD, BaseType>
  Vec &operator=(const TD inp) {
    static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
    static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
    static_assert(!isSubsetCalc::value,
                  "Cannot assign to subset of a calculation");
    if constexpr (isSubset::value) {
      for (size_t i = 0; i < d.ind.size(); i++) {
        d[i] = inp;
      }
    } else {
      d.resize(1);
      d[0] = inp;
    }
    return *this;
  }

   template <typename TD>
    requires std::is_same_v<TD, int>
  Vec &operator=(const TD inp) {
    static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
    static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
    static_assert(!isSubsetCalc::value,
                  "Cannot assign to subset of a calculation");
    if constexpr (isSubset::value) {
      for (size_t i = 0; i < d.ind.size(); i++) {
        d[i] = static_cast<BaseType>(inp);
      }
    } else {
      d.resize(1);
      d[0] = static_cast<BaseType>(inp);
    }
    return *this;
  }

  template <typename TD>
    requires std::is_same_v<TD, bool>
  Vec &operator=(const TD inp) {
    static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
    static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
    static_assert(!isSubsetCalc::value,
                  "Cannot assign to subset of a calculation");
    if constexpr (isSubset::value) {
      for (size_t i = 0; i < d.ind.size(); i++) {
        d[i] = static_cast<BaseType>(inp);
      }
    } else {
      d.resize(1);
      d[0] = static_cast<BaseType>(inp);
    }
    return *this;
  }

  Vec &operator=(Vec<BaseType> &other) {
    static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
    static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
    if constexpr (isSubset::value) {
      ass(other.size() == d.ind.size(),
          "number of items to replace is not a multiple of replacement length");
      for (size_t i = 0; i < d.ind.size(); i++) {
        d[i] = other[i];
      }
    } else {
        if (this == &other)
          return *this;
        if (size() != other.size()) {
          resize(other.size());
          for (size_t i = 0; i < other.size(); i++)
            d[i] = other[i];
        }  
    }
    
    return *this;
  }

  template <typename T2, typename R2, typename Trait2>
  Vec &operator=(const Vec<T2, R2, Trait2> &otherVec) {
    static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
    static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
    static_assert(!isSubsetCalc::value,
                  "Cannot assign to subset of a calculation");

    if constexpr (isBuffer::value) {
      Buffer<T> temp(otherVec.size());
      for (size_t i = 0; i < otherVec.size(); i++)
        temp[i] = otherVec[i];
      d.moveit(temp);
    } else if constexpr (isBorrow::value) {
      ass(otherVec.size() == this->size(),
          "number of items to replace is not a multiple of replacement length");
      Buffer<T> temp(otherVec.size());
      for (size_t i = 0; i < otherVec.size(); i++)
        temp[i] = otherVec[i];
      d.moveit(temp);
    } else if constexpr (isBorrowSEXP::value) {
      Buffer<T> temp(otherVec.size());
      for (size_t i = 0; i < otherVec.size(); i++)
        temp[i] = otherVec[i];
      if (otherVec.size() > this->size())
        d.resize(otherVec.size());
      d.moveit(temp);
    } else if constexpr (isSubset::value) {
      ass(otherVec.size() == d.ind.size(),
          "number of items to replace is not a multiple of replacement length");
      Buffer<T> temp(otherVec.size());
      for (size_t i = 0; i < otherVec.size(); i++) {
        temp[i] = otherVec[i];
      }
      if(d.p -> size() < temp.size()) d.resize(temp.size());
      for (size_t i = 0; i < d.ind.size(); i++) {
          d[i] = temp[i];
      }
    }
    return *this;
  }

  Vec& operator=(SEXP s) {
    d(s);
    return *this;
  }

  operator bool() const {
    ass(this -> size() == 1, "Error in if: the condition has length > 1");
    return static_cast<bool>(d[0]);
  }

  size_t size() const { return d.size(); }
  bool im() const { return d.im(); }
  size_t nc() const { return d.nc(); }
  size_t nr() const { return d.nr(); }
  auto begin() const { 
    if constexpr(isSubset::value || isSubsetCalc::value) {
      return It<T>{d.p -> p};  
    } else {
      return It<T>{d.p};  
    }
  }
  auto end() const { 
    if constexpr(isSubset::value || isSubsetCalc::value) {
      return It<T>{d.p -> p + this -> size()};  
    } else {
      return It<T>{d.p + this->size()}; 
    }
  }
  T &back() const { return d.p[this->size()]; }
  void fill(T value) { d.fill(value); }
  void resize(size_t newSize) { d.resize(newSize); }
  friend std::ostream &operator<<(std::ostream &os, const Vec &vec) {
    os << "Vec [ ";
    for (size_t i = 0; i < vec.size(); i++) {
      os << vec.d.p[i] << " ";
    }
    os << "]";
    return os;
  }
};

} // namespace etr

#endif
