#ifndef ASSING2VEC_ETR_H
#define ASSING2VEC_ETR_H

#include "VectorClass.hpp"
#include <type_traits>

template <typename TD>
  requires std::is_arithmetic_v<TD>
Vec &operator=(const TD inp) {
  static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
  static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
  static_assert(!isRVec::value,
                "Cannot assign to an r value. E.g. c(1, 2, 3) <- 1");
  if constexpr (is<TD, T>) {
    if constexpr (isSubset::value) {
      for (std::size_t i = 0; i < d.ind.size(); i++) {
        d[i] = inp;
      }
#ifdef DERIV_ETR
      for (std::size_t i = 0; i < d.ind.size(); i++) {
        deriv[i] = 0;
      }
#endif
    } else if constexpr (isBorrow::value) {
      d.sz = 1;
      d[0] = inp;
#ifdef DERIV_ETR
      deriv.resize(1);
      deriv[0] = 0;
#endif

    } else {
      d.resize(1);
      d[0] = inp;
#ifdef DERIV_ETR
      deriv.resize(1);
      deriv[0] = 0;
#endif
    }
    return *this;
  } else {
    if constexpr (isSubset::value) {
      for (std::size_t i = 0; i < d.ind.size(); i++) {
        d[i] = static_cast<T>(inp);
      }
#ifdef DERIV_ETR
      for (std::size_t i = 0; i < d.ind.size(); i++) {
        deriv[i] = 0;
      }
#endif

    } else if constexpr (isBorrow::value) {
      d.sz = 1;
      d[0] = static_cast<T>(inp);
#ifdef DERIV_ETR
      deriv.resize(1);
      deriv[0] = 0;
#endif

    } else {
      d.resize(1);
      d[0] = static_cast<T>(inp);
#ifdef DERIV_ETR
      deriv.resize(1);
      deriv[0] = 0;
#endif
    }
    return *this;
  }
}

Vec &operator=(const Vec<T, R, Trait> &otherVec) {

  static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
  static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
  static_assert(!isRVec::value,
                "Cannot assign to an r value. E.g. c(1, 2, 3) <- 1");

  using DataTypeOtherVec = typename etr::ExtractDataType<
      std::remove_reference_t<decltype(otherVec)>>::RetType;
  if constexpr (isBuffer::value) {
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    d.moveit(temp);
  } else if constexpr (isBorrow::value) {
    ass<"number of items to replace is not a multiple of replacement length">(
        otherVec.size() <= d.capacity);
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    ass<"size cannot be increased above the size of the borrowed object">(
        d.sz <= otherVec.size());
    d.sz = otherVec.size();
    for (std::size_t i = 0; i < otherVec.size(); i++)
      d[i] = temp[i];
  } else if constexpr (isBorrowSEXP::value) {
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    if (otherVec.size() > this->size())
      d.resize(otherVec.size());

    for (std::size_t i = 0; i < otherVec.size(); i++)
      d[i] = temp[i];

  } else if constexpr (isSubset::value) {
    ass<"number of items to replace is not a multiple of replacement length">(
        otherVec.size() == d.ind.size());
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    if (d.p->size() < temp.size())
      d.resize(temp.size());
    for (std::size_t i = 0; i < d.ind.size(); i++) {
      d[i % d.ind.size()] = temp[i];
    }
  }
  if (otherVec.d.im()) {
    d.setMatrix(true, otherVec.d.nr(), otherVec.d.nc());
  }

#ifdef DERIV_ETR
  // TODO: what if other Vec is subsetted
  // deriv.resize(otherVec.size());
  // for (std::size_t i = 0; i < deriv.size(); i++) {
  //   deriv[i] = otherVec.deriv[i];
  // }
  // TODO: is there something to do here?
#endif

  return *this;
}

// TODO: finish this
template <typename T2, typename R2, typename Trait2>
  requires(IsRVec<const Vec<T2, R2, Trait2>> && std::is_same_v<T, T2> &&
           isBuffer::value)
Vec &operator=(Vec<T2, R2, Trait2> &&otherVec) {

  static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
  static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
  static_assert(!isRVec::value,
                "Cannot assign to an r value. E.g. c(1, 2, 3) <- 1");
  std::size_t tempSize = otherVec.size();
  std::size_t tempCapacity = otherVec.d.capacity;
  T *tempP = otherVec.d.p;
  bool allocated = otherVec.d.allocated;

  otherVec.d.allocated = d.allocated;
  otherVec.d.sz = this->size();
  otherVec.d.capacity = d.capacity;
  otherVec.d.p = d.p;

  d.allocated = allocated;
  d.sz = tempSize;
  d.capacity = tempCapacity;
  d.p = tempP;

  if (otherVec.d.im()) {
    d.setMatrix(true, otherVec.nr(), otherVec.nc());
  }
#ifdef DERIV_ETR
  deriv.resize(this->size());
  for (std::size_t i = 0; i < deriv.size(); i++) {
    deriv[i] = 0;
  }
#endif

  return *this;
}

template <typename T2, typename R2, typename Trait2>
Vec &operator=(const Vec<T2, R2, Trait2> &otherVec) {

  static_assert(!isUnaryOP::value, "Cannot assign to unary calculation");
  static_assert(!isBinaryOP::value, "Cannot assign to binary calculation");
  static_assert(!isRVec::value,
                "Cannot assign to an r value. E.g. c(1, 2, 3) <- 1");
  using DataTypeOtherVec = typename etr::ExtractDataType<
      std::remove_reference_t<decltype(otherVec)>>::RetType;
  using typeOtherVec = std::remove_reference_t<decltype(otherVec)>;
  if constexpr (isBuffer::value) {

    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    d.moveit(temp);
  } else if constexpr (isBorrow::value) {
    ass<"number of items to replace is not a multiple of replacement length">(
        otherVec.size() <= d.capacity);
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    ass<"size cannot be increased above the size of the borrowed object">(
        d.sz <= otherVec.size());
    d.sz = otherVec.size();
    for (std::size_t i = 0; i < otherVec.size(); i++)
      d[i] = temp[i];
  } else if constexpr (isBorrowSEXP::value) {
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    if (otherVec.size() > this->size())
      d.resize(otherVec.size());

    for (std::size_t i = 0; i < otherVec.size(); i++)
      d[i] = temp[i];

  } else if constexpr (isSubset::value) {
    ass<"number of items to replace is not a multiple of replacement length">(
        otherVec.size() == d.ind.size());
    temp.resize(otherVec.size());
    for (std::size_t i = 0; i < otherVec.size(); i++) {
      if constexpr (is<DataTypeOtherVec, T>) {
        temp[i] = otherVec[i];
      } else {
        temp[i] = static_cast<T>(otherVec[i]);
      }
    }
    for (std::size_t i = 0; i < d.ind.size(); i++) {
      d[i % d.ind.size()] = temp[i];
    }
  }
  if (otherVec.d.im()) {
    d.setMatrix(true, otherVec.d.nr(), otherVec.d.nc());
  }

#ifdef DERIV_ETR
  if constexpr (IsRVec<const Vec<T2, R2, Trait2>> ||
                Operation<decltype(otherVec.d)>) {
    deriv.resize(this->size());
    // for (std::size_t i = 0; i < deriv.size(); i++) {
    //   deriv[i] = 0;
    // }
  } else {
    // deriv.resize(otherVec.size());
    // for (std::size_t i = 0; i < deriv.sz; i++) {
    //   deriv[i] = otherVec.deriv[i];
    // }
  }
#endif

  return *this;
}

#ifdef STANDALONE_ETR
#else
Vec &operator=(SEXP s) {
  d.initSEXP(s);
#ifdef DERIV_ETR
  deriv.resize(d.sz);
  deriv.fill(0.0);
#endif
  return *this;
}
#endif
#endif
