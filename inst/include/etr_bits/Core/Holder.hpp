#ifndef HOLDER_ETR_HPP
#define HOLDER_ETR_HPP

#include <cmath>
#include <limits>

#ifdef STANDALONE_ETR
#include "stddef.h"
#include <cxxabi.h>
#else
// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#endif

#include <algorithm>
#include <array>
#include <ios>
#include <iostream>
#include <iterator>
#include <math.h>
#include <memory>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>
#include <functional>
#include <numeric>

namespace etr {
// Handler which store reference to r for L values and otherwise copy the value to keep it alive
// -----------------------------------------------------------------------------------------------------------
template <typename T>
class ConstHolder {
  std::optional<T> val;
  const T* ptr = nullptr;

public:
  // Construct from lvalue
  ConstHolder(const T& ref) : ptr(&ref) {}

  // Construct from rvalue
  ConstHolder(T&& r) {
    val.emplace(std::move(r));
    ptr = &val.value();
  }

  // Copy constructor
  ConstHolder(const ConstHolder& other) {
    if (other.val.has_value()) {
      val.emplace(*other.val);
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }

  // Copy assignment
  ConstHolder& operator=(const ConstHolder& other) {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }

  // Move constructor
  ConstHolder(ConstHolder&& other) noexcept {
    if (other.val.has_value()) {
      val.emplace(std::move(*other.val));
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }

  // Move assignment
  ConstHolder& operator=(ConstHolder&& other) noexcept {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }

  const T& get() const {
    return *ptr;
  }
};


template <typename T>
class Holder {
  std::optional<T> val;
  T* ptr = nullptr;

public:
  // Construct from lvalue
  Holder(T& ref) : ptr(&ref) {}

  // Construct from rvalue
  Holder(T&& r) {
    val.emplace(std::move(r));
    ptr = &val.value();
  }

  // Copy constructor
  Holder(const Holder& other) {
    if (other.val.has_value()) {
      val.emplace(*other.val);
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }

  // Copy assignment
  Holder& operator=(const Holder& other) {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }

  // Move constructor
  Holder(Holder&& other) noexcept {
    if (other.val.has_value()) {
      val.emplace(std::move(*other.val));
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }

  // Move assignment
  Holder& operator=(Holder&& other) noexcept {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }

  T& get() const {
    return *ptr;
  }
};

}
#endif
