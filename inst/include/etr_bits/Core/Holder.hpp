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
template <class T>
class ConstHolder {
  std::shared_ptr<T> owned; // used only when constructed from rvalue
  const T* ptr = nullptr;

public:
  ConstHolder(const T& ref) : ptr(&ref) {}

  ConstHolder(T&& r)
    : owned(std::make_shared<T>(std::move(r))), ptr(owned.get()) {}

  ConstHolder(const ConstHolder&) = default;
  ConstHolder(ConstHolder&&) noexcept = default;
  ConstHolder& operator=(const ConstHolder&) = default;
  ConstHolder& operator=(ConstHolder&&) noexcept = default;

  const T& get() const { return *ptr; }
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
