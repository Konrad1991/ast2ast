#ifndef BACKWARD_H
#define BACKWARD_H

#include "UtilsTraits.hpp"

namespace etr {


/*
template<typename T>
void walk(const T& v) {
  if constexpr(std::is_arithmetic<T>::value) {
    return;
  } else {
    using trait = std::remove_reference<decltype(v)>::type::TypeTrait;
    using isVec = std::is_same<trait, etr::VectorTrait>;

    using caseTraitD = std::remove_reference<decltype(v)>::type::CaseTrait;
    using isUnaryOP = std::is_same<caseTraitD, etr::UnaryTrait>;
    using isBinaryOP = std::is_same<caseTraitD, etr::BinaryTrait>;
    //if constexpr()
  }
  return;
}
*/



template<typename T>
requires UnaryOrBinaryOperation<T>
void walk(T&& v) {
  return;
}

template<typename T>
requires NotOperation<T>
void walk(T&& v) {
  walk(v.d);
  return;
}

template<typename T>
requires std::is_arithmetic_v<T>
void walk(T&& v) {
  return;
}

} // namespace etr

#endif