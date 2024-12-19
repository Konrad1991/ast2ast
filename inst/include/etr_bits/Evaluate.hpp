#include "BufferVector.hpp"
#include "Core.hpp"
#include <cstddef>

#ifndef EVALUATE_HPP
#define EVALUATE_HPP


namespace etr {
// NOTE: evaluates a Subset or a calculation and returns the result
// If something else is passed as argument it is forwarded

template<typename T>
inline auto Evaluate(T && obj) {
  using CaseTrait =
        typename std::remove_reference<decltype(obj.d)>::type::CaseTrait;
  if constexpr(IS<CaseTrait, UnaryTrait> ||
    IS<CaseTrait, BinaryTrait> ||
    IS<CaseTrait, SubsetClassTrait>) {
    using RetType = typename std::remove_reference<decltype(obj)>::type::RetType;
    Vec<RetType, Buffer<RetType, BufferTrait>, VectorTrait> res(SI{obj.size()});
    for (size_t i = 0; i < res.size(); i++) {
      res[i] = obj.d[i];
    }
    return res;
  } else {
    return std::forward<decltype(obj)>(obj);
  }
}

}

#endif
