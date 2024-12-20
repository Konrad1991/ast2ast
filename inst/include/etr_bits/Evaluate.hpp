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
  if constexpr(IsOpVec<T>) {
    using RetType = typename ReRef<decltype(obj)>::type::RetType;
    Vec<RetType, Buffer<RetType, RBufferTrait>> res(SI{obj.size()});
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
