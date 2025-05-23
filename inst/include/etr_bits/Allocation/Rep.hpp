#ifndef REP_ETR_H
#define REP_ETR_H

#include "../Core/Types.hpp"
#include <type_traits>

namespace etr {

template<typename T>
inline std::size_t ConvertTimesRep(const T& times) {
  using DecayedT = Decayed<T>;
  static_assert(IsFloat<DecayedT> || IsInteger<DecayedT>,
  "times argument of rep can only handle integer or double values");
  if constexpr(IsArithV<DecayedT>) {
    if constexpr (IsInteger<DecayedT>) {
      ass<"times in fct rep has to be a positive integer">(times >= 1);
      return times;
    } else {
      std::size_t times_ = static_cast<std::size_t>(std::floor(times));
      ass<"times in fct rep has to be a positive integer">(times_ >= 1);
      return times_;
    }
  } else {
    ass<"times in rep has to be a vector of length 1">(times.size() == 1);
    if constexpr (IsInteger<DecayedT>) {
      std::size_t times_ = times[0];
      ass<"times in fct rep has to be a positive integer">(times_ >= 1);
      return times_;
    } else {
      std::size_t times_ = static_cast<std::size_t>(std::floor(times[0]));
      ass<"times in fct rep has to be a positive integer">(times_ >= 1);
      return times_;
    }
  }
}

template <typename L, typename R>
inline auto repInternal(const L &inp, const R& times) {
  if constexpr (IsArithV<L>) {
    std::size_t length = ConvertTimesRep(times);
    Vec<L, Buffer<L, RBufferTrait>> ret(SI{length});
    ret.fill(inp);
    return ret;
  } else if constexpr (!IsArithV<L>) {
    std::size_t length = ConvertTimesRep(times) * inp.size();
    using DataType = typename ExtractDataType<Decayed<L>>::RetType;
    Vec<DataType, Buffer<DataType, RBufferTrait>> ret(SI{length});
    std::size_t counter = 0;
    for (std::size_t i = 0; i < ret.size(); i++) {
      ret[i] = inp[counter];
      counter++;
      if (counter >= inp.size())
        counter = 0;
    }
    return ret;
  }
}

template <typename L, typename R> inline auto rep(L &&inp, R &&times) {
  return repInternal(inp, times);
}

} // namespace etr

#endif
