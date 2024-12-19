#ifndef COCA_ETR_H
#define COCA_ETR_H

#include "BufferVector.hpp"
#include "Core.hpp"
#include "Core/Concepts.hpp"
#include "Core/Reflection.hpp"
#include "Core/Traits.hpp"
#include "Core/Types.hpp"
#include <type_traits>

namespace etr {

inline auto determine_type(const auto &rest) {
  using restType = typename ReRef<decltype(rest)>::type;
  if constexpr (IsArithV<restType>) {
    return typename ReRef<decltype(rest)>::type{};
  } else {
    using tD = ExtractedTypeD<restType>;
    return typename ExtractDataType<tD>::RetType{};
  }
};

inline auto determine_type(const auto &first, const auto &rest) {
  using firstType = typename ReRef<decltype(first)>::type;
  using restType = typename ReRef<decltype(rest)>::type;
  if constexpr (IsArithV<firstType> &&
                IsArithV<restType>) {
    return typename std::common_type<firstType, restType>::type{};
  } else if constexpr (!IsArithV<firstType> &&
                       IsArithV<restType>) {
    using tD = ExtractedTypeD<firstType>;
    using firstInner = typename ExtractDataType<tD>::RetType;
    return typename std::common_type<firstInner, restType>::type{};
  } else if constexpr (IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using tD = ExtractedTypeD<restType>;
    using restInner = typename ExtractDataType<tD>::RetType;
    return typename std::common_type<firstType, restInner>::type{};
  } else if constexpr (!IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using tD1 = ExtractedTypeD<firstType>;
    using firstInner = typename ExtractDataType<tD1>::RetType;
    using tD2 = ExtractedTypeD<restType>;
    using restInner = typename ExtractDataType<tD2>::RetType;
    return typename std::common_type<firstInner, restInner>::type{};
  }
}

inline auto determine_type(const auto &first, const auto &...rest) {
  using firstType = typename ReRef<decltype(first)>::type;
  using restType = decltype(determine_type(rest...));
  if constexpr (IsArithV<firstType> &&
                IsArithV<restType>) {
    return typename std::common_type<firstType, restType>::type{};
  } else if constexpr (!IsArithV<firstType> &&
                       IsArithV<restType>) {
    using tD = ExtractedTypeD<firstType>;
    using firstInner = typename ExtractDataType<tD>::RetType;
    return typename std::common_type<firstInner, restType>::type{};
  } else if constexpr (IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using tD = ExtractedTypeD<restType>;
    using restInner = typename ExtractDataType<tD>::RetType;
    return typename std::common_type<firstType, restInner>::type{};
  } else if constexpr (!IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using tD1 = ExtractedTypeD<firstType>;
    using firstInner = typename ExtractDataType<tD1>::RetType;
    using tD2 = ExtractedTypeD<restType>;
    using restInner = typename ExtractDataType<tD2>::RetType;
    return typename std::common_type<firstInner, restInner>::type{};
  }
}

// NOTE: isnt it possible to use the function etr::c instead of etr::coca
template <class F, class... Args> inline F forEachArg(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...);
  return f;
}

template <typename... Args> inline auto coca(Args &&...args) {
  using cType = decltype(determine_type(args...));
  int size = 0;

  forEachArg(
      [&](auto arg) {
        if constexpr (IsArithV<decltype(arg)>) {
          size++;
        } else {
          size += arg.size();
        }
      },
      args...);

  Vec<cType, Buffer<cType, BufferTrait, RBufTrait>, RVecTrait> ret(SI{size});
  std::size_t index = 0;

  forEachArg(
      [&](const auto &arg) {
        using testType =
            std::remove_const_t<ReRef_t<decltype(arg)>>;
        if constexpr (IsArithV<testType>) {
          if constexpr (IS<testType, cType>) {
            ret[index] = arg;
          } else {
            ret[index] = static_cast<cType>(arg);
          }
          index++;
        } else {
          using tD = ReRef_t<decltype(arg)>;
          using tD2 = ExtractedTypeD<tD>;
          using InnerType = typename ExtractDataType<tD2>::RetType;
          // using tD = ExtractedTypeD<decltype(arg)>;
          // using InnerType = typename ExtractDataType<tD>::RetType;
          // const auto &const_arg = arg;
          if constexpr (IS<InnerType, cType>) {
            for (int i = 0; i < arg.size(); i++) {
              ret[index + i] = arg[i]; // const_arg[i];
            }
          } else {
            for (int i = 0; i < arg.size(); i++) {
              ret[index + i] = static_cast<cType>(arg[i]);
            }
          }
          index += arg.size();
        }
      },
      args...);
  return ret;
}

} // namespace etr

#endif
