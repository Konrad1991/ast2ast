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
  using restType = std::remove_reference<decltype(rest)>::type;
  if constexpr (std::is_arithmetic_v<restType>) {
    return typename std::remove_reference<decltype(rest)>::type{};
  } else {
    using tD = ExtractedTypeD<restType>;
    return typename ExtractDataType<tD>::RetType{};
  }
};

inline auto determine_type(const auto &first, const auto &rest) {
  using firstType = std::remove_reference<decltype(first)>::type;
  using restType = std::remove_reference<decltype(rest)>::type;
  if constexpr (std::is_arithmetic_v<firstType> &&
                std::is_arithmetic_v<restType>) {
    return typename std::common_type<firstType, restType>::type{};
  } else if constexpr (!std::is_arithmetic_v<firstType> &&
                       std::is_arithmetic_v<restType>) {
    using tD = ExtractedTypeD<firstType>;
    using firstInner = ExtractDataType<tD>::RetType;
    return typename std::common_type<firstInner, restType>::type{};
  } else if constexpr (std::is_arithmetic_v<firstType> &&
                       !std::is_arithmetic_v<restType>) {
    using tD = ExtractedTypeD<restType>;
    using restInner = ExtractDataType<tD>::RetType;
    return typename std::common_type<firstType, restInner>::type{};
  } else if constexpr (!std::is_arithmetic_v<firstType> &&
                       !std::is_arithmetic_v<restType>) {
    using tD1 = ExtractedTypeD<firstType>;
    using firstInner = ExtractDataType<tD1>::RetType;
    using tD2 = ExtractedTypeD<restType>;
    using restInner = ExtractDataType<tD2>::RetType;
    return typename std::common_type<firstInner, restInner>::type{};
  }
}

inline auto determine_type(const auto &first, const auto &...rest) {
  using firstType = std::remove_reference<decltype(first)>::type;
  using restType = decltype(determine_type(rest...));
  if constexpr (std::is_arithmetic_v<firstType> &&
                std::is_arithmetic_v<restType>) {
    return typename std::common_type<firstType, restType>::type{};
  } else if constexpr (!std::is_arithmetic_v<firstType> &&
                       std::is_arithmetic_v<restType>) {
    using tD = ExtractedTypeD<firstType>;
    using firstInner = ExtractDataType<tD>::RetType;
    return typename std::common_type<firstInner, restType>::type{};
  } else if constexpr (std::is_arithmetic_v<firstType> &&
                       !std::is_arithmetic_v<restType>) {
    using tD = ExtractedTypeD<restType>;
    using restInner = ExtractDataType<tD>::RetType;
    return typename std::common_type<firstType, restInner>::type{};
  } else if constexpr (!std::is_arithmetic_v<firstType> &&
                       !std::is_arithmetic_v<restType>) {
    using tD1 = ExtractedTypeD<firstType>;
    using firstInner = ExtractDataType<tD1>::RetType;
    using tD2 = ExtractedTypeD<restType>;
    using restInner = ExtractDataType<tD2>::RetType;
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
        if constexpr (std::is_arithmetic_v<decltype(arg)>) {
          size++;
        } else {
          size += arg.size();
        }
      },
      args...);

  Vec<cType, Buffer<cType, BufferTrait, RBufTrait>, RVecTrait> ret(SI{size});
  std::size_t index = 0;

  forEachArg(
      [&](auto &&arg) {
        using testType =
            std::remove_const_t<std::remove_reference_t<decltype(arg)>>;
        if constexpr (std::is_arithmetic_v<testType>) {
          if constexpr (std::is_same_v<testType, cType>) {
            ret[index] = arg;
          } else {
            ret[index] = static_cast<cType>(arg);
          }
          index++;
        } else {
          using tD = std::remove_reference_t<decltype(arg)>;
          using tD2 = ExtractedTypeD<tD>;
          using InnerType = typename ExtractDataType<tD2>::RetType;
          // using tD = ExtractedTypeD<decltype(arg)>;
          // using InnerType = typename ExtractDataType<tD>::RetType;
          const auto &const_arg = arg;
          if constexpr (std::is_same_v<InnerType, cType>) {
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

template <int Idx, typename AV, typename... Args>
inline auto coca(AV &av, Args &&...args) {
  using cType = decltype(determine_type(args...));
  int size = 0;
  forEachArg(
      [&](auto arg) {
        if constexpr (std::is_arithmetic_v<decltype(arg)>) {
          size++;
        } else {
          size += arg.size();
        }
      },
      args...);

  av.varConstants[Idx].resize(size);
  std::size_t index = 0;

  forEachArg(
      [&](auto &&arg) {
        using testType =
            std::remove_const_t<std::remove_reference_t<decltype(arg)>>;
        if constexpr (std::is_arithmetic_v<testType>) {
          if constexpr (std::is_same_v<testType, cType>) {
            av.varConstants[Idx][index] = arg;
          } else {
            av.varConstants[Idx][index] = static_cast<cType>(arg);
          }
          index++;
        } else {
          using tD = std::remove_reference_t<decltype(arg)>;
          using tD2 = ExtractedTypeD<tD>;
          using InnerType = typename ExtractDataType<tD2>::RetType;
          if constexpr (std::is_same_v<InnerType, cType>) {
            for (int i = 0; i < arg.size(); i++) {
              av.varConstants[Idx][index + i] = arg[i];
            }
          } else {
            for (int i = 0; i < arg.size(); i++) {
              av.varConstants[Idx][index + i] = static_cast<cType>(arg[i]);
            }
          }
          index += arg.size();
        }
      },
      args...);

  Vec<double, VarPointer<decltype(av), Idx, -1, ConstantTypeTrait>,
      ConstantTypeTrait>
      ret(av); // TODO: add ConstantTypeTrait to each VarPointer in fct such as
               // coca
  return ret;
}

} // namespace etr

#endif
