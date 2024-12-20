#ifndef COCA_ETR_H
#define COCA_ETR_H

namespace etr {

inline auto determine_type(const auto &rest) {
  using restType = typename ReRef<decltype(rest)>::type;
  if constexpr (IsArithV<restType>) {
    return typename ReRef<decltype(rest)>::type{};
  } else {
    using tD = ExtractedRType<restType>;
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
    using tD = ExtractedRType<firstType>;
    using firstInner = typename ExtractDataType<tD>::RetType;
    return typename std::common_type<firstInner, restType>::type{};
  } else if constexpr (IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using tD = ExtractedRType<restType>;
    using restInner = typename ExtractDataType<tD>::RetType;
    return typename std::common_type<firstType, restInner>::type{};
  } else if constexpr (!IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using tD1 = ExtractedRType<firstType>;
    using firstInner = typename ExtractDataType<tD1>::RetType;
    using tD2 = ExtractedRType<restType>;
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
    using tD = ExtractedRType<firstType>;
    using firstInner = typename ExtractDataType<tD>::RetType;
    return typename std::common_type<firstInner, restType>::type{};
  } else if constexpr (IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using tD = ExtractedRType<restType>;
    using restInner = typename ExtractDataType<tD>::RetType;
    return typename std::common_type<firstType, restInner>::type{};
  } else if constexpr (!IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using tD1 = ExtractedRType<firstType>;
    using firstInner = typename ExtractDataType<tD1>::RetType;
    using tD2 = ExtractedRType<restType>;
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

  Vec<cType, Buffer<cType, RBufferTrait>> ret(SI{size});
  std::size_t index = 0;

  forEachArg(
      [&](const auto &arg) {
        using testType =
            std::remove_const_t<ReRef<decltype(arg)>>;
        if constexpr (IsArithV<Decayed<decltype(arg)>>) {
          if constexpr (IS<testType, cType>) {
            ret[index] = arg;
          } else {
            ret[index] = static_cast<cType>(arg);
          }
          index++;
        } else {
          using InnerType = typename ReRef<decltype(arg)>::type::RetType;
          if constexpr (IS<InnerType, cType>) {
            for (int i = 0; i < arg.size(); i++) {
              ret[index + i] = arg[i];
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
