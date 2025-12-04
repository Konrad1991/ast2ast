#ifndef CONCATENATE_ETR_HPP
#define CONCATENATE_ETR_HPP

namespace etr {

inline auto determine_type(const auto &rest) {
  using restType = typename ReRef<decltype(rest)>::type;
  if constexpr (IsArithV<restType>) {
    return typename ReRef<decltype(rest)>::type{};
  } else {
    return typename ExtractDataType<restType>::value_type{};
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
    using firstInner = typename ExtractDataType<firstType>::value_type;
    return typename std::common_type<firstInner, restType>::type{};
  } else if constexpr (IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using restInner = typename ExtractDataType<restType>::value_type;
    return typename std::common_type<firstType, restInner>::type{};
  } else if constexpr (!IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using firstInner = typename ExtractDataType<firstType>::value_type;
    using restInner = typename ExtractDataType<restType>::value_type;
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
    using firstInner = typename ExtractDataType<firstType>::value_type;
    return typename std::common_type<firstInner, restType>::type{};
  } else if constexpr (IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using restInner = typename ExtractDataType<restType>::value_type;
    return typename std::common_type<firstType, restInner>::type{};
  } else if constexpr (!IsArithV<firstType> &&
                       !IsArithV<restType>) {
    using firstInner = typename ExtractDataType<firstType>::value_type;
    using restInner = typename ExtractDataType<restType>::value_type;
    return typename std::common_type<firstInner, restInner>::type{};
  }
}

template <typename... Args> inline auto c(Args &&...args) {
  static_assert(sizeof...(Args) > 0,
  "\n\n[etr::c Error]\nYou cannot use c() without any arguments\n");

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

  Array<cType, Buffer<cType, RBufferTrait>> ret(SI{size});
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
          using InnerType = typename ReRef<decltype(arg)>::type::value_type;
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
