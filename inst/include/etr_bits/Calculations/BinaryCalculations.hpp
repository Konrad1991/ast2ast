#ifndef BINARYOPERATION_ETR_HPP
#define BINARYOPERATION_ETR_HPP

namespace etr {

template <typename L, typename R, typename Trait>
struct BinaryOpClassIterator {
  ConstHolder<L> l;
  ConstHolder<R> r;
  size_t index;

  BinaryOpClassIterator(const L& l_, const R& r_, size_t index = 0)
    : l(l_), r(r_), index(index) {}

  auto operator*() const {
    if constexpr (!IsArithV<L> && IsArithV<R>) {
      return Trait::f(l.get()[index % l.get().size()], r.get());
    } else if constexpr (IsArithV<L> && !IsArithV<R>) {
      return Trait::f(l.get(), r.get()[index % r.get().size()]);
    } else if constexpr (!IsArithV<L> && !IsArithV<R>) {
      return Trait::f(l.get()[index % l.get().size()], r.get()[index % r.get().size()]);
    }
  }

  BinaryOpClassIterator& operator++() {
    ++index;
    return *this;
  }

  bool operator!=(const BinaryOpClassIterator& other) const {
    return index != other.index;
  }
};

template <typename L, typename R, typename BTrait>
inline auto determine_type_binary_op() {
  if constexpr (IsComparisonTrait<BTrait>) {
    return bool{};
  } else if constexpr (IsClassV<L> && IsClassV<R>) {
    using RetType =
    std::common_type_t<typename L::RetType, typename R::RetType>;
    return RetType{};
  } else if constexpr (!IsClassV<L> && IsClassV<R>) {
    using RetType = std::common_type_t<L, typename R::RetType>;
    return RetType{};
  } else if constexpr (IsClassV<L> && !IsClassV<R>) {
    using RetType = std::common_type_t<typename L::RetType, R>;
    return RetType{};
  }
}

template <typename L, typename R, typename BTrait> struct BinaryOperation {
  using Trait = BTrait;
  using RetType = decltype(determine_type_binary_op<L, R, BTrait>());
  using Type = decltype(determine_type_binary_op<L, R, BTrait>());
  const L &l;
  const R &r;
  using typeTraitL = L;
  using typeTraitR = R;
  using TypeTrait = BinaryTrait;

  BinaryOperation(const L &l_, const R &r_) : l(l_), r(r_) {}
  auto operator[](std::size_t i) const {
    if constexpr (!IsArithV<L> && IsArithV<R>) {
      return Trait::f(l[i % l.size()], r);
    } else if constexpr (IsArithV<L> && !IsArithV<R>) {
      return Trait::f(l, r[i % r.size()]);
    } else if constexpr (!IsArithV<L> && !IsArithV<R>) {
      return Trait::f(l[i % l.size()], r[i % r.size()]);
    }
  }
  std::size_t size() const {
    if constexpr (!IsArithV<L> && IsArithV<R>) {
      return l.size();
    } else if constexpr (IsArithV<L> && !IsArithV<R>) {
      return r.size();
    } else if constexpr (!IsArithV<L> && !IsArithV<R>) {
      return l.size() > r.size() ? l.size() : r.size();
    }
  }
  bool im() const {
    if constexpr (IsArithV<L>) {
      return r.im();
    } else if constexpr (IsArithV<R>) {
      return l.im();
    } else {
      return l.im() || r.im();
    }
  }
  std::size_t nc() const {
    if constexpr (IsArithV<L>) {
      return r.nc();
    } else if constexpr (IsArithV<R>) {
      return l.nc();
    } else {
      if (l.im() && r.im()) {
        return (l.nc() > r.nc()) ? l.nc() : r.nc();
      } else if (!l.im() && r.im()) {
        return r.nc();
      } else if (l.im() && !r.im()) {
        return l.nc();
      } else {
        ass<"Matrix calculation failed!">(false);
        return false;
      }
    }
  }
  std::size_t nr() const {
    if constexpr (IsArithV<L>) {
      return r.nr();
    } else if constexpr (IsArithV<R>) {
      return l.nr();
    } else {
      if (l.im() && r.im()) {
        return (l.nr() > r.nr()) ? l.nr() : r.nr();
      } else if (!l.im() && r.im()) {
        return r.nr();
      } else if (l.im() && !r.im()) {
        return l.nr();
      } else {
        ass<"Matrix calculation failed!">(false);
        return false;
      }
    }
  }

  // Copy constructor
  BinaryOperation(const BinaryOperation& other) : l(other.l), r(other.r) {}
  // Copy assignment
  BinaryOperation& operator=(const BinaryOperation& other) = delete;
  // Move constructor
  BinaryOperation(BinaryOperation&& other) : l(std::move(other.l)), r(std::move(other.r)) {}
  // Move assignment
  BinaryOperation& operator=(BinaryOperation&& other) = delete;

  auto begin() const { return BinaryOpClassIterator<L, R, BTrait>{l, r, 0}; }
  auto end() const {
    return BinaryOpClassIterator<L, R, BTrait>{l, r, this->size()};
  }
};

template <typename L, typename R, typename Trait>
using BinaryOpResult = decltype(determine_type_binary_op<L, R, Trait>());

template <typename L, typename R, typename Trait>
using BinaryVec =
    Vec<BinaryOpResult<L, R, Trait>, BinaryOperation<L, R, Trait>>;

template <typename L, typename R, typename Trait>
inline auto create_bin_vec(const L &l, const R &r) {
  if constexpr (!IsArithV<L> && IsArithV<R>) {
    return BinaryVec<decltype(l.d), R, Trait>(
        BinaryOperation<decltype(l.d), R, Trait>(l.d, r));
  } else if constexpr (IsArithV<L> && !IsArithV<R>) {
    return BinaryVec<L, decltype(r.d), Trait>(
        BinaryOperation<L, decltype(r.d), Trait>(l, r.d));
  } else if constexpr (!IsArithV<L> && !IsArithV<R>) {
    return BinaryVec<decltype(l.d), decltype(r.d), Trait>(
        BinaryOperation<decltype(l.d), decltype(r.d), Trait>(l.d, r.d));
  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

// TODO: does this work with R values? If not use either approach as in
// LazySubsetting or change to const L&
template <typename L, typename R> auto operator+(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, PlusTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R> auto operator-(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, MinusTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R> auto operator*(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, TimesTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R> auto operator/(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, DivideTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R> auto power(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, PowTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
  requires(IsVec<Decayed<L>> || IsVec<Decayed<R>>)
auto operator==(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, EqualTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
  requires(IsVec<Decayed<L>> || IsVec<Decayed<R>>)
auto operator!=(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, UnEqualTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R> auto operator>(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, LargerTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R> auto operator>=(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, LargerEqualTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R> auto operator<(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, SmallerTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R> auto operator<=(L &&l, R &&r) {
  return create_bin_vec<std::decay_t<L>, std::decay_t<R>, SmallerEqualTrait>(
      std::forward<L>(l), std::forward<R>(r));
}

} // namespace etr

#endif
