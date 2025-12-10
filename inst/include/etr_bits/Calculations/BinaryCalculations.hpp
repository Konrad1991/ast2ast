#ifndef BINARYOPERATION_ETR_HPP
#define BINARYOPERATION_ETR_HPP

namespace etr {

template <typename L, typename R, typename Trait>
struct BinaryOpClassIterator {
  const L& l;
  const R& r;
  size_t index;

  BinaryOpClassIterator(const L& l_, const R& r_, size_t index_ = 0) : l(l_), r(r_), index(index_) {}

  auto operator*() const {
    if constexpr (!IsCppArithV<L> && IsCppArithV<R>) {
      return Trait::f(l[index], r);
    } else if constexpr (IsCppArithV<L> && !IsCppArithV<R>) {
      return Trait::f(l, r[index]);
    } else if constexpr (!IsCppArithV<L> && !IsCppArithV<R>) {
      return Trait::f(l[index], r[index]);
    }
  }

  BinaryOpClassIterator& operator++() {
    ++index;
    return *this;
  }
};

struct BinaryOpSentinel {
  std::size_t n;
};

template<typename L, typename R, typename Trait>
inline bool operator!=(const BinaryOpClassIterator<L, R, Trait>& it, const BinaryOpSentinel& s) {
  return it.index < s.n;
}

template <typename L, typename R, typename BTrait>
inline auto determine_type_binary_op() {
  if constexpr (IsComparisonTrait<BTrait>) {
    return bool{};
  } else if constexpr (IsArray<L> && IsArray<R>) {
    using value_type = std::common_type_t<typename L::value_type, typename R::value_type>;
    return value_type{};
  } else if constexpr (!IsArray<L> && IsArray<R>) {
    using value_type = std::common_type_t<L, typename R::value_type>;
    return value_type{};
  } else if constexpr (IsArray<L> && !IsArray<R>) {
    using value_type = std::common_type_t<typename L::value_type, R>;
    return value_type{};
  }
}

template <typename L, typename R, typename BTrait> struct BinaryOperation {
  using Trait = BTrait;
  using Type = decltype(determine_type_binary_op<L, R, BTrait>());
  ConstHolder<L> l;
  ConstHolder<R> r;
  using typeTraitL = L;
  using typeTraitR = R;
  using TypeTrait = BinaryTrait;

  // l value & l value
  BinaryOperation(const L &l_, const R &r_) : l(l_), r(r_) {}
  // l value & r value
  BinaryOperation(const L &l_, R &&r_) : l(l_), r(std::move(r_)) {}
  // r value & l value
  BinaryOperation(L &&l_, const R &r_) : l(std::move(l_)), r(r_) {}
  // r value & r value
  BinaryOperation(L &&l_, R &&r_) : l(std::move(l_)), r(std::move(r_)) {}

  auto operator[](std::size_t i) const {
    constexpr bool is_scalar_l = IsCppArithV<L> || IsArith<L>;
    constexpr bool is_scalar_r = IsCppArithV<R> || IsArith<R>;
    if constexpr (!is_scalar_l && is_scalar_r) {
      return Trait::f(l.get()[i], r.get());
    } else if constexpr (is_scalar_l && !is_scalar_r) {
      return Trait::f(l.get(), r.get()[i]);
    } else if constexpr (!is_scalar_l && !is_scalar_r) {
      return Trait::f(l.get()[i], r.get()[i]);
    } else if constexpr (is_scalar_l && is_scalar_r) {
      return Trait::f(l.get(), r.get());
    }
  }
  std::size_t size() const {
    constexpr bool is_scalar_l = IsCppArithV<L> || IsArith<L>;
    constexpr bool is_scalar_r = IsCppArithV<R> || IsArith<R>;
    if constexpr (!is_scalar_l && is_scalar_r) {
      return l.get().size();
    } else if constexpr (is_scalar_l && !is_scalar_r) {
      return r.get().size();
    } else if constexpr (!is_scalar_l && !is_scalar_r) {
      return l.get().size() > r.get().size() ? l.get().size() : r.get().size();
    } else if constexpr (is_scalar_l && is_scalar_r) {
      return 1;
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

  auto begin() const { return BinaryOpClassIterator<L, R, BTrait>{l.get(), r.get(), 0}; }
  auto end() const { return BinaryOpSentinel{this->size()}; }
};

template <typename T, typename L, typename R, typename Trait>
using BinaryArray =
Array<T, BinaryOperation<L, R, Trait>>;


inline std::vector<std::size_t> match_dims(const std::vector<std::size_t>& l_dim, const std::vector<std::size_t>& r_dim) {
  ass<"encountered non-conformable arrays">(l_dim.size() == r_dim.size());
  for (std::size_t i = 0; i < l_dim.size(); i++) {
    if (l_dim[i] != r_dim[i]) ass<"encountered non-conformable arrays">(false);
  }
  return l_dim;
}

template <typename L, typename R, typename Trait>
inline auto create_bin_vec(L &&l,R &&r) {
  using LD = std::decay_t<L>;
  using RD = std::decay_t<R>;
  constexpr bool is_scalar_l = IsCppArithV<LD> || IsArith<LD>;
  constexpr bool is_scalar_r = IsCppArithV<RD> || IsArith<RD>;
  using T = decltype(determine_type_binary_op<LD, RD, Trait>());
  if constexpr (!is_scalar_l && is_scalar_r) {
    using Ld = std::decay_t<decltype(l.d)>;
    if constexpr(IsRvalueV<L&&>) {
      return BinaryArray<T, Ld, RD, Trait>(
        BinaryOperation<Ld, RD, Trait>(std::move(l.d), std::forward<R>(r)), l.get_dim());
    } else {
      return BinaryArray<T, Ld, RD, Trait>(
        BinaryOperation<Ld, RD, Trait>(l.d, std::forward<R>(r)), l.get_dim());
    }
  } else if constexpr (is_scalar_l && !is_scalar_r) {
    using Rd = std::decay_t<decltype(r.d)>;
    if constexpr(IsRvalueV<R&&>) {
      return BinaryArray<T, LD, Rd, Trait>(
        BinaryOperation<LD, Rd, Trait>(std::forward<L>(l), std::move(r.d)), r.get_dim());
    } else {
      return BinaryArray<T, LD, Rd, Trait>(
        BinaryOperation<LD, Rd, Trait>(std::forward<L>(l), r.d), r.get_dim());
    }
  } else if constexpr (!is_scalar_l && !is_scalar_r) {
    auto dim = match_dims(l.get_dim(), r.get_dim());
    using Ld = std::decay_t<decltype(l.d)>;
    using Rd = std::decay_t<decltype(r.d)>;
    if constexpr(IsRvalueV<L&&> && IsRvalueV<R&&>) {
      return BinaryArray<T, Ld, Rd, Trait>(
        BinaryOperation<Ld, Rd, Trait>(std::move(l.d), std::move(r.d)), std::move(dim));
    } else if constexpr(!IsRvalueV<L&&> && IsRvalueV<R&&>) {
      return BinaryArray<T, Ld, Rd, Trait>(
        BinaryOperation<Ld, Rd, Trait>(l.d, std::move(r.d)), std::move(dim));
    } else if constexpr(IsRvalueV<L&&> && !IsRvalueV<R&&>) {
      return BinaryArray<T, Ld, Rd, Trait>(
        BinaryOperation<Ld, Rd, Trait>(std::move(l.d), r.d), std::move(dim));
    } else {
      return BinaryArray<T, Ld, Rd, Trait>(
        BinaryOperation<Ld, Rd, Trait>(l.d, r.d), std::move(dim));
    }

  } else {
    ass<"This case should not be reached. Contact author">(false);
  }
}

template <typename L, typename R>
requires (IsArray<std::decay_t<L>> || IsArray<std::decay_t<R>>)
inline auto operator+(L &&l, R &&r) {
  return create_bin_vec<L, R, PlusTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires (IsArray<std::decay_t<L>> || IsArray<std::decay_t<R>>)
inline auto operator-(L &&l, R &&r) {
  return create_bin_vec<L, R, MinusTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires (IsArray<std::decay_t<L>> || IsArray<std::decay_t<R>>)
inline auto operator*(L &&l, R &&r) {
  return create_bin_vec<L, R, TimesTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires (IsArray<std::decay_t<L>> || IsArray<std::decay_t<R>>)
inline auto operator/(L &&l, R &&r) {
  return create_bin_vec<L, R, DivideTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires (IsArray<std::decay_t<L>> || IsArray<std::decay_t<R>>)
inline auto power(L &&l, R &&r) {
  return create_bin_vec<L, R, PowTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires(IsArray<Decayed<L>> || IsArray<Decayed<R>>)
inline auto operator==(L &&l, R &&r) {
  return create_bin_vec<L, R, EqualTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires(IsArray<Decayed<L>> || IsArray<Decayed<R>>)
inline auto operator!=(L &&l, R &&r) {
  return create_bin_vec<L, R, UnEqualTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires(IsArray<Decayed<L>> || IsArray<Decayed<R>>)
inline auto operator>(L &&l, R &&r) {
  return create_bin_vec<L, R, LargerTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires(IsArray<Decayed<L>> || IsArray<Decayed<R>>)
inline auto operator>=(L &&l, R &&r) {
  return create_bin_vec<L, R, LargerEqualTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires(IsArray<Decayed<L>> || IsArray<Decayed<R>>)
inline auto operator<(L &&l, R &&r) {
  return create_bin_vec<L, R, SmallerTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires(IsArray<Decayed<L>> || IsArray<Decayed<R>>)
inline auto operator<=(L &&l, R &&r) {
  return create_bin_vec<L, R, SmallerEqualTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires(IsArray<Decayed<L>> || IsArray<Decayed<R>>)
inline auto operator&(L &&l, R &&r) {
  return create_bin_vec<L, R, AndTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

template <typename L, typename R>
requires(IsArray<Decayed<L>> || IsArray<Decayed<R>>)
auto operator|(L &&l, R &&r) {
  return create_bin_vec<L, R, OrTrait>(
    std::forward<L>(l), std::forward<R>(r));
}

} // namespace etr

#endif
