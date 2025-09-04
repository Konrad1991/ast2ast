#include "../Core.hpp"
#include "../Core/Traits.hpp"
#include <optional>
#include <type_traits>
#include "HelperSubsetting.hpp"

#ifndef SUBSETTING_PRESERVING_ETR_HPP
#define SUBSETTING_PRESERVING_ETR_HPP

namespace etr {

// Iterators
// -----------------------------------------------------------------------------------------------------------
template <typename Subset>
struct SubsetClassIterator {
  Holder<Subset> subset;
  size_t index;

  SubsetClassIterator(Subset& subset, size_t index = 0)
    : subset(subset), index(index) {}

  SubsetClassIterator(Subset&& subset, size_t index = 0)
    : subset(std::move(subset)), index(index) {}

  auto operator*() const {
    return subset.get()[index];
  }

  SubsetClassIterator& operator++() {
    ++index;
    return *this;
  }

  bool operator!=(const SubsetClassIterator& other) const {
    return index != other.index;
  }
};

// The SubsetClass
// -----------------------------------------------------------------------------------------------------------
template <typename L, typename R, typename Trait>
class SubsetClass {
public:
  using Type = typename ReRef<L>::type::RetType;
  using RetType = typename ReRef<L>::type::RetType;
  using TypeTrait = Trait;
  Holder<L> l;
  ConstHolder<R> r;

  mutable std::size_t row_i = 0;
  mutable std::size_t col_i = 0;

  // L is l value & R is l value
  SubsetClass(L& l_, const R& r_) : l(l_), r(r_) {}
  // L is l value & R is r value
  SubsetClass(L& l_, R&& r_) : l(l_), r(std::move(r_)) {}
  // L is r value & R is l value
  SubsetClass(L&& l_, const R& r_) : l(std::move(l_)), r(r_) {}
  // L is r value & R is r value
  SubsetClass(L&& l_, R&& r_) : l(std::move(l_)), r(std::move(r_)) {}

  auto& get_mat(size_t i) {
    const auto& [row, col] = r.get();
    using rowT = decltype(row);
    using colT = decltype(col);
    if constexpr (!IsArithV<rowT> && !IsArithV<colT>) {
      const size_t idx = (col[col_i] - 1) * l.get().nr() + (row[row_i] - 1);
      if (++row_i == row.size()) {
        row_i = 0;
        ++col_i;
      }
      return l.get()[idx % l.get().size()];
    } else if constexpr(IsArithV<rowT> && !IsArithV<colT>) {
      const size_t idx = (col[col_i] - 1) * l.get().nr() + row - 1;
      ++col_i;
      return l.get()[idx % l.get().size()];
    } else if constexpr(!IsArithV<rowT> && IsArithV<colT>) {
      const size_t idx = (col - 1) * l.get().nr() + (row[row_i] - 1);
      if (++row_i == row.size()) {
        row_i = 0;
      }
      return l.get()[idx % l.get().size()];
    } else if constexpr(IsArithV<rowT> && IsArithV<colT>) {
      const size_t idx = (col - 1) * l.get().nr() + (row - 1);
      return l.get()[idx % l.get().size()];
    }
  }
  auto get_mat_const(size_t i) const {
    const auto& [row, col] = r.get();
    using rowT = decltype(row);
    using colT = decltype(col);
    if constexpr (!IsArithV<rowT> && !IsArithV<colT>) {
      const size_t idx = (col[col_i] - 1) * l.get().nr() + (row[row_i] - 1);
      if (++row_i == row.size()) {
        row_i = 0;
        ++col_i;
      }
      return l.get()[idx % l.get().size()];
    } else if constexpr(IsArithV<rowT> && !IsArithV<colT>) {
      const size_t idx = (col[col_i] - 1) * l.get().nr() + row - 1;
      ++col_i;
      return l.get()[idx % l.get().size()];
    } else if constexpr(!IsArithV<rowT> && IsArithV<colT>) {
      const size_t idx = (col - 1) * l.get().nr() + (row[row_i] - 1);
      if (++row_i == row.size()) {
        row_i = 0;
      }
      return l.get()[idx % l.get().size()];
    } else if constexpr(IsArithV<rowT> && IsArithV<colT>) {
      const size_t idx = (col - 1) * l.get().nr() + (row - 1);
      return l.get()[idx % l.get().size()];
    }
  }
  auto operator[](size_t i) const {
    if constexpr (IsPair<R>) { // Matrix
      return get_mat_const(i);
    } else if constexpr (IsBool<R> && IsArithV<R>) {
      return l.get()[i % l.get().size()];
    } else if constexpr (IsArithV<R>) {
      return l.get()[r.get() - 1];
    } else {
      return l.get()[((r.get()[i % r.get().size()]) - 1) % l.get().size() ];
    }
  }
  auto& operator[](size_t i) {
    if constexpr (IsPair<R>) { // Matrix
      return get_mat(i);
    } else if constexpr (IsBool<R> && IsArithV<R>) {
      return l.get()[i % l.get().size()];
    } else if constexpr (IsArithV<R>) {
      return l.get()[r.get() - 1];
    } else {
      return l.get()[((r.get()[i % r.get().size()]) - 1) % l.get().size() ];
    }
  }

  size_t size_mat() const {
    const auto& [row, col] = r.get();
    using rowT = decltype(row);
    using colT = decltype(col);
    if constexpr (!IsArithV<rowT> && !IsArithV<colT>) {
      return row.size() * col.size();
    } else if constexpr(IsArithV<rowT> && !IsArithV<colT>) {
      return col.size();
    } else if constexpr(!IsArithV<rowT> && IsArithV<colT>) {
      return row.size();
    } else if constexpr(IsArithV<rowT> && IsArithV<colT>) {
      return 1;
    }
  }
  size_t size() const {
    if constexpr (IsPair<R>) { // Matrix
      return size_mat();
    } else if constexpr (IsBool<R> && IsArithV<R>) {
      return r.get() ? l.get().size() : 0;
    } else if constexpr (IsArithV<R>) {
      return 1;
    } else {
      return r.get().size();
    }
  }
  bool im() const {
    if constexpr (IsMat<L>) {
      return l.get().im();
    } else {
      return false;
    }
  }
  size_t nr_mat() const {
    const auto& row = r.get().first;
    using rowT = Decayed<decltype(row)>;
    if constexpr (IsArithV<rowT>) {
      return 1;
    } else {
      return row.size();
    }
  }
  std::size_t nr() const {
    if constexpr (IsPair<R>) {
      return nr_mat();
    } else if constexpr (IsMat<L>) {
      return l.get().nr();
    } else {
      return false;
    }
  }
  size_t nc_mat() const {
    const auto& col = r.get().second;
    using colT = Decayed<decltype(col)>;
    if constexpr (IsArithV<colT>) {
      return 1;
    } else {
      return col.size();
    }
  }
  std::size_t nc() const {
    if constexpr (IsPair<R>) {
      return nc_mat();
    } else if constexpr (IsMat<L>) {
      return l.get().nc();
    } else {
      return false;
    }
  }

  // Copy constructor
  SubsetClass(const SubsetClass& other) : l(other.l), r(other.r) {}
  // Copy assignment
  SubsetClass& operator=(const SubsetClass& other) {
    l = other.l;
    r = other.r;
    return *this;
  };
  // Move constructor
  SubsetClass(SubsetClass&& other) : l(std::move(other.l)), r(std::move(other.r)) {}
  // Move assignment
  SubsetClass& operator=(SubsetClass&& other) {
    l = std::move(other.l);
    r = std::move(other.r);
    return *this;
  }

  auto begin() {
    return SubsetClassIterator<SubsetClass>{*this, 0};
  }
  auto end() {
    return SubsetClassIterator<SubsetClass>{*this, this->size()};
  }
  auto begin() const {
    return SubsetClassIterator<const SubsetClass>{*this, 0};
  }
  auto end() const {
    return SubsetClassIterator<const SubsetClass>{*this, this->size()};
  }

};

// Convert boolean and double vectors to integer vectors
// -----------------------------------------------------------------------------------------------------------
template <typename V>
inline void precalcVecInt(const V &vec, std::vector<std::size_t> &ind) {
  if constexpr (IsBool<V>) {
    ind.clear();
    for (std::size_t i = 0; i < vec.size(); i++) {
      if (vec[i]) {
        ind.push_back(i + 1);  // 1-based indexing
      }
    }
  } else if constexpr (IsFloat<V>) {
    ind.resize(vec.size());
    for (std::size_t i = 0; i < ind.size(); ++i) {
      ind[i] = static_cast<std::size_t>(std::floor(vec[i]));
    }
  }
}


// Vector subsetting
// -----------------------------------------------------------------------------------------------------------
/* Gets subsetted:          Subsetted with:       Status
1  Vec                                 bool       Done
2  Vec                                  int       Done
3  Vec                               double       Done
4  Vec                              VecBool       Done
5  Vec                       VecIntOrDouble       Done
5  Vec                          Calculation       Done
*/

// TODO: change all entries to auto&& and adapt the decltype and the creation of the pairs

template <typename RetType, typename L, typename R>
using SubsetVecType = Vec<RetType, SubsetClass<L, R, SubsetClassTrait>>;

using IndType = std::vector<std::size_t>;

// NOTE: R = bool | int | double
template <typename L, typename R>
requires (
IsArithV<Decayed<R>>
)
inline auto subset(L&& l, R&& r) {
  using RDecayed = std::decay_t<R>;
  using LDecayed = std::decay_t<L>;
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;

  auto&& lf = std::forward<L>(l);
  if constexpr (IsBool<RDecayed>) {
    return SubsetVecType<RetType, LDecayed, RDecayed>(
      SubsetClass<LDecayed, RDecayed, SubsetClassTrait>(lf, std::forward<R>(r))
    );
  } else if constexpr (IsFloat<RDecayed>) {
    const std::size_t rs = safe_index_from_double(r);
    // const std::size_t rs = static_cast<std::size_t>(std::floor(r));
    ass<"Negative indices are not supported">(rs >= 1);
    using RSType = decltype(rs);
    return SubsetVecType<RetType, LDecayed, RSType>(
      SubsetClass<LDecayed, RSType, SubsetClassTrait>(lf, std::forward<RSType>(rs))
    );
  } else if constexpr (IsInteger<RDecayed>) {
    ass<"Negative indices are not supported">(r >= 1);
    return SubsetVecType<RetType, LDecayed, RDecayed>(
      SubsetClass<LDecayed, RDecayed, SubsetClassTrait>(lf, std::forward<R>(r))
    );
  } else {
    ass<"Found unsupported type in subsetting">(false);
  }
}

// NOTE: R = Vec or Mat<bool|int|double>
template <typename L, typename R>
requires (!IsArithV<Decayed<R>> && IsArrayLike<Decayed<L>>)
inline auto subset(L&& l, R&& r) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  using RDecayed = std::decay_t<R>;
  using LDecayed = std::decay_t<L>;
  auto&& lf = std::forward<L>(l);
  if constexpr (IsBool<RDecayed> || IsFloat<RDecayed>) {
    std::vector<std::size_t> ind;
    precalcVecInt(r, ind);
    return SubsetVecType<RetType, LDecayed, IndType>(
      SubsetClass<LDecayed, IndType, SubsetClassTrait>(lf, std::forward<IndType>(ind))
    );
  } else {
    auto&& r_d = std::forward<R>(r).d;
    using RDType = decltype(r.d);
    return SubsetVecType<RetType, LDecayed, RDType>(
      SubsetClass<LDecayed, RDType, SubsetClassTrait>(lf, r_d)
    );
  }
}

// Convert for matric subsetting
// -----------------------------------------------------------------------------------------------------------
template<typename V>
inline std::vector<std::size_t> precalcVecAlongOneDim(const V& vec, bool obj, bool is_row) {
  std::vector<std::size_t> ind;
  if (!obj) return ind;
  if (is_row) {
    ind.resize(vec.nr());
  } else {
    ind.resize(vec.nc());
  }
  for (size_t i = 0; i < ind.size(); i++) {
    ind[i] = i + 1;
  }
  return ind;
}

template<typename V, typename O>
inline auto ArithmeticHandler(const V& vec, const O& obj, bool is_row) {
  using DecayedO = Decayed<O>;
  if constexpr(IsBool<DecayedO>) {
    return precalcVecAlongOneDim(vec, obj, is_row);
  } else if constexpr(IsFloat<DecayedO>) {
    return static_cast<std::size_t>(std::floor(obj));
  } else {
    return obj;
  }
}

// Matrix subsetting
// -----------------------------------------------------------------------------------------------------------
/* Gets subsetted:          Rows subsetted with:          Cols subsetted with:        Status
2  Mat                      int, double, bool             int, double, bool           Done
2  Mat                      int, double, bool             VecIntOrDouble              Done
2  Mat                      VecIntOrDouble                int, double, bool           Done
2  Mat                      int, double, bool             VecBool                     Done
2  Mat                      VecBool                       int, double, bool           Done
4  Mat                      VecBool                       VecIntOrDouble              Done
4  Mat                      VecIntOrDouble                VecBool                     Done
4  Mat                      VecBool                       VecBool                     Done
5  Mat                      VecIntOrDouble                VecIntOrDouble              Done
*/

template <typename RetType, typename L, typename R>
using SubsetMatType = Mat<RetType, SubsetClass<L, R, SubsetClassTrait>>;

// NOTE: R and C are arithmetic values
template <typename L, typename R, typename C>
requires(
  IsMat<Decayed<L>> && IsArithV<Decayed<R>> && IsArithV<Decayed<C>>
)
inline auto subset(L&& l, R&& row, C&& col) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  using LDecayed = std::decay_t<L>;
  auto r = ArithmeticHandler(l, row, true);
  auto c = ArithmeticHandler(l, col, false);
  auto&& lf = std::forward<L>(l);
  using pair_type = std::pair<decltype(r), decltype(c)>;
  return SubsetMatType<RetType, LDecayed, pair_type>(
    SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
      lf,
      std::pair<decltype(r), decltype(c)>{r, c}
    )
  );
}
// NOTE: R is arithmetic
template <typename L, typename R, typename C>
requires(
  IsMat<Decayed<L>> && IsArithV<Decayed<R>> && IsArrayLike<Decayed<C>>
)
inline auto subset(L&& l, R&& row, C&& col) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  using LDecayed = std::decay_t<L>;
  using CDecayed = std::decay_t<C>;
  auto r = ArithmeticHandler(l, row, true);
  auto&& col_d = std::forward<C>(col).d;
  auto&& lf = std::forward<L>(l);
  if constexpr (IsBool<CDecayed> || IsFloat<CDecayed>) {
    std::vector<std::size_t> ind;
    precalcVecInt(col, ind);
    using pair_type = std::pair<decltype(r), IndType>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        lf,
        std::pair<decltype(r), IndType>(std::forward<decltype(r)>(r), std::move(ind))
    ));
  } else {
    using CInner = decltype(col.d);
    using pair_type = std::pair<decltype(r), decltype(col_d)&>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        lf,
        std::pair<decltype(r), decltype(col_d)&>{r, col_d}
      )
    );
  }
}
// NOTE: C is arithmetic
template <typename L, typename R, typename C>
requires(
IsMat<Decayed<L>> && IsArrayLike<Decayed<R>> && IsArithV<Decayed<C>>
)
inline auto subset(L&& l, R&& row, C&& col) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  using LDecayed = std::decay_t<L>;
  using RDecayed = std::decay_t<R>;
  auto c = ArithmeticHandler(l, col, true);
  auto&& row_d = std::forward<R>(row).d;
  auto&& lf = std::forward<L>(l);
  if constexpr (IsBool<RDecayed> || IsFloat<RDecayed>) {
    std::vector<std::size_t> ind;
    precalcVecInt(row, ind);
    using pair_type = std::pair<IndType, decltype(c)>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        lf,
        std::pair<IndType, decltype(c)>{std::move(ind), std::forward<decltype(c)>(c)}
      ));
  } else {
    using pair_type = std::pair<decltype(row_d)&, decltype(c)>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        lf,
        std::pair<decltype(row_d)&, decltype(c)>{row_d, c}
      ));
  }
}

// NOTE: R and C are Vec or Mat<bool|int|double>
template <typename L, typename R, typename C>
requires(
  IsMat<Decayed<L>> && IsArrayLike<Decayed<R>> && IsArrayLike<Decayed<C>>
)
inline auto subset(L&& l, R&& row, C&& col) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  using LDecayed = std::decay_t<L>;
  using RDecayed = std::decay_t<R>;
  using CDecayed = std::decay_t<C>;
  using RInner = decltype(row.d);
  using CInner = decltype(col.d);

  if constexpr ((!IsFloat<RDecayed>) && (!IsFloat<CDecayed>) && (!IsBool<RDecayed>) && (!IsBool<CDecayed>)) {
    auto&& row_d = std::forward<R>(row).d;
    auto&& col_d = std::forward<C>(col).d;
    using pair_type = std::pair<decltype(row_d)&, decltype(col_d)&>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        std::forward<L>(l),
        std::pair<decltype(row_d)&, decltype(col_d)&>{row_d, col_d}
      )
    );
  } else if constexpr((IsInteger<CDecayed> && !IsBool<CDecayed>) && (IsFloat<RDecayed> || IsBool<RDecayed>) ) {
    std::vector<std::size_t> ind;
    precalcVecInt(row, ind);
    using IndR = decltype(ind);
    auto&& col_d = std::forward<C>(col).d;
    using pair_type = std::pair<IndR, decltype(col_d)&>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        std::forward<L>(l),
        std::pair<IndR, decltype(col_d)&>{std::move(ind), col_d}
      )
    );
  } else if constexpr((IsInteger<RDecayed> && !IsBool<RDecayed>) && (IsFloat<CDecayed> || IsBool<CDecayed>)) {
    std::vector<std::size_t> ind;
    precalcVecInt(col, ind);
    using IndC = decltype(ind);
    auto&& row_d = std::forward<R>(row).d;
    using pair_type = std::pair<decltype(row_d)&, IndC>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        std::forward<L>(l),
        std::pair<decltype(row_d)&, IndC>{row_d, std::move(ind)}
      )
    );
  } else {
    std::vector<std::size_t> indR;
    std::vector<std::size_t> indC;
    precalcVecInt(row, indR);
    precalcVecInt(col, indC);
    using IndR = decltype(indR);
    using IndC = decltype(indC);
    using pair_type = std::pair<IndR, IndC>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        std::forward<L>(l),
        std::pair<IndType, IndType>{std::move(indR), std::move(indC)}
      )
    );
  }
}

// Prevent error functions
// -----------------------------------------------------------------------------------------------------------
template <typename L, typename R>
requires (IsArithV<Decayed<L>>)
inline auto subset(L&&, R&&) {
  static_assert(
    false,
    "\n\n"
    "[etr::subset Error]\n"
    "You tried to subset an arithmetic value with a single index.\n"
    "This operation is not supported.\n"
    "Hint: Wrap your value in 'c' if you intend to treat it as a vector.\n"
    "\n"
  );
}

template <typename L, typename R, typename C>
requires (IsArithV<Decayed<L>>)
inline auto subset(L&&, R&&, C&&) {
  static_assert(
    false,
    "\n\n"
    "[etr::subset Error]\n"
    "You tried to subset an arithmetic value with two indices (row, col).\n"
    "This operation is not supported.\n"
    "Hint: Use subset(mat, row, col) for matrices.\n"
    "      Use subset(vec, index) for vectors.\n"
    "      Scalar values like '42' cannot be subset.\n"
    "\n"
  );
}

// NOTE: User uses a vector instead of a matrix
// The only purpose is to throw a compiler error
template<typename L, typename R, typename C>
requires(
  IsVec<Decayed<L>>
)
inline auto subset(L&&l, R&& row, C&& col) {
    static_assert(
    false,
    "\n\n\n"
    "[etr::subset Error]\n"
    "You tried to subset a vector with two arguments (rows and columns),\n"
    "but this form of subsetting is only valid for matrices.\n"
    "Hint: Use subset(vec, index) for vectors.\n"
    "      Use subset(mat, row_index, col_index) for matrices."
    "\n\n\n"
  );
}

} // namespace etr

#endif
