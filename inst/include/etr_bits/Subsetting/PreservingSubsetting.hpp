#include "../Core.hpp"
#include "../Core/Traits.hpp"
#include <optional>
#include <type_traits>

#ifndef SUBSETTING_PRESERVING_ETR_HPP
#define SUBSETTING_PRESERVING_ETR_HPP

namespace etr {

// Iterators
// -----------------------------------------------------------------------------------------------------------
template <typename L, typename R> struct SubsetClassIterator {
  L &l;
  const R &r;
  size_t index;

  SubsetClassIterator(L &l, const R &r, size_t index = 0)
  : l(l), r(r), index(index) {}

  auto &operator*() const {
    if constexpr (IsArithV<R>) {
      return l[r];
    } else if constexpr (!IsArithV<R>) {
      return l[r[index % r.size()] - 1];
    }
  }

  SubsetClassIterator &operator++() {
    ++index;
    return *this;
  }

  bool operator!=(const SubsetClassIterator &other) const {
    return index != other.index;
  }
};

template <typename L, typename R> struct SubsetClassConstantIterator {
  L &l;
  const R &r;
  size_t index;

  SubsetClassConstantIterator(L &l, const R &r, size_t index = 0)
  : l(l), r(r), index(index) {}

  auto operator*() const {
    if constexpr (IsArithV<R>) {
      return l[r];
    } else if constexpr (!IsArithV<R>) {
      return l[r[index % r.size()] - 1];
    }
  }

  SubsetClassConstantIterator &operator++() {
    ++index;
    return *this;
  }

  bool operator!=(const SubsetClassConstantIterator &other) const {
    return index != other.index;
  }
};

// Handler which store reference to r for L values and otherwise copy the value to keep it alive
// -----------------------------------------------------------------------------------------------------------
// NOTE: SubsetClass is moved into Vec. As Holder is part of SubsetClass it also requires the copy/move constructors and assignment operators
template <typename T>
class ConstHolder {
  std::optional<T> val;
  const T* ptr = nullptr;

public:
  // Construct from lvalue
  ConstHolder(const T& ref) : ptr(&ref) {}

  // Construct from rvalue
  ConstHolder(T&& r) {
    val.emplace(std::move(r));
    ptr = &val.value();
  }

  // Copy constructor
  ConstHolder(const ConstHolder& other) {
    if (other.val.has_value()) {
      val.emplace(*other.val);
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }

  // Copy assignment
  ConstHolder& operator=(const ConstHolder& other) {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }

  // Move constructor
  ConstHolder(ConstHolder&& other) noexcept {
    if (other.val.has_value()) {
      val.emplace(std::move(*other.val));
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }

  // Move assignment
  ConstHolder& operator=(ConstHolder&& other) noexcept {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }

  const T& get() const {
    return *ptr;
  }
};

template <typename T>
class Holder {
  std::optional<T> val;
  T* ptr = nullptr;

public:
  // Construct from lvalue
  Holder(T& ref) : ptr(&ref) {}

  // Construct from rvalue
  Holder(T&& r) {
    val.emplace(std::move(r));
    ptr = &val.value();
  }

  // Copy constructor
  Holder(const Holder& other) {
    if (other.val.has_value()) {
      val.emplace(*other.val);
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }

  // Copy assignment
  Holder& operator=(Holder& other) {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }

  // Move constructor
  Holder(Holder&& other) noexcept {
    if (other.val.has_value()) {
      val.emplace(std::move(*other.val));
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }

  // Move assignment
  Holder& operator=(Holder&& other) noexcept {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }

  T& get() const {
    return *ptr;
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
      return l.get()[idx];
    } else if constexpr(IsArithV<rowT> && !IsArithV<colT>) {
      const size_t idx = (col[col_i] - 1) * l.get().nr() + row - 1;
      ++col_i;
      return l.get()[idx];
    } else if constexpr(!IsArithV<rowT> && IsArithV<colT>) {
      const size_t idx = (col - 1) * l.get().nr() + (row[row_i] - 1);
      if (++row_i == row.size()) {
        row_i = 0;
      }
      return l.get()[idx];
    } else if constexpr(IsArithV<rowT> && IsArithV<colT>) {
      const size_t idx = (col - 1) * l.get().nr() + (row - 1);
      return l.get()[idx];
    }
  }
  auto operator[](size_t i) const {
    if constexpr (IsPair<R>) { // Matrix
      return get_mat(i);
    } else if constexpr (IsBool<R> && IsArithV<R>) {
      return l.get()[i % l.get().size()];
    } else if constexpr (IsArithV<R>) {
      return l.get()[r.get() - 1];
    } else {
      return l.get()[r.get()[i % r.get().size()] - 1];
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
      return l.get()[r.get()[i % r.get().size()] - 1];
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

  auto begin() const {
    return SubsetClassIterator<L, decltype(r.get())>{l, r.get(), 0};
  } // TODO: does not work for matrices. Requires a constexpr if
  // Does it even work for vectors?
  auto end() const { return SubsetClassIterator<L, decltype(r.get())>{l, r.get(), this->size()}; }
};

// Convert boolean and double vectors to integer vectors
// -----------------------------------------------------------------------------------------------------------
template <typename V> inline void precalcVecInt(const V &vec, std::vector<std::size_t> &ind) {
  if constexpr (IsBool<V>) {
    std::size_t sizeTrue = 0;
    for (std::size_t i = 0; i < vec.size(); i++) {
      if (vec[i]) {
        sizeTrue++;
      }
    }
    ind.resize(sizeTrue);
    std::size_t counter = 0;
    for (std::size_t i = 0; i < vec.size(); i++) {
      if (vec[i]) {
        ind[counter] = i + 1;
        counter++;
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

// NOTE: R = bool | int | double
template <typename L, typename R>
requires (
IsArithV<Decayed<R>>
)
inline auto subset(L&& l, R&& r) {
  using RetType = typename ExtractDataType<Decayed<L>>::RetType;
  using RDecayed = std::decay_t<R>;
  using LDecayed = std::decay_t<L>;
  if constexpr (IsBool<RDecayed>) {
    return Vec<RetType, SubsetClass<LDecayed, RDecayed, SubsetClassTrait>>(
      SubsetClass<LDecayed, RDecayed, SubsetClassTrait>(
        std::forward<L>(l), std::forward<R>(r)));
  } else if constexpr (IsFloat<RDecayed>) {
    const std::size_t rs = static_cast<std::size_t>(std::floor(r));
    ass<"Negative indices are not supported">(rs >= 1);
    return Vec<RetType, SubsetClass<LDecayed, RDecayed, SubsetClassTrait>>(
      SubsetClass<LDecayed, RDecayed, SubsetClassTrait>(
        std::forward<L>(l), std::forward<decltype(rs)>(rs)));
  } else if constexpr (IsInteger<RDecayed>) {
    ass<"Negative indices are not supported">(r >= 1);
    return Vec<RetType, SubsetClass<LDecayed, RDecayed, SubsetClassTrait>>(
      SubsetClass<LDecayed, RDecayed, SubsetClassTrait>(
        std::forward<L>(l), std::forward<R>(r)));
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
  if constexpr (IsBool<RDecayed> || IsFloat<RDecayed>) {
    std::vector<std::size_t> ind;
    precalcVecInt(r, ind);
    return Vec<RetType, SubsetClass<LDecayed, decltype(ind), SubsetClassTrait>>(
      SubsetClass<LDecayed, decltype(ind), SubsetClassTrait>(
        std::forward<L>(l), std::forward<decltype(ind)>(ind))); // TODO: move the ind?
  } else {
    return Vec<RetType, SubsetClass<LDecayed, decltype(r.d), SubsetClassTrait>>(
      SubsetClass<LDecayed, decltype(r.d), SubsetClassTrait>(
        std::forward<L>(l), std::forward<decltype(r.d)>(r.d)));
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
  using pair_type = std::pair<decltype(r), decltype(c)>;
  return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
    SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
      std::forward<L>(l),
      std::make_pair(std::move(r), std::move(c))
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
  using CInner = decltype(col.d);
  auto r = ArithmeticHandler(l, row, true);
  using pair_type = std::pair<decltype(r), CInner>;
  return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
    SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
      std::forward<L>(l),
      std::make_pair(std::move(r), std::move(col.d))
    )
  );
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
  using RInner = decltype(row.d);
  auto c = ArithmeticHandler(l, col, true);
  using pair_type = std::pair<RInner, decltype(c)>;
  return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
    SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
      std::forward<L>(l),
      std::make_pair(std::move(row.d), std::move(c))
    )
  );
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

  if constexpr (IsInteger<RDecayed> && IsInteger<CDecayed>) {
    using pair_type = std::pair<RInner, CInner>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        std::forward<L>(l),
        std::make_pair(std::move(row.d), std::move(col.d))
      )
    );
  } else if constexpr(!IsInteger<RDecayed> && IsInteger<CDecayed>) {
    std::vector<std::size_t> ind;
    precalcVecInt(row, ind);
    using IndR = decltype(ind);
    using pair_type = std::pair<IndR, CInner>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        std::forward<L>(l),
        std::make_pair(std::move(ind), std::move(col.d))
      )
    );
  } else if constexpr(IsInteger<RDecayed> && !IsInteger<CDecayed>) {
    std::vector<std::size_t> ind;
    precalcVecInt(col, ind);
    using IndC = decltype(ind);
    using pair_type = std::pair<RInner, IndC>;
    return Mat<RetType, SubsetClass<LDecayed, pair_type, SubsetClassTrait>>(
      SubsetClass<LDecayed, pair_type, SubsetClassTrait>(
        std::forward<L>(l),
        std::make_pair(std::move(row.d), std::move(ind))
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
        std::make_pair(std::move(indR), std::move(indC))
      )
    );
  }
}

} // namespace etr

#endif
