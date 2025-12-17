#include "HelperSubsetting.hpp"

#ifndef SUBSETTING_PRESERVING_ETR_HPP
#define SUBSETTING_PRESERVING_ETR_HPP

namespace etr {

// Iterators
// -----------------------------------------------------------------------------------------------------------
template <typename Subset>
struct SubsetViewIterator {
  Holder<Subset> subset;
  size_t index;

  SubsetViewIterator(Subset& subset_, size_t index_ = 0)
    : subset(subset_), index(index_) {}

  SubsetViewIterator(Subset&& subset_, size_t index_ = 0)
    : subset(std::move(subset_)), index(index_) {}

  auto operator*() const {
    return subset.get().get(index);
  }

  SubsetViewIterator& operator++() {
    ++index;
    return *this;
  }

  bool operator!=(const SubsetViewIterator& other) const {
    return index != other.index;
  }
};

// The SubsetView
// -----------------------------------------------------------------------------------------------------------
template <typename O, std::size_t N, typename Trait>
struct SubsetView {
public:
  using TypeTrait = Trait;
  using value_type = typename ReRef<O>::type::value_type;
  Holder<O> obj;
  ConstHolder<Buffer<int>> indices;

  SubsetView(O& obj_, Buffer<int>&& indices_) :
    obj(obj_), indices(std::move(indices_)) {}
  SubsetView(O&& obj_, Buffer<int>&& indices_) :
    obj(obj_), indices(std::move(indices_)) {}

  auto get(std::size_t i) const {
    return obj.get().get(
      safe_modulo(
        indices.get().get(
          safe_modulo(i, indices.get().size())
        )
        ,
        obj.get().size()
      )
    );
  }
  template<typename Val>
  void set(std::size_t i, const Val& v) const {
    if constexpr (IS<Decayed<Val>, value_type>) {
      return obj.get().set(
        safe_modulo(
          indices.get().get(
            safe_modulo(i, indices.get().size())
          )
          ,
          obj.get().size()
        ),
        v
      );
    } else {
      return obj.get().set(
        safe_modulo(
          indices.get().get(
            safe_modulo(i, indices.get().size())
          )
          ,
          obj.get().size()
        ),
        static_cast<value_type>(v)
      );
    }
  }
  std::size_t size() const {return indices.get().size();}

  // Copy constructor
  SubsetView(const SubsetView& other) : obj(other.obj), indices(other.indices) {}
  // Copy assignment
  SubsetView& operator=(const SubsetView& other) {
    obj = other.obj;
    indices = other.indices;
    return *this;
  };
  // Move constructor
  SubsetView(SubsetView&& other) : obj(std::move(other.obj)), indices(std::move(other.indices)) {}
  // Move assignment
  SubsetView& operator=(SubsetView&& other) {
    obj = std::move(other.obj);
    indices = std::move(other.indices);
    return *this;
  }

  auto begin() { return SubsetViewIterator<SubsetView>{*this, 0}; }
  auto end() { return SubsetViewIterator<SubsetView>{*this, this->size()}; }
  auto begin() const { return SubsetViewIterator<const SubsetView>{*this, 0}; }
  auto end() const { return SubsetViewIterator<const SubsetView>{*this, this->size()}; }
};

// The const SubsetView
// -----------------------------------------------------------------------------------------------------------
template <typename O, std::size_t N, typename Trait>
struct ConstSubsetView {
public:
  using TypeTrait = Trait;
  using value_type = typename ReRef<O>::type::value_type;
  ConstHolder<O> obj;
  ConstHolder<Buffer<int>> indices;

  ConstSubsetView(O& obj_, Buffer<int>&& indices_) :
    obj(obj_), indices(std::move(indices_)) {}
  ConstSubsetView(O&& obj_, Buffer<int>&& indices_) :
    obj(obj_), indices(std::move(indices_)) {}

  auto get(std::size_t i) const {
    return obj.get().get(
      safe_modulo(
        indices.get().get(
          safe_modulo(i, indices.get().size())
        )
        ,
        obj.get().size()
      )
    );
  }
  std::size_t size() const {return indices.get().size();}

  // Copy constructor
  ConstSubsetView(const ConstSubsetView& other) : obj(other.obj), indices(other.indices) {}
  // Copy assignment
  ConstSubsetView& operator=(const ConstSubsetView& other) {
    obj = other.obj;
    indices = other.indices;
    return *this;
  };
  // Move constructor
  ConstSubsetView(ConstSubsetView&& other) : obj(std::move(other.obj)), indices(std::move(other.indices)) {}
  // Move assignment
  ConstSubsetView& operator=(ConstSubsetView&& other) {
    obj = std::move(other.obj);
    indices = std::move(other.indices);
    return *this;
  }

  auto begin() const { return SubsetViewIterator<const ConstSubsetView>{*this, 0}; }
  auto end() const { return SubsetViewIterator<const ConstSubsetView>{*this, this->size()}; }
};

template<std::size_t N>
inline std::array<std::size_t, N> make_strides_from_vec(const std::vector<std::size_t>& dim) {
  ass<"Dimension mismatch in make strides">(dim.size() == N);
  std::array<std::size_t, N> stride{};
  stride[0] = 1;
  for (std::size_t k = 1; k < N; k++) stride[k] = stride[k-1] * dim[k-1];
  return stride;
}
inline std::vector<std::size_t> make_strides_dyn(const std::vector<std::size_t>& dim) {
  std::vector<std::size_t> stride(dim.size(), 0);
  stride[0] = 1;
  for (std::size_t k = 1; k < stride.size(); k++) stride[k] = stride[k-1] * dim[k-1];
  return stride;
}

template<std::size_t N,typename T, typename O>
inline void fill_scalars_in_index_lists(
                             const T& arr, std::array<Buffer<Integer>, N>& converted_double_arrays,
                             std::array<Buffer<Integer>, N>& converted_bool_arrays,
                             std::array<const Buffer<Integer>*, N>& index_lists, O&& arg,
                             std::size_t& counter, std::size_t& counter_converted_double, std::size_t& counter_converted_bool) {
  using A = std::decay_t<decltype(arg)>;
  if constexpr (IsCppDouble<A>) {
    auto& v = converted_double_arrays[counter_converted_double++];
    v.push_back(safe_index_from_double(arg));
    index_lists[counter++] = &v;
  } else if constexpr(IsCppLogical<A> && IsCppInteger<A>) {
    auto& v = converted_bool_arrays[counter_converted_bool++];
    if (arg) {
      const std::size_t len = arr.dim[counter];
      v.resize(len);
      for (std::size_t i = 0; i < len; ++i) {
        v.set(i, static_cast<int>(i) + 1);
      }
    } else {
      ass<"Bool subsetting is only with TRUE possible">(false);
    }
    index_lists[counter++] = &v;
  } else if constexpr(!IsCppLogical<A> && IsCppInteger<A>) {
    auto& v = converted_double_arrays[counter_converted_double++];
    v.push_back(arg);
    index_lists[counter++] = &v;
  }
}

template<typename ValType, std::size_t N, typename T, typename... Args>
inline void fill_index_lists(const T& arr, std::array<Buffer<Integer>, N>& converted_double_arrays,
                             std::array<Buffer<Integer>, N>& converted_bool_arrays,
                             std::array<const Buffer<Integer>*, N>& index_lists, Args&&... args) {
  std::size_t counter = 0;
  std::size_t counter_converted_double = 0;
  std::size_t counter_converted_bool = 0;
  forEachArg(
    [&](const auto& arg) {
      using A = std::decay_t<decltype(arg)>;
      // --- Case 1: Array<Integer>
      if constexpr (IsArray<A> && IsInteger<ValType>) {
        index_lists[counter++] = &arg;
      }
      // --- Case 2: Array<Double> || Array<Dual>
      else if constexpr (IsArray<A> && (IsDouble<ValType> || IsDual<ValType>)) {
        auto& v = converted_double_arrays[counter_converted_double++];
        v.resize(arg.size());
        for (std::size_t i = 0; i < arg.size(); i++) {
          v.set(i, safe_index_from_double(arg.get(i).val));
        }
        index_lists[counter++] = &v;
      }
      // --- Case 3: Array<Logical>
      else if constexpr (IsArray<A> && IsLogical<ValType>) {
        auto& v = converted_bool_arrays[counter_converted_bool++];
        for (std::size_t b = 0; b < arg.size(); b++) {
          if (arg.get(b)) v.push_back(b + 1);
        }
        index_lists[counter++] = &v;
      }
      // --- Case 4: C++ scalars
      else if constexpr (IsCppArithV<A>) {
        fill_scalars_in_index_lists<N>(arr, converted_double_arrays, converted_bool_arrays,
                                    index_lists, arg,
                                    counter, counter_converted_double, counter_converted_bool);
      }
      // --- Case 5: Scalars
      else if constexpr (IsArithV<A>) {
        fill_scalars_in_index_lists<N>(arr, converted_double_arrays, converted_bool_arrays,
                                    index_lists, arg.val,
                                    counter, counter_converted_double, counter_converted_bool);
      }
      else {
        static_assert(!sizeof(A*), "Unsupported index type");
      }
    },
    args...
  );
}

// Create mutable subset
template <typename ArrayType, typename... Args>
inline auto subset(ArrayType& arr, const Args&... args) {

  using E = typename ExtractDataType<ArrayType>::value_type;
  constexpr std::size_t N = sizeof...(Args);
  if (N > arr.dim.size()) {
    ass<"Too many index arguments for array rank">(false);
  }
  if (N < arr.dim.size()) {
    ass<"Too less index arguments for array rank">(false);
  }

  std::array<Buffer<Integer>, N> converted_double_arrays;
  std::array<Buffer<Integer>, N> converted_bool_arrays;
  std::array<const Buffer<Integer>*, N> index_lists{};

  fill_index_lists<E, N>(
    arr,
    converted_double_arrays,
    converted_bool_arrays,
    index_lists,
    args...
  );

  std::vector<std::size_t> L(N, 0);
  for (std::size_t k = 0; k < N; k++) {
    if (!index_lists[k] || (index_lists[k]->size() == 0)) {
      ass<"Empty index for at least one dimension">(false);
    }
    L[k] = index_lists[k]->size();
  }

  auto stride = make_strides_from_vec<N>(arr.dim);

  std::size_t S = 1;
  for (std::size_t k = 0; k < N; k++) S *= L[k];

  Buffer<int> out(S);

  std::array<std::size_t, N> pos{};

  std::size_t offset = 0;
  std::size_t k = 0;
  std::size_t counter = 0;
  for (;;) {
    offset = 1;
    for (std::size_t k = 0; k < N; k++) {
      offset += ((*index_lists[k]).get(pos[k]).val - 1) * stride[k];
    }
    out.set(counter++, offset - 1);

    k = 0;
    for (;;) {
      pos[k] += 1;
      if (pos[k] < L[k]) break;
      pos[k] = 0;
      k++;
      if (k == N) {
        return Array<E, SubsetView<ArrayType, N, SubsetViewTrait>>(
          SubsetView<ArrayType, N, SubsetViewTrait>{arr, std::move(out)},
          std::move(L)
        );
      }
    }
  }
}

// Create constant subset
// ------------------------------------------------------------------
template <typename ArrayType, typename... Args>
inline auto subset(ArrayType&& arr, const Args&... args) {

  using E = typename ExtractDataType<ArrayType>::value_type;
  constexpr std::size_t N = sizeof...(Args);
  if (N > arr.dim.size()) {
    ass<"Too many index arguments for array rank">(false);
  }
  if (N < arr.dim.size()) {
    ass<"Too less index arguments for array rank">(false);
  }

  std::array<Buffer<Integer>, N> converted_double_arrays;
  std::array<Buffer<Integer>, N> converted_bool_arrays;
  std::array<const Buffer<Integer>*, N> index_lists{};

  fill_index_lists<E, N>(
    arr,
    converted_double_arrays,
    converted_bool_arrays,
    index_lists,
    args...
  );

  std::vector<std::size_t> L(N, 0);
  for (std::size_t k = 0; k < N; k++) {
    if (!index_lists[k] || (index_lists[k]->size() == 0)) {
      ass<"Empty index for at least one dimension">(false);
    }
    L[k] = index_lists[k]->size();
  }

  auto stride = make_strides_from_vec<N>(arr.dim);

  std::size_t S = 1;
  for (std::size_t k = 0; k < N; k++) S *= L[k];

  Buffer<int> out(S);

  std::array<std::size_t, N> pos{};

  std::size_t offset = 0;
  std::size_t k = 0;
  std::size_t counter = 0;
  for (;;) {
    offset = 1;
    for (std::size_t k = 0; k < N; k++) {
      offset += ((*index_lists[k]).get(pos[k]).val - 1) * stride[k];
    }
    out.set(counter++, offset - 1);

    k = 0;
    for (;;) {
      pos[k] += 1;
      if (pos[k] < L[k]) break;
      pos[k] = 0;
      k++;
      if (k == N) {
        return Array<E, ConstSubsetView<ArrayType, N, ConstSubsetViewTrait>>(
          ConstSubsetView<ArrayType, N, ConstSubsetViewTrait>{std::move(arr), std::move(out)},
          std::move(L)
        );
      }
    }
  }
}

} // namespace etr

#endif
