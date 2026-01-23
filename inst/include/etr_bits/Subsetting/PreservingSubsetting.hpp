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
  Buffer<int> indices;

  SubsetView(O& obj_, Buffer<int>&& indices_) :
    obj(obj_), indices(std::move(indices_)) {}
  SubsetView(O&& obj_, Buffer<int>&& indices_) :
    obj(obj_), indices(std::move(indices_)) {}

  auto get(std::size_t i) const {
    return obj.get().get(indices.get(i));
  }
  template<typename Val>
  void set(std::size_t i, const Val& v) const {
    if constexpr (IS<Decayed<Val>, value_type>) {
      obj.get().set(indices.get(i), v);
    } else {
      obj.get().set(indices.get(i), static_cast<value_type>(v));
    }
  }
  std::size_t size() const {return indices.size();}

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
    return obj.get().get(indices.get().get(i));
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

template <typename O, typename Trait> struct SubsetWithScalarView {
  using TypeTrait = Trait;
  using value_type = typename ReRef<O>::type::value_type;
  Holder<O> obj;
  int index;

  SubsetWithScalarView(O& obj_, int index_) : obj(obj_), index(index_) {}
  SubsetWithScalarView(O&& obj_, int index_) : obj(obj_), index(index_) {}

  auto get(std::size_t i) const { return obj.get().get(index); }
  template<typename Val> void set(std::size_t i, const Val& v) const {
    if constexpr (IS<Decayed<Val>, value_type>) {
      obj.get().set(index, v);
    } else {
      obj.get().set(index, static_cast<value_type>(v));
    }
  }
  std::size_t size() const {return 1; }

  // Copy constructor
  SubsetWithScalarView(const SubsetWithScalarView& other) : obj(other.obj), index(other.index) {}
  // Copy assignment
  SubsetWithScalarView& operator=(const SubsetWithScalarView& other) {
    obj = other.obj;
    index = other.index;
    return *this;
  };
  // Move constructor
  SubsetWithScalarView(SubsetWithScalarView&& other) : obj(std::move(other.obj)), index(other.index) {}
  // Move assignment
  SubsetWithScalarView& operator=(SubsetWithScalarView&& other) {
    obj = std::move(other.obj);
    index = other.index;
    return *this;
  }

  auto begin() const { return SubsetViewIterator<const SubsetWithScalarView>{*this, 0}; }
  auto end() const { return SubsetViewIterator<const SubsetWithScalarView>{*this, this->size()}; }
};
template <typename O, typename Trait> struct ConstSubsetWithScalarView {
  using TypeTrait = Trait;
  using value_type = typename ReRef<O>::type::value_type;
  ConstHolder<O> obj;
  int index;

  ConstSubsetWithScalarView(O& obj_, int index_) : obj(obj_), index(index_) {}
  ConstSubsetWithScalarView(O&& obj_, int index_) : obj(obj_), index(index_) {}

  auto get(std::size_t i) const { return obj.get().get(index); }
  std::size_t size() const {return 1; }

  // Copy constructor
  ConstSubsetWithScalarView(const ConstSubsetWithScalarView& other) : obj(other.obj), index(other.index) {}
  // Copy assignment
  ConstSubsetWithScalarView& operator=(const ConstSubsetWithScalarView& other) {
    obj = other.obj;
    index = other.index;
    return *this;
  };
  // Move constructor
  ConstSubsetWithScalarView(ConstSubsetWithScalarView&& other) : obj(std::move(other.obj)), index(other.index) {}
  // Move assignment
  ConstSubsetWithScalarView& operator=(ConstSubsetWithScalarView&& other) {
    obj = std::move(other.obj);
    index = other.index;
    return *this;
  }

  auto begin() const { return SubsetViewIterator<const ConstSubsetWithScalarView>{*this, 0}; }
  auto end() const { return SubsetViewIterator<const ConstSubsetWithScalarView>{*this, this->size()}; }
};

// -----------------------------------------------------------------------------------------------------------
template<std::size_t N>
inline std::array<std::size_t, N> make_strides_from_vec(const std::vector<std::size_t>& dim) {
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
                             const T& arr, std::array<Buffer<Integer>, N>& converted_arrays,
                             std::array<const Buffer<Integer>*, N>& index_lists, O&& arg,
                             std::size_t& counter, std::size_t& counter_converted) {
  const auto& dim = dim_view(arr.get_dim());
  using A = std::decay_t<decltype(arg)>;
  if constexpr (IsCppDouble<A>) {
    auto& v = converted_arrays[counter_converted++];
    v.push_back(safe_index_from_double(arg));
    index_lists[counter++] = &v;
  } else if constexpr(IsCppLogical<A>) {
    auto& v = converted_arrays[counter_converted++];
    if (arg) {
      const std::size_t len = dim[counter];
      v.resize(len);
      for (std::size_t i = 0; i < len; ++i) {
        v.set(i, static_cast<int>(i) + 1);
      }
    } else {
      ass<"Bool subsetting is only with TRUE possible">(false);
    }
    index_lists[counter++] = &v;
  } else if constexpr(IsCppInteger<A>) {
    auto& v = converted_arrays[counter_converted++];
    v.push_back(arg);
    index_lists[counter++] = &v;
  }
}

template<std::size_t N, typename T, typename... Args>
inline void fill_index_lists(const T& arr, std::array<Buffer<Integer>, N>& converted_arrays,
                             std::array<const Buffer<Integer>*, N>& index_lists, Args&&... args) {
  std::size_t counter = 0;
  std::size_t counter_converted = 0;
  const auto& dim = dim_view(arr.get_dim());
  forEachArg(
    [&](const auto& arg) {
      using A = std::decay_t<decltype(arg)>;
      if constexpr (IsArray<A>) {
        ass<"Too many index arguments for at least one dimension">(dim[counter] >= arg.size());

        using arg_val_type = typename ExtractDataType<A>::value_type;
        // --- Case 1.1: Array<Integer> (L value)
        if constexpr (IsArray<A> && IsLBufferArray<A> && IsInteger<arg_val_type>) {
          const std::size_t n = arg.size();
          if (dim[counter] == arg.size()) {
            index_lists[counter++] = &arg.d;
          } else {
            auto& v = converted_arrays[counter_converted++];
            v.resize(n);
            for (std::size_t i = 0; i < n; i++) {
              const auto i_val = get_scalar_val(arg.get(i));
              ass<"Found NA value in subsetting (within an integer object)">(!i_val.isNA());
              v.set(i, i_val.val);
            }
            index_lists[counter++] = &v;
          }
        }
        // --- Case 2: Array<Logical>
        else if constexpr (IsArray<A> && IsLogical<arg_val_type>) {
          const std::size_t n = dim[counter];
          auto& v = converted_arrays[counter_converted++];
          for (std::size_t b = 0; b < n; b++) {
            const auto b_val = get_scalar_val(arg.get(safe_modulo(b, arg.size())));
            ass<"Found NA value in subsetting (within a logical object)">(!b_val.isNA());
            if (b_val.val) {
              v.push_back(b + 1);
            }
          }
          index_lists[counter++] = &v;
        }
        // --- Case 3: Array except LBuffer Integer or LBuffer Logical
        else if constexpr (IsArray<A>) {
          const std::size_t n = arg.size();
          auto& v = converted_arrays[counter_converted++];
          v.resize(n);
          for (std::size_t i = 0; i < n; i++) {
            const auto d_val = get_scalar_val(arg.get(i));
            ass<"Found NA value in subsetting (within a double object)">(!d_val.isNA());
            v.set(i, safe_index_from_double(d_val.val));
          }
          index_lists[counter++] = &v;
        }
      }
      // --- Case 4: C++ scalars
      else if constexpr (IsCppArithV<A>) {
        fill_scalars_in_index_lists<N>(arr, converted_arrays,
                                       index_lists, arg,
                                       counter, counter_converted);
      }
      // --- Case 5: Scalars
      else if constexpr (IsScalarLike<A>) {
        fill_scalars_in_index_lists<N>(arr, converted_arrays,
                                       index_lists, get_val(arg),
                                       counter, counter_converted);
      }
      else {
        static_assert(!sizeof(A*), "Unsupported index type");
      }
    },
    args...
  );
}

struct out_L {
  Buffer<int> out;
  std::vector<std::size_t> L;
};

template <typename ArrayType, typename... Args>
inline out_L create_indices(const ArrayType& arr, const Args&... args) {
  constexpr std::size_t N = sizeof...(Args);
  const auto& dim = dim_view(arr.get_dim());
  if (N > dim.size()) {
    ass<"Too many index arguments for array rank">(false);
  }
  if (N < dim.size()) {
    ass<"Too less index arguments for array rank">(false);
  }

  std::array<Buffer<Integer>, N> converted_arrays;
  std::array<const Buffer<Integer>*, N> index_lists{};

  fill_index_lists<N>(
    arr,
    converted_arrays,
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

  auto stride = make_strides_from_vec<N>(dim);

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
      const auto val = (*index_lists[k]).get(pos[k]);
      ass<"Found NA value in subsetting">(!val.isNA());
      offset += (val.val - 1) * stride[k];
    }
    out.set(counter++, offset - 1);

    k = 0;
    for (;;) {
      pos[k] += 1;
      if (pos[k] < L[k]) break;
      pos[k] = 0;
      k++;
      if (k == N) {
        return out_L{std::move(out), std::move(L)};
      }
    }
  }
}

// Create mutable subset
template <typename ArrayType, typename... Args>
inline auto subset(ArrayType& arr, const Args&... args) {
  using E = typename ExtractDataType<ArrayType>::value_type;
  constexpr std::size_t N = sizeof...(Args);

  auto ol = create_indices(arr, args...);
  return Array<E, SubsetView<ArrayType, N, SubsetViewTrait>>(
    SubsetView<ArrayType, N, SubsetViewTrait>{arr, std::move(ol.out)},
    std::move(ol.L)
  );
}

// Fast path: subset(vec, Scalar)
template <typename ArrayType, typename I> requires(IsScalarLike<I> && !IsLogical<I> && !IsLogicalRef<I>)
inline decltype(auto) subset(ArrayType& arr, const I& idx) {
  return at(arr, idx);
}
template <typename ArrayType, typename I> requires(IsScalarLike<I> && !IsLogical<I> && !IsLogicalRef<I>)
inline const auto subset(const ArrayType& arr, const I& idx) {
  return at(arr, idx);
}

// Create subset of subset
// ------------------------------------------------------------------
template <typename ArrayType, typename... Args>
requires IsSubsetArray<ArrayType>
inline auto subset(ArrayType&& arr, const Args&... args) {
  using E = typename ExtractDataType<ArrayType>::value_type;
  constexpr std::size_t N = sizeof...(Args);

  auto ol = create_indices(arr, args...);
  return Array<E, SubsetView<ArrayType, N, SubsetViewTrait>>(
    SubsetView<ArrayType, N, SubsetViewTrait>{arr, std::move(ol.out)},
    std::move(ol.L)
  );
}

// Create constant subset
// ------------------------------------------------------------------
template <typename ArrayType, typename... Args>
requires (!IsSubsetArray<ArrayType>)
inline auto subset(ArrayType&& arr, const Args&... args) {
  using E = typename ExtractDataType<ArrayType>::value_type;
  constexpr std::size_t N = sizeof...(Args);

  auto ol = create_indices(arr, args...);
  return Array<E, ConstSubsetView<ArrayType, N, ConstSubsetViewTrait>>(
    ConstSubsetView<ArrayType, N, ConstSubsetViewTrait>{std::move(arr), std::move(ol.out)},
    std::move(ol.L)
  );
}

} // namespace etr

#endif
