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
    obj(std::move(obj_)), indices(std::move(indices_)) {}

  // indices.get(i) stays checked: i comes from whatever loop drives this
  // view, not from create_indices. The offset it returns is unchecked
  // against obj: create_indices already proved it's in bounds (same
  // reasoning as R never re-checking the assembled offset it built from
  // validated subscripts, see PreservingSubsetting.hpp's create_indices).
  auto get(std::size_t i) const {
    return obj.get().get_unchecked(indices.get(i));
  }
  template<typename Val>
  void set(std::size_t i, const Val& v) const {
    if constexpr (IS<Decayed<Val>, value_type>) {
      obj.get().set_unchecked(indices.get(i), v);
    } else {
      obj.get().set_unchecked(indices.get(i), static_cast<value_type>(v));
    }
  }
  // For when THIS SubsetView is itself nested inside another view/expression
  // (e.g. subset-of-subset) -- the outer caller already proved i is in
  // bounds, so skip indices' own check too, not just obj's.
  auto get_unchecked(std::size_t i) const {
    return obj.get().get_unchecked(indices.get_unchecked(i));
  }
  template<typename Val>
  void set_unchecked(std::size_t i, const Val& v) const {
    if constexpr (IS<Decayed<Val>, value_type>) {
      obj.get().set_unchecked(indices.get_unchecked(i), v);
    } else {
      obj.get().set_unchecked(indices.get_unchecked(i), static_cast<value_type>(v));
    }
  }
  std::size_t size() const {return indices.size();}

  std::size_t translate(std::size_t i) const {
    return static_cast<std::size_t>(indices.get(i));
  }

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

  ConstSubsetView(O& obj_, Buffer<int>&& indices_) : obj(obj_), indices(std::move(indices_)) {}
  ConstSubsetView(O&& obj_, Buffer<int>&& indices_) : obj(std::move(obj_)), indices(std::move(indices_)) {}

  auto get(std::size_t i) const {
    return obj.get().get_unchecked(indices.get().get(i));
  }
  auto get_unchecked(std::size_t i) const {
    return obj.get().get_unchecked(indices.get().get_unchecked(i));
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
  auto get_unchecked(std::size_t i) const { return obj.get().get_unchecked(index); }
  template<typename Val> void set_unchecked(std::size_t i, const Val& v) const {
    if constexpr (IS<Decayed<Val>, value_type>) {
      obj.get().set_unchecked(index, v);
    } else {
      obj.get().set_unchecked(index, static_cast<value_type>(v));
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

// Returns `dim` unchanged (by reference, no copy) unless a single index is
// being used to linearly address a multi-dim array, which needs a
// synthetic 1-element "total size" dim -- `storage` backs that one rare
// case so the common case (nargs == dim.size(), true for plain vector and
// matrix subsetting) allocates nothing.
inline const std::vector<std::size_t>& linear_dim_if_single_index(
    const std::vector<std::size_t>& dim, std::size_t nargs,
    std::vector<std::size_t>& storage) {
  if (nargs == 1 && dim.size() > 1) {
    std::size_t total = 1;
    for (std::size_t d : dim) total *= d;
    storage.assign(1, total);
    return storage;
  }
  return dim;
}

template<std::size_t N, typename O>
inline void fill_scalars_in_index_lists(
  const std::vector<std::size_t>& dim,
  std::array<Buffer<Integer>, N>& converted_arrays,
  std::array<const Buffer<Integer>*, N>& index_lists, O&& arg,
  std::size_t& counter, std::size_t& counter_converted) {
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

template<std::size_t N, typename... Args>
inline void fill_index_lists(const std::vector<std::size_t>& dim,
                             std::array<Buffer<Integer>, N>& converted_arrays,
                             std::array<const Buffer<Integer>*, N>& index_lists,
                             std::array<bool, N>& is_plain_int_array, Args&&... args) {
  std::size_t counter = 0;
  std::size_t counter_converted = 0;
  forEachArg(
    [&](const auto& arg) {
      using A = std::decay_t<decltype(arg)>;
      if constexpr (IsArray<A>) {
        ass<"Too many index arguments for at least one dimension">(dim[counter] >= arg.size());

        using arg_val_type = typename ExtractDataType<A>::value_type;
        // --- Case 1.1: Array<Integer> (L value)
        // arg.d is only ever read here, into a freshly-allocated `out` that
        // outlives this call -- no aliasing hazard, so reference it directly
        // regardless of length, no copy needed, no per-element check here
        // either: a separate NA pass over it would be a genuinely new O(L[k])
        // loop (measured ~20% slower on the hot vector-assign case), unlike
        // every other branch below where the check just rides along with a
        // conversion loop that has to happen anyway. is_plain_int_array[k]
        // lets create_indices' own (already mandatory) validation pass pick
        // the "integer object"-specific message for this axis instead,
        // without a second pass.
        if constexpr (IsArray<A> && IsLBufferArray<A> && IsInteger<arg_val_type>) {
          is_plain_int_array[counter] = true;
          index_lists[counter++] = &arg.d;
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
        // --- Case 3: Array<IsReverseDouble>
        else if constexpr (IsArray<A> && IsReverseDouble<arg_val_type>) {
          const std::size_t n = arg.size();
          auto& v = converted_arrays[counter_converted++];
          v.resize(n);
          for (std::size_t i = 0; i < n; i++) {
            const auto d_val = get_scalar_val(arg.get(i));
            ass<"Found NA value in subsetting (within a double object)">(!d_val.isNA());
            v.set(i, safe_index_from_double(d_val.get_val_from_tape()));
          }
          index_lists[counter++] = &v;
        }
        // --- Case 4: Array except LBuffer Integer or LBuffer Logical
        else if constexpr (IsArray<A>) {
          // TODO: shouldn't that be splitted up based on the base type?
          // because at least the error is misleading if the object
          // is not holding doubles
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
        fill_scalars_in_index_lists<N>(dim, converted_arrays,
                                       index_lists, arg,
                                       counter, counter_converted);
      }
      // --- Case 5: Scalars
      else if constexpr (IsScalarLike<A>) {
        // get_val() drops the is_na flag, and an NA Integer carries val == 0,
        // which would otherwise trip the zero-index guard downstream instead
        // of reporting the NA. (NA Double/Dual carry NaN and are caught by
        // safe_index_from_double as "invalid index argument".)
        using V = std::decay_t<decltype(get_val(arg))>;
        if constexpr (std::is_integral_v<V> && !std::is_same_v<V, bool>) {
          ass<"Found NA value in subsetting (within an integer object)">(!arg.isNA());
        }
        fill_scalars_in_index_lists<N>(dim, converted_arrays,
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
  std::vector<std::size_t> dim_storage;
  const std::vector<std::size_t>& dim = linear_dim_if_single_index(dim_view(arr.get_dim()), N, dim_storage);
  if (N > dim.size()) {
    ass<"Too many index arguments for array rank">(false);
  }
  if (N < dim.size()) {
    ass<"Too less index arguments for array rank">(false);
  }

  std::array<Buffer<Integer>, N> converted_arrays;
  std::array<const Buffer<Integer>*, N> index_lists{};
  std::array<bool, N> is_plain_int_array{};

  fill_index_lists<N>(
    dim,
    converted_arrays,
    index_lists,
    is_plain_int_array,
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

  // Validate each subscript ONCE, over its own L[k] values, instead of
  // re-validating on every one of the S output elements. The hot loop
  // below then reads raw values and needs no bounds/NA checks at all --
  // every value it can see has already been proven valid here.
  // Mirrors R's own range-check pass in ArraySubset(), src/main/subset.c
  // (GNU R source, see ~/Documents/r-source). The is_plain_int_array
  // branch (once per axis, not per element) picks a message specific to
  // "integer object" for that case instead of paying for a second pass
  // over it in fill_index_lists just to get a better message (measured:
  // that costs ~20% on the hot vector-assign case).
  std::array<const int*, N> vals{};
  for (std::size_t k = 0; k < N; k++) {
    // Upper bound too, not just NA/negative: get_unchecked/set_unchecked
    // below trust this pass completely, so it's the only place left that
    // can catch an index past dim[k].
    if (is_plain_int_array[k]) {
      for (std::size_t j = 0; j < L[k]; j++) {
        const auto val = (*index_lists[k]).get(j);
        ass<"Found NA value in subsetting (within an integer object)">(!val.isNA());
        ass<"Zero and negative indices are not supported">(val.val >= 1);
        ass<"Error: out of boundaries">(static_cast<std::size_t>(val.val) <= dim[k]);
      }
    } else {
      for (std::size_t j = 0; j < L[k]; j++) {
        const auto val = (*index_lists[k]).get(j);
        ass<"Found NA value in subsetting">(!val.isNA());
        ass<"Zero and negative indices are not supported">(val.val >= 1);
        ass<"Error: out of boundaries">(static_cast<std::size_t>(val.val) <= dim[k]);
      }
    }
    vals[k] = index_lists[k]->data();
  }

  // Matrix fast path: mirrors R's MatrixSubset() (src/main/subset.c) --
  // the stride[1]-scaled term only changes once per outer (L[1]) step
  // instead of being recomputed on every one of the S = L[0]*L[1]
  // elements, like the general N-dim loop below still does.
  if constexpr (N == 2) {
    const int* v0 = vals[0];
    const int* v1 = vals[1];
    std::size_t counter = 0;
    for (std::size_t j = 0; j < L[1]; j++) {
      const std::size_t col_offset = 1 + (static_cast<std::size_t>(v1[j]) - 1) * stride[1];
      for (std::size_t i = 0; i < L[0]; i++) {
        const std::size_t offset = col_offset + (static_cast<std::size_t>(v0[i]) - 1) * stride[0];
        out.set_unchecked(counter++, offset - 1);
      }
    }
    return out_L{std::move(out), std::move(L)};
  } else {
    std::array<std::size_t, N> pos{};

    std::size_t offset = 0;
    std::size_t k = 0;
    std::size_t counter = 0;
    for (;;) {
      offset = 1;
      for (std::size_t k = 0; k < N; k++) {
        offset += (static_cast<std::size_t>(vals[k][pos[k]]) - 1) * stride[k];
      }
      out.set_unchecked(counter++, offset - 1);

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
}

// Fused assignment: `arr[args...] <- rhs`. Unlike `subset(arr, args...) =
// rhs` (which builds a SubsetView backed by a materialized `out` offset
// buffer, then a separate pass copies through it), this computes each
// offset and writes straight into `arr` in the SAME pass -- no `out` buffer
// at all, mirroring R's fused subassign.c.
//
// `rhs` comes right after `arr` (not last, alongside args...) because a
// template parameter pack can only be deduced when nothing follows it in
// the function's parameter list.
//
// `rhs` is still fully materialized into `temp` first, matching
// Array<T,SubsetView<...>>::assign()'s copy_with_temp: until alias
// detection is added at the codegen level (planned separately, checking
// whether `arr` occurs in `rhs` or in `args...`), this stays required for
// correctness -- `x[idx] <- x[idx] + 1` must read the unmodified `x`
// throughout, even when `idx` has duplicates.
template <typename ArrayType, typename RHS, typename... Args>
inline void subset_assign(ArrayType& arr, const RHS& rhs, const Args&... args) {
  using E = typename ExtractDataType<ArrayType>::value_type;
  constexpr std::size_t N = sizeof...(Args);

  std::vector<std::size_t> dim_storage;
  const std::vector<std::size_t>& dim = linear_dim_if_single_index(dim_view(arr.get_dim()), N, dim_storage);
  if (N > dim.size()) {
    ass<"Too many index arguments for array rank">(false);
  }
  if (N < dim.size()) {
    ass<"Too less index arguments for array rank">(false);
  }

  std::array<Buffer<Integer>, N> converted_arrays;
  std::array<const Buffer<Integer>*, N> index_lists{};
  std::array<bool, N> is_plain_int_array{};
  fill_index_lists<N>(dim, converted_arrays, index_lists, is_plain_int_array, args...);

  std::array<std::size_t, N> L{};
  for (std::size_t k = 0; k < N; k++) {
    if (!index_lists[k] || (index_lists[k]->size() == 0)) {
      ass<"Empty index for at least one dimension">(false);
    }
    L[k] = index_lists[k]->size();
  }

  auto stride = make_strides_from_vec<N>(dim);

  std::size_t S = 1;
  for (std::size_t k = 0; k < N; k++) S *= L[k];

  // Validate each subscript ONCE, same as create_indices -- see its comment
  // for why the write loop below then needs no bounds/NA checks, and for
  // why is_plain_int_array picks the message per-axis instead of per-element.
  std::array<const int*, N> vals{};
  for (std::size_t k = 0; k < N; k++) {
    if (is_plain_int_array[k]) {
      for (std::size_t j = 0; j < L[k]; j++) {
        const auto val = (*index_lists[k]).get(j);
        ass<"Found NA value in subsetting (within an integer object)">(!val.isNA());
        ass<"Zero and negative indices are not supported">(val.val >= 1);
        ass<"Error: out of boundaries">(static_cast<std::size_t>(val.val) <= dim[k]);
      }
    } else {
      for (std::size_t j = 0; j < L[k]; j++) {
        const auto val = (*index_lists[k]).get(j);
        ass<"Found NA value in subsetting">(!val.isNA());
        ass<"Zero and negative indices are not supported">(val.val >= 1);
        ass<"Error: out of boundaries">(static_cast<std::size_t>(val.val) <= dim[k]);
      }
    }
    vals[k] = index_lists[k]->data();
  }

  Buffer<E> temp(S);
  // Scalar RHS (e.g. `x[idx] <- 0.0`) broadcasts one value into every
  // selected element -- mirrors Array<T,SubsetView<...>>::operator=(scalar)
  // (ArrayClass.hpp), which has no `.size()`/`.get(i)` to call at all.
  if constexpr (IsScalarLike<RHS>) {
    if constexpr (IS<RHS, E>) {
      for (std::size_t i = 0; i < S; i++) temp.set(i, rhs);
    } else {
      const E val = static_cast<E>(rhs);
      for (std::size_t i = 0; i < S; i++) temp.set(i, val);
    }
  } else {
    ass<"number of items to replace is not a multiple of replacement length">(rhs.size() == S);
    using RhsValueType = typename ReRef<RHS>::type::value_type;
    for (std::size_t i = 0; i < S; i++) {
      if constexpr (IS<RhsValueType, E>) {
        temp.set(i, rhs.get(i));
      } else {
        temp.set(i, cast_preserve_na<E>(rhs.get(i)));
      }
    }
  }

  if constexpr (N == 2) {
    const int* v0 = vals[0];
    const int* v1 = vals[1];
    std::size_t counter = 0;
    for (std::size_t j = 0; j < L[1]; j++) {
      const std::size_t col_offset = 1 + (static_cast<std::size_t>(v1[j]) - 1) * stride[1];
      for (std::size_t i = 0; i < L[0]; i++) {
        const std::size_t offset = col_offset + (static_cast<std::size_t>(v0[i]) - 1) * stride[0];
        arr.d.set_unchecked(offset - 1, temp.get_unchecked(counter++));
      }
    }
  } else {
    std::array<std::size_t, N> pos{};
    std::size_t counter = 0;
    for (;;) {
      std::size_t offset = 1;
      for (std::size_t k = 0; k < N; k++) {
        offset += (static_cast<std::size_t>(vals[k][pos[k]]) - 1) * stride[k];
      }
      arr.d.set_unchecked(offset - 1, temp.get_unchecked(counter++));

      std::size_t k = 0;
      for (;;) {
        pos[k] += 1;
        if (pos[k] < L[k]) break;
        pos[k] = 0;
        k++;
        if (k == N) {
          return;
        }
      }
    }
  }
}

// Fast path: `arr[scalars...] <- rhs`. Mirrors subset()'s AllScalarIndices
// overload below -- skips subset_assign's index-list/temp-buffer machinery
// entirely for a single at()-Ref write. ReverseDouble stays on the general
// path for the same reason subset() excludes it: its write must rebind the
// tape id via Buffer::set, which at()'s by-value return can't do.
template <typename ArrayType, typename RHS, typename... Args>
requires AllScalarIndices<Args...> &&
         (!IsReverseDouble<typename ExtractDataType<ArrayType>::value_type>)
inline void subset_assign(ArrayType& arr, const RHS& rhs, const Args&... args) {
  at(arr, args...) = rhs;
}

// Create mutable subset
template <typename ArrayType, typename... Args>
inline auto subset(ArrayType& arr, const Args&... args) {
  using E = typename ExtractDataType<ArrayType>::value_type;
  using DTYPE = Decayed<decltype(arr.d)>;
  constexpr std::size_t N = sizeof...(Args);

  auto ol = create_indices(arr, args...);
  return Array<E, SubsetView<DTYPE, N, SubsetViewTrait>>(
    SubsetView<DTYPE, N, SubsetViewTrait>{arr.d, std::move(ol.out)},
    std::move(ol.L)
  );
}

// Fast path: subset(array, Scalars...)
// ReverseDouble is excluded: its element write must rebind the tape id via
// Buffer::set, so it routes through the SubsetView overload above instead of
// the by-value at() handle.
template <typename ArrayType, typename... Args>
requires AllScalarIndices<Args...> &&
         (!IsReverseDouble<typename ExtractDataType<ArrayType>::value_type>)
inline decltype(auto) subset(ArrayType& arr, const Args&... args) {
  return at(arr, args...);
}

template <typename ArrayType, typename... Args> requires AllScalarIndices<Args...>
inline decltype(auto) subset(ArrayType&& arr, const Args&... args) {
  return at(std::forward<ArrayType>(arr), args...);
}

// Create subset of subset
// ------------------------------------------------------------------
template <typename ArrayType, typename... Args>
requires IsSubsetArray<ArrayType> && HasNonScalarIndex<Args...>
inline auto subset(ArrayType&& arr, const Args&... args) {
  using E = typename ExtractDataType<ArrayType>::value_type;
  using DTYPE = Decayed<decltype(arr.d)>;
  constexpr std::size_t N = sizeof...(Args);

  auto ol = create_indices(arr, args...);
  return Array<E, SubsetView<DTYPE, N, SubsetViewTrait>>(
    SubsetView<DTYPE, N, SubsetViewTrait>{std::move(arr.d), std::move(ol.out)},
    std::move(ol.L)
  );
}

// Create constant subset -- materializes into a flat, owned buffer instead
// of a ConstSubsetView, matching R's own `[` (always a copy, never aliases
// the source). This also sidesteps what ConstSubsetView did here: `arr` is
// a const reference (that's why this overload, not the mutable one above,
// got picked), so `std::move(arr.d)` couldn't actually move -- it silently
// copy-constructed a whole extra `arr.d`-sized Buffer just to wrap it in a
// view over S elements. Gathering only the S selected elements directly is
// strictly less copying, and leaves later reads as flat/contiguous instead
// of index-indirected, which is what actually lets them vectorize.
// ------------------------------------------------------------------
template <typename ArrayType, typename... Args> requires (!IsSubsetArray<std::remove_reference_t<ArrayType>>) && HasNonScalarIndex<Args...>
inline auto subset(ArrayType&& arr, const Args&... args) {
  using E = typename ExtractDataType<ArrayType>::value_type;

  auto ol = create_indices(arr, args...);
  const std::size_t S = ol.out.size();
  Array<E, Buffer<E, RBufferTrait>> res(SI{S});
  res.dim = std::move(ol.L);
  for (std::size_t i = 0; i < S; i++) {
    res.set(i, arr.d.get_unchecked(ol.out.get_unchecked(i)));
  }
  return res;
}

} // namespace etr

#endif
