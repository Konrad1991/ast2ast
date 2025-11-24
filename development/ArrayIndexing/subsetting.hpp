template<typename Arg>
requires std::is_same_v<std::decay_t<Arg>, std::vector<std::size_t>>
inline auto subset_mixed(Array& arr, Arg&& arg) {
  std::array<std::size_t, 1> L1D{ arg.size() };
  if constexpr (std::is_lvalue_reference_v<Arg>) {
    return SubsetView<Array, 1, OneBased>(arr, arg, L1D);
  } else {
    auto& v = arg;
    return SubsetView<Array, 1, OneBased>(arr, std::move(v), L1D);
  }
}

template<std::size_t N, typename T, typename... Args>
inline void fill_index_lists(const T& arr, std::array<std::vector<int>, N>& converted_double_arrays,
                             std::array<std::vector<int>, N>& converted_bool_arrays,
                             std::array<const std::vector<int>*, N>& index_lists, Args&&... args) {
  std::size_t counter = 0;
  std::size_t counter_converted_double = 0;
  std::size_t counter_converted_bool = 0;
  forEachArg(
    [&](const auto& arg) {
      using A = std::decay_t<decltype(arg)>;
      // --- Case 1: vector<int>
      if constexpr (std::is_same_v<A, std::vector<int>>) {
        index_lists[counter++] = &arg;
      }
      // --- Case 2: vector<double>
      else if constexpr (std::is_same_v<A, std::vector<double>>) {
        auto& v = converted_double_arrays[counter_converted_double++];
        v.reserve(arg.size());
        for (double d : arg) {
          v.push_back(safe_index_from_double(d));
        }
        index_lists[counter++] = &v;
      }
      // --- Case 3: vector<bool>
      else if constexpr (std::is_same_v<A, std::vector<bool>>) {
        auto& v = converted_bool_arrays[counter_converted_bool++];
        v.reserve(arg.size());
        for (std::size_t b = 0; b < arg.size(); b++) {
          if (arg[b]) v.push_back(b + 1);
        }
        index_lists[counter++] = &v;
      }
      // --- Case 4: scalars
      else if constexpr (std::is_arithmetic_v<A>) {
        if constexpr (std::is_floating_point_v<A>) {
          auto& v = converted_double_arrays[counter_converted_double++];
          v.push_back(safe_index_from_double(arg));
          index_lists[counter++] = &v;
        } else if constexpr(std::is_integral_v<A> && std::is_same_v<A, bool>) {
          auto& v = converted_bool_arrays[counter_converted_bool++];
          if (arg) {
            const std::size_t len = arr.dim[counter];
            v.reserve(len);
            for (std::size_t i = 0; i < len; ++i) {
              v.push_back(static_cast<int>(i) + 1);
            }
          } else {
            ass<"Bool subsetting is only with TRUE possible">(false);
          }
          index_lists[counter++] = &v;
        } else if constexpr(std::is_integral_v<A> && !std::is_same_v<A, bool>) {
          auto& v = converted_double_arrays[counter_converted_double++];
          v.push_back(arg);
          index_lists[counter++] = &v;
        }
      }
      else {
        static_assert(!sizeof(A*), "Unsupported index type");
      }
    },
    args...
  );
}

template <typename... Args>
inline auto subset_mixed(Array& arr, const Args&... args) -> SubsetView<Array, sizeof...(Args), ZeroBased> {
  constexpr std::size_t N = sizeof...(Args);
  if (N > arr.dim.size()) {
    ass<"Too many index arguments for array rank">(false);
  }

  std::array<std::vector<int>, N> converted_double_arrays;
  std::array<std::vector<int>, N> converted_bool_arrays;
  std::array<const std::vector<int>*, N> index_lists{};

  fill_index_lists<N>(
    arr,
    converted_double_arrays,
    converted_bool_arrays,
    index_lists,
    args...
  );

  std::array<std::size_t, N> L{};
  for (std::size_t k = 0; k < N; k++) {
    if (!index_lists[k] || index_lists[k]->empty()) {
      ass<"Empty index for at least one dimension">(false);
    }
    L[k] = index_lists[k]->size();
  }

  auto stride = make_strides_from_vec<N>(arr.dim);

  std::size_t S = 1;
  for (std::size_t k = 0; k < N; k++) S *= L[k];

  std::vector<std::size_t> out;
  out.reserve(S);

  std::array<std::size_t, N> pos{};

  std::size_t offset = 0;
  std::size_t k = 0;
  for (;;) {
    offset = 1;
    for (std::size_t k = 0; k < N; k++) {
      offset += ((*index_lists[k])[pos[k]] - 1) * stride[k];
    }
    out.push_back(arr.data[offset - 1]);

    k = 0;
    for (;;) {
      pos[k] += 1;
      if (pos[k] < L[k]) break;
      pos[k] = 0;
      k++;
      if (k == N) {
        return SubsetView<Array, N, ZeroBased>(arr, std::move(out), L);
      }
    }
  }
}
