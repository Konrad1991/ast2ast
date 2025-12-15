#ifndef PRINT_ETR_HPP
#define PRINT_ETR_HPP

namespace etr {
#ifdef STANDALONE_ETR
#define PRINT_STREAM std::cout
#else
#define PRINT_STREAM Rcpp::Rcout
#endif

inline void print() { PRINT_STREAM << std::endl; }

template <typename T>
requires (IS<T, bool> || IS<T, int> || IS<T, double>)
inline void print(const T &inp) {
  if constexpr (IS<T, bool>) {
    PRINT_STREAM << std::boolalpha << inp << std::endl;
  } else {
    PRINT_STREAM << inp << std::endl;
  }
}

inline void print(const char *inp) {
  PRINT_STREAM << inp << std::endl;
}

template<typename P, typename T>
inline void print_matrix(const P& pos, const T& obj,
                         std::size_t rs, std::size_t cs, std::size_t offset) {
  if (!pos.empty()) {
    PRINT_STREAM << ", , ";
    for (std::size_t i = 0; i < pos.size(); i++) {
      PRINT_STREAM << pos[i] + 1;
      if (i < (pos.size() - 1)) {
        PRINT_STREAM << ", ";
      }
    }
    PRINT_STREAM << std::endl;
  }

  for (std::size_t r = 0; r < rs; r++) {
    for (std::size_t c = 0; c < cs; c++) {
      PRINT_STREAM << obj[offset + c * rs + r] << "   ";
    }
    PRINT_STREAM << std::endl;
  }
}

template<typename T, std::size_t N>
inline void print_subsetted_arrays(const T& obj) {
  auto strides_all = make_strides_from_vec<N>(obj.dim.get());
  std::array<std::size_t, N - 2> strides;
  for (std::size_t i = 0; i < strides.size(); i++) strides[i] = strides_all[i + 2];

  std::array<std::size_t, N - 2> pos{};
  std::array<std::size_t, N - 2> L;
  for (std::size_t i = 0; i < L.size(); i++) L[i] = obj.dim.get()[i + 2];

  std::size_t offset = 0;
  std::size_t k = 0;
  for (;;) {
    offset = 1;
    for (std::size_t k = 0; k < (N - 2); k++) {
      offset += (pos[k]) * strides[k];
    }
    print_matrix(pos, obj, obj.dim.get()[0], obj.dim.get()[1], offset - 1);
    PRINT_STREAM << std::endl;
    k = 0;
    for (;;) {
      pos[k] += 1;
      if (pos[k] < L[k]) break;
      pos[k] = 0;
      k++;
      if (k == (N - 2)) {
        return;
      }
    }
  }
}

template<typename T>
requires IsSubsetArray<T>
inline void print(const T& obj) {
  constexpr std::size_t N = subsetview_traits<Decayed<decltype(obj.d)>>::value;

  if (obj.dim.get().size() == 1) {
    for (std::size_t i = 0; i < obj.size(); i++) {
      PRINT_STREAM << obj[i] << "\t";
    }
    PRINT_STREAM << std::endl;
  }

  if (obj.dim.get().size() == 2) {
    print_matrix(std::vector<std::size_t>{0, 0}, obj, obj.dim.get()[0], obj.dim.get()[1], 0);
  }

  if (obj.dim.get().size() > 2) {
    if constexpr (N > 2) {
      print_subsetted_arrays<T, N>(obj);
    }
  }
}

template<typename Dim>
auto&& dim_view(Dim& d) {
  if constexpr (requires { d.get(); }) {
    return d.get();
  } else {
    return d;
  }
}

// Print array
template<typename T>
requires (!IsSubsetArray<T> && IsArray<T>)
inline void print(const T& arr) {

  const auto& dim = dim_view(arr.dim);
  if (dim.size() == 1) {
    for (std::size_t i = 0; i < arr.size(); i++) {
      PRINT_STREAM << arr[i] << "\t";
    }
    PRINT_STREAM << std::endl;
  }

  if (dim.size() == 2) {
    print_matrix(std::vector<std::size_t>{0, 0}, arr, dim[0], dim[1], 0);
  }

  if (dim.size() > 2) {
    auto strides_all = make_strides_dyn(dim);
    std::size_t N = strides_all.size();
    std::vector<std::size_t> strides(strides_all.size() - 2);
    for (std::size_t i = 0; i < strides.size(); i++) strides[i] = strides_all[i + 2];

    std::vector<std::size_t> pos(N - 2, 0);
    std::vector<std::size_t> L(N - 2, 0);
    for (std::size_t i = 0; i < L.size(); i++) L[i] = dim[i + 2];

    std::size_t offset = 0;
    std::size_t k = 0;
    for (;;) {
      offset = 1;
      for (std::size_t k = 0; k < (N - 2); k++) {
        offset += (pos[k]) * strides[k];
      }
      print_matrix(pos, arr, dim[0], dim[1], offset - 1);
      PRINT_STREAM << std::endl;
      k = 0;
      for (;;) {
        pos[k] += 1;
        if (pos[k] < L[k]) break;
        pos[k] = 0;
        k++;
        if (k == (N - 2)) {
          return;
        }
      }
    }
  }
}

} // namespace etr

#endif
