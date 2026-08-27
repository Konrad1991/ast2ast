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
requires IsScalarLike<T>
inline void print(const T &inp) {
  PRINT_STREAM << inp << std::endl;
}

inline void print(const char *inp) {
  PRINT_STREAM << inp << std::endl;
}

template <typename T>
inline void print(const Collection<T> &c) {
  PRINT_STREAM << "Collection[" << c.size() << "]" << std::endl;
  for (const auto &elem : c) {
    print(elem); // unqualified: ADL finds the element struct's own print(), which lives outside namespace etr
  }
}

template<typename P, typename T>
inline void print_matrix(const P& pos, const T& obj,
                         std::size_t rs, std::size_t cs, std::size_t offset) {
  // pos is only non-empty for the pages of a rank > 2 array; a plain matrix
  // passes an empty pos and gets no ", , i" slab header.
  if (!pos.empty()) {
    PRINT_STREAM << ", , ";
    for (std::size_t i = 0; i < pos.size(); i++) {
      PRINT_STREAM << pos[i] + 1;
      if (i < (pos.size() - 1)) {
        PRINT_STREAM << ", ";
      }
    }
    PRINT_STREAM << "\n" << std::endl;
  }

  auto pad = [](const std::string& s, std::size_t w) {
    return std::string(w > s.size() ? w - s.size() : 0, ' ') + s;
  };

  // Stringify every cell first so each column can be right-aligned to its
  // widest entry, matching R's matrix layout.
  std::vector<std::string> cell(rs * cs);
  std::vector<std::size_t> width(cs, 0);
  for (std::size_t c = 0; c < cs; c++) {
    std::ostringstream head;
    head << "[," << (c + 1) << "]";
    width[c] = head.str().size();
    for (std::size_t r = 0; r < rs; r++) {
      std::ostringstream v;
      v << obj.get(offset + c * rs + r);
      cell[c * rs + r] = v.str();
      width[c] = std::max(width[c], cell[c * rs + r].size());
    }
  }

  std::ostringstream last_label;
  last_label << "[" << rs << ",]";
  const std::size_t label_width = last_label.str().size();

  PRINT_STREAM << std::string(label_width, ' ');
  for (std::size_t c = 0; c < cs; c++) {
    std::ostringstream head;
    head << "[," << (c + 1) << "]";
    PRINT_STREAM << " " << pad(head.str(), width[c]);
  }
  PRINT_STREAM << std::endl;

  for (std::size_t r = 0; r < rs; r++) {
    std::ostringstream label;
    label << "[" << (r + 1) << ",]";
    PRINT_STREAM << pad(label.str(), label_width);
    for (std::size_t c = 0; c < cs; c++) {
      PRINT_STREAM << " " << pad(cell[c * rs + r], width[c]);
    }
    PRINT_STREAM << std::endl;
  }
}

template<typename T, std::size_t N>
inline void print_subsetted_arrays(const T& obj) {
  const auto& dim = dim_view(obj.get_dim());
  auto strides_all = make_strides_from_vec<N>(dim);
  std::array<std::size_t, N - 2> strides;
  for (std::size_t i = 0; i < strides.size(); i++) strides[i] = strides_all[i + 2];

  std::array<std::size_t, N - 2> pos{};
  std::array<std::size_t, N - 2> L;
  for (std::size_t i = 0; i < L.size(); i++) L[i] = dim[i + 2];

  std::size_t offset = 0;
  std::size_t k = 0;
  for (;;) {
    offset = 1;
    for (std::size_t k = 0; k < (N - 2); k++) {
      offset += (pos[k]) * strides[k];
    }
    print_matrix(pos, obj, dim[0], dim[1], offset - 1);
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

  const auto& dim = dim_view(obj.get_dim());
  if (dim.size() == 1) {
    for (std::size_t i = 0; i < obj.size(); i++) {
      PRINT_STREAM << obj.get(i) << "\t";
    }
    PRINT_STREAM << std::endl;
  }

  if (dim.size() == 2) {
    print_matrix(std::vector<std::size_t>{}, obj, dim[0], dim[1], 0);
  }

  if (dim.size() > 2) {
    if constexpr (N > 2) {
      print_subsetted_arrays<T, N>(obj);
    }
  }
}

// Print array
template<typename T>
requires (!IsSubsetArray<T> && IsArray<T>)
inline void print(const T& arr) {

  const auto& dim = dim_view(arr.get_dim());
  if (dim.size() == 1) {
    for (std::size_t i = 0; i < arr.size(); i++) {
      PRINT_STREAM << arr.get(i) << "\t";
    }
    PRINT_STREAM << std::endl;
  }

  if (dim.size() == 2) {
    print_matrix(std::vector<std::size_t>{}, arr, dim[0], dim[1], 0);
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
