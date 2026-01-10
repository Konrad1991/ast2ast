template<typename P, typename T>
inline void print_matrix(const P& pos, const T& obj,
                         std::size_t rs, std::size_t cs, std::size_t offset) {
  if (!pos.empty()) {
    std::cout << ", , ";
    for (std::size_t i = 0; i < pos.size(); i++) {
      std::cout << pos[i] + 1;
      if (i < (pos.size() - 1)) {
        std::cout << ", ";
      }
    }
    std::cout << std::endl;
  }

  for (std::size_t r = 0; r < rs; r++) {
    for (std::size_t c = 0; c < cs; c++) {
      std::cout << obj[offset + c * rs + r] << "   ";
    }
    std::cout << std::endl;
  }
}

template <typename O, std::size_t N, typename Mode>
inline void print(const SubsetView<O, N, Mode>& view) {

  if (view.dim.size() == 1) {
    for (std::size_t i = 0; i < view.size(); i++) {
      std::cout << view[i] << "\t";
    }
    std::cout << std::endl;
  }

  if (view.dim.size() == 2) {
    print_matrix(std::vector<std::size_t>{0, 0}, view, view.dim[0], view.dim[1], 0);
  }

  if (view.dim.size() > 2) {
    auto strides_all = make_strides<N>(view.dim);
    std::array<std::size_t, N - 2> strides;
    for (std::size_t i = 0; i < strides.size(); i++) strides[i] = strides_all[i + 2];

    std::array<std::size_t, N - 2> pos{};
    std::array<std::size_t, N - 2> L;
    for (std::size_t i = 0; i < L.size(); i++) L[i] = view.dim[i + 2];

    std::size_t offset = 0;
    std::size_t k = 0;
    for (;;) {
      offset = 1;
      for (std::size_t k = 0; k < (N - 2); k++) {
        offset += (pos[k]) * strides[k];
      }
      print_matrix(pos, view, view.dim[0], view.dim[1], offset - 1);
      std::cout << std::endl;
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

inline void print(const Array& arr) {

  if (arr.dim.size() == 1) {
    for (std::size_t i = 0; i < arr.size(); i++) {
      std::cout << arr[i] << "\t";
    }
    std::cout << std::endl;
  }

  if (arr.dim.size() == 2) {
    print_matrix(std::vector<std::size_t>{0, 0}, arr, arr.dim[0], arr.dim[1], 0);
  }

  if (arr.dim.size() > 2) {
    auto strides_all = make_strides_dyn(arr.dim);
    std::size_t N = strides_all.size();
    std::vector<std::size_t> strides(strides_all.size() - 2);
    for (std::size_t i = 0; i < strides.size(); i++) strides[i] = strides_all[i + 2];

    std::vector<std::size_t> pos(N - 2, 0);
    std::vector<std::size_t> L(N - 2, 0);
    for (std::size_t i = 0; i < L.size(); i++) L[i] = arr.dim[i + 2];

    std::size_t offset = 0;
    std::size_t k = 0;
    for (;;) {
      offset = 1;
      for (std::size_t k = 0; k < (N - 2); k++) {
        offset += (pos[k]) * strides[k];
      }
      print_matrix(pos, arr, arr.dim[0], arr.dim[1], offset - 1);
      std::cout << std::endl;
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
