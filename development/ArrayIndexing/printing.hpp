template<typename ValueFn>
inline void print_1d(ValueFn value, std::size_t n, int width) {
  for (std::size_t i = 0; i < n; ++i)
    std::cout << std::setw(width) << value(i);
  std::cout << "\n";
}
template<typename ValueFn>
inline void print_2d(ValueFn value,
                     std::size_t rows,
                     std::size_t cols,
                     const std::array<std::size_t,2>& stride,
                     int width) {
  std::cout << "      ";
  for (std::size_t c = 0; c < cols; ++c)
    std::cout << std::setw(width) << ("[, " + std::to_string(c+1) + "]");
  std::cout << "\n";

  for (std::size_t r = 0; r < rows; ++r) {
    std::cout << "[" << std::setw(2) << (r+1) << ",] ";
    for (std::size_t c = 0; c < cols; ++c) {
      std::size_t off = r * stride[0] + c * stride[1];
      std::cout << std::setw(width) << value(off);
    }
    std::cout << "\n";
  }
}

template<std::size_t N, typename ValueFn>
inline void print_nd(ValueFn value,
                     const std::array<std::size_t,N>& dim,
                     const std::array<std::size_t,N>& stride,
                     int width) {
    std::vector<std::size_t> outer(N - 2, 0);

    auto print_slice = [&](const std::vector<std::size_t>& outer) {
        const std::size_t rows = dim[0], cols = dim[1];

        std::cout << "      ";
        for (std::size_t c = 0; c < cols; ++c)
            std::cout << std::setw(width) << ("[, " + std::to_string(c+1) + "]");
        std::cout << "\n";

        for (std::size_t r = 0; r < rows; ++r) {
            std::cout << "[" << std::setw(2) << (r+1) << ",] ";
            for (std::size_t c = 0; c < cols; ++c) {

                std::size_t off = r * stride[0] + c * stride[1];
                for (std::size_t t = 0; t + 2 < N; ++t)
                    off += outer[t] * stride[t+2];

                std::cout << std::setw(width) << value(off);
            }
            std::cout << "\n";
        }
    };

    for (;;) {
        std::cout << "\n, , ";
        for (std::size_t t = 0; t < outer.size(); ++t) {
            if (t) std::cout << ", ";
            std::cout << (outer[t] + 1);
        }
        std::cout << "\n\n";

        print_slice(outer);

        std::size_t ax = 0;
        for (;;) {
            outer[ax] += 1;
            if (outer[ax] < dim[ax + 2]) break;
            outer[ax] = 0;
            ++ax;
            if (ax == outer.size()) return;
        }
    }
}

template <typename O, std::size_t N, typename Mode>
inline void print(const SubsetView<O, N, Mode>& view) {

  auto stride = make_strides<N>(view.dim);

  int width = 1;
  for (std::size_t i = 0; i < view.indices.get().size(); ++i) {
    int w = std::to_string(view[i + 1]).size();
    width = std::max(width, w);
  }
  width++;

  if constexpr (N == 1) {
    print_1d(
      [&](std::size_t i){ return view[i + 1]; },
      view.dim[0],
      width
    );
    return;
  }

  if constexpr (N == 2) {
    print_2d(
      [&](std::size_t off){ return view[off + 1]; },
      view.dim[0],
      view.dim[1],
      std::array<std::size_t,2>{ stride[0], stride[1] },
      width
    );
    return;
  }

  print_nd<N>(
    [&](std::size_t off){ return view[off + 1]; },
    view.dim,
    stride,
    width
  );
}

void print(const Array& arr) {
  if (arr.dim.empty()) {
    std::cout << "[]\n";
    return;
  }

  switch (arr.dim.size()) {
    case 1: {
      int width = 1;
      for (int v : arr.data)
      width = std::max(width, int(std::to_string(v).size()));
      width++;

      print_1d([&](size_t i){ return arr.data[i]; }, arr.dim[0], width);
      break;
    }

    case 2: {
      std::array<std::size_t,2> d2{arr.dim[0], arr.dim[1]};
      auto stride = make_strides<2>(d2);

      int width = 1;
      for (int v : arr.data)
      width = std::max(width, int(std::to_string(v).size()));
      width++;

      print_2d([&](size_t i){ return arr.data[i]; },
               arr.dim[0], arr.dim[1], stride, width);
      break;
    }

    default: {
      constexpr std::size_t MAX = 16;
      std::array<std::size_t,MAX> d{};
      for (size_t i = 0; i < arr.dim.size(); ++i) d[i] = arr.dim[i];
      auto stride = make_strides<MAX>(d);

      int width = 1;
      for (int v : arr.data)
      width = std::max(width, int(std::to_string(v).size()));
      width++;

      print_nd<MAX>([&](size_t i){ return arr.data[i]; }, d, stride, width);
      break;
    }
  }
}
