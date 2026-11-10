#ifndef SHELL_SORT_ETR_HPP
#define SHELL_SORT_ETR_HPP

#include <cstddef>
#include <vector>

namespace etr {

// Shell sort with Sedgewick (1986) increments, as R's sort(method = "shell").
// Not stable.
// -----------------------------------------------------------------------------------------------------------
template <typename T, typename Less>
inline void shell_sort(std::vector<T> &values, Less less) {
  static constexpr std::size_t increments[] = {
    1073790977, 268460033, 67121153, 16783361, 4197377, 1050113,
    262913, 65921, 16577, 4193, 1073, 281, 77, 23, 8, 1
  };
  const std::size_t n = values.size();
  for (const std::size_t h : increments) {
    if (h >= n) continue;
    for (std::size_t i = h; i < n; i++) {
      T current = values[i];
      std::size_t j = i;
      while (j >= h && less(current, values[j - h])) {
        values[j] = values[j - h];
        j -= h;
      }
      values[j] = current;
    }
  }
}

// Stable; used by sort(method = "auto") for short inputs.
template <typename T, typename Less>
inline void insertion_sort(std::vector<T> &values, Less less) {
  for (std::size_t i = 1; i < values.size(); i++) {
    T current = values[i];
    std::size_t j = i;
    while (j > 0 && less(current, values[j - 1])) {
      values[j] = values[j - 1];
      j--;
    }
    values[j] = current;
  }
}

}

#endif
