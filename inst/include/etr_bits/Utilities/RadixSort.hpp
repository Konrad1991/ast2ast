#ifndef RADIX_SORT_ETR_HPP
#define RADIX_SORT_ETR_HPP

#include <array>
#include <cstdint>
#include <cstring>
#include <numeric>
#include <vector>

namespace etr {

// LSD radix sort, 8-bit digits, stable. Values are mapped to unsigned keys
// whose unsigned order equals the signed / floating point order.
// NA / NaN are not handled here; the caller removes them first.
// -----------------------------------------------------------------------------------------------------------
inline std::uint32_t radix_key(std::int32_t v) {
  return static_cast<std::uint32_t>(v) ^ 0x80000000u;
}
inline std::uint64_t radix_key(double v) {
  // -0.0 would otherwise sort before 0.0 and break stability of ties
  if (v == 0.0) v = 0.0;
  std::uint64_t bits;
  std::memcpy(&bits, &v, sizeof(bits));
  constexpr std::uint64_t sign = 0x8000000000000000ull;
  return (bits & sign) ? ~bits : (bits | sign);
}

template <typename Key>
inline void radix_sort_keys(std::vector<Key> &keys, std::vector<std::size_t> &index) {
  const std::size_t n = keys.size();
  if (n < 2) return;
  std::vector<Key> keys_tmp(n);
  std::vector<std::size_t> index_tmp(n);
  for (std::size_t pass = 0; pass < sizeof(Key); pass++) {
    const unsigned shift = static_cast<unsigned>(pass * 8);
    std::array<std::size_t, 256> count{};
    for (std::size_t i = 0; i < n; i++) count[(keys[i] >> shift) & 0xFF]++;
    if (count[(keys[0] >> shift) & 0xFF] == n) continue; // all share this digit
    std::size_t offset = 0;
    for (auto &c : count) {
      const std::size_t current = c;
      c = offset;
      offset += current;
    }
    for (std::size_t i = 0; i < n; i++) {
      const std::size_t position = count[(keys[i] >> shift) & 0xFF]++;
      keys_tmp[position] = keys[i];
      index_tmp[position] = index[i];
    }
    keys.swap(keys_tmp);
    index.swap(index_tmp);
  }
}

// Returns the 0-based permutation that sorts `values` (like order() - 1).
template <typename T>
inline std::vector<std::size_t> radix_order(const std::vector<T> &values, bool decreasing = false) {
  using Key = decltype(radix_key(T{}));
  std::vector<Key> keys(values.size());
  for (std::size_t i = 0; i < values.size(); i++) {
    // inverting the key keeps ties in input order, unlike reversing the result
    keys[i] = decreasing ? static_cast<Key>(~radix_key(values[i])) : radix_key(values[i]);
  }
  std::vector<std::size_t> index(values.size());
  std::iota(index.begin(), index.end(), std::size_t{0});
  radix_sort_keys(keys, index);
  return index;
}

template <typename T>
inline void radix_sort(std::vector<T> &values, bool decreasing = false) {
  const std::vector<std::size_t> index = radix_order(values, decreasing);
  std::vector<T> sorted(values.size());
  for (std::size_t i = 0; i < index.size(); i++) sorted[i] = values[index[i]];
  values.swap(sorted);
}

}

#endif
