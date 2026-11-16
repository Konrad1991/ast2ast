#ifndef KNUTHTAOCP2002_ETR_HPP
#define KNUTHTAOCP2002_ETR_HPP

#include <cmath>
#include <cstddef>
#include <cstdint>

namespace etr {

// same algorithm R uses for RNGkind("Knuth-TAOCP-2002") (src/main/RNG.c),
// reimplemented so it does not need R's global RNG state.
// Knuth's code uses `long`, but R redefines it as Int32 (unsigned 32 bit).
class knuth_taocp_2002 {
  static constexpr int long_lag = 100;
  static constexpr int short_lag = 37;
  static constexpr std::uint32_t modulus = std::uint32_t(1) << 30;
  static constexpr int stream_separation = 70;
  static constexpr int quality = 1009;
  static constexpr double two_power_minus_30 = 9.31322574615479e-10;
  static constexpr double i2_32m1 = 2.328306437080797e-10;

  std::uint32_t state[long_lag];
  int position;

  static double fixup(double obj) {
    if (obj <= 0.0) return 0.5 * i2_32m1;
    if ((1.0 - obj) <= 0.0) return 1.0 - 0.5 * i2_32m1;
    return obj;
  }

  static std::uint32_t init_scrambling(std::uint32_t seed) {
    for (int i = 0; i < 50; i++) seed = seed * 69069 + 1;
    return seed;
  }

  static std::uint32_t mod_difference(std::uint32_t x, std::uint32_t y) {
    return (x - y) & (modulus - 1);
  }

  void random_array(std::uint32_t *buffer, int length) {
    int i, j;
    for (j = 0; j < long_lag; j++) buffer[j] = state[j];
    for (; j < length; j++) buffer[j] = mod_difference(buffer[j - long_lag], buffer[j - short_lag]);
    for (i = 0; i < short_lag; i++, j++) state[i] = mod_difference(buffer[j - long_lag], buffer[j - short_lag]);
    for (; i < long_lag; i++, j++) state[i] = mod_difference(buffer[j - long_lag], state[i - short_lag]);
  }

  void random_start(std::uint32_t seed) {
    constexpr int buffer_length = long_lag + long_lag - 1;
    std::uint32_t buffer[buffer_length];
    std::uint32_t shift = (seed + 2) & (modulus - 2);
    for (int j = 0; j < long_lag; j++) {
      buffer[j] = shift;
      shift <<= 1;
      if (shift >= modulus) shift -= modulus - 2;
    }
    buffer[1]++;
    shift = seed & (modulus - 1);
    int t = stream_separation - 1;
    while (t) {
      for (int j = long_lag - 1; j > 0; j--) {
        buffer[j + j] = buffer[j];
        buffer[j + j - 1] = 0;
      }
      for (int j = buffer_length - 1; j >= long_lag; j--) {
        buffer[j - (long_lag - short_lag)] = mod_difference(buffer[j - (long_lag - short_lag)], buffer[j]);
        buffer[j - long_lag] = mod_difference(buffer[j - long_lag], buffer[j]);
      }
      if (shift & 1) {
        for (int j = long_lag; j > 0; j--) buffer[j] = buffer[j - 1];
        buffer[0] = buffer[long_lag];
        buffer[short_lag] = mod_difference(buffer[short_lag], buffer[long_lag]);
      }
      if (shift) shift >>= 1; else t--;
    }
    int j;
    for (j = 0; j < short_lag; j++) state[j + long_lag - short_lag] = buffer[j];
    for (; j < long_lag; j++) state[j - short_lag] = buffer[j];
    for (j = 0; j < 10; j++) random_array(buffer, buffer_length);
  }

  // R returns the refreshed state, not the 1009 long buffer
  std::uint32_t next() {
    if (position >= long_lag) {
      std::uint32_t buffer[quality];
      random_array(buffer, quality);
      position = 0;
    }
    return state[position++];
  }

  // R's rbits(): non-negative integer < 2^bits in 16 bit chunks
  double rbits(int bits) {
    std::int_least64_t v = 0;
    for (int n = 0; n <= bits; n += 16) {
      const int v1 = static_cast<int>(std::floor(runif() * 65536));
      v = 65536 * v + v1;
    }
    const std::int_least64_t one64 = 1L;
    return static_cast<double>(v & ((one64 << bits) - 1));
  }

public:
  Integer seed;

  knuth_taocp_2002() : knuth_taocp_2002(0) {}
  explicit knuth_taocp_2002(int seed_) : seed(seed_) {
    random_start(init_scrambling(static_cast<std::uint32_t>(seed_)) % 1073741821);
    position = long_lag;
  }

  double runif() { return fixup(next() * two_power_minus_30); }

  double runif(double lower, double upper) {
    return lower + (upper - lower) * runif();
  }

  // R_unif_index with sample.kind = "Rejection" (R's default): uniform integer in [0, n)
  std::size_t unif_index(std::size_t n) {
    const double dn = static_cast<double>(n);
    if (dn <= 0) return 0;
    const int bits = static_cast<int>(std::ceil(std::log2(dn)));
    double dv;
    do { dv = rbits(bits); } while (dn <= dv);
    return static_cast<std::size_t>(dv);
  }
};

} // namespace etr

#endif
