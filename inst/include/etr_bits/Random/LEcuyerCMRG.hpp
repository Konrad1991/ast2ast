#ifndef LECUYERCMRG_ETR_HPP
#define LECUYERCMRG_ETR_HPP

#include <cmath>
#include <cstddef>
#include <cstdint>

namespace etr {

// same algorithm R uses for RNGkind("L'Ecuyer-CMRG") (src/main/RNG.c),
// reimplemented so it does not need R's global RNG state.
class lecuyer_cmrg {
  static constexpr std::int_least64_t m1 = 4294967087;
  static constexpr std::int_least64_t m2 = 4294944443;
  static constexpr double normc = 2.328306549295727688e-10;
  static constexpr std::int_least64_t a12 = 1403580;
  static constexpr std::int_least64_t a13n = 810728;
  static constexpr std::int_least64_t a21 = 527612;
  static constexpr std::int_least64_t a23n = 1370589;

  std::uint32_t state[6];

  static std::uint32_t init_scrambling(std::uint32_t seed) {
    for (int i = 0; i < 50; i++) seed = seed * 69069 + 1;
    return seed;
  }

  void rng_init(std::uint32_t seed) {
    for (int j = 0; j < 6; j++) {
      seed = seed * 69069 + 1;
      while (seed >= m2) seed = seed * 69069 + 1;
      state[j] = seed;
    }
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

  lecuyer_cmrg() : lecuyer_cmrg(0) {}
  explicit lecuyer_cmrg(int seed_) : seed(seed_) {
    rng_init(init_scrambling(static_cast<std::uint32_t>(seed_)));
  }

  // R applies no fixup here: the result is already in (0, 1)
  double runif() {
    std::int_least64_t p1 = a12 * state[1] - a13n * state[0];
    p1 -= (p1 / m1) * m1;
    if (p1 < 0) p1 += m1;
    state[0] = state[1]; state[1] = state[2]; state[2] = static_cast<std::uint32_t>(p1);

    std::int_least64_t p2 = a21 * state[5] - a23n * state[3];
    p2 -= (p2 / m2) * m2;
    if (p2 < 0) p2 += m2;
    state[3] = state[4]; state[4] = state[5]; state[5] = static_cast<std::uint32_t>(p2);

    return static_cast<double>((p1 > p2) ? (p1 - p2) : (p1 - p2 + m1)) * normc;
  }

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
