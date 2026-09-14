#ifndef MERSENNETWISTER_ETR_HPP
#define MERSENNETWISTER_ETR_HPP

#include <cstddef>
#include <cstdint>

namespace etr {

// vendored from Konrad1991/Random (src/MersenneTwister.cpp): same algorithm
// R uses for RNGkind("Mersenne-Twister"), reimplemented so pso() does not
// need R's global RNG state (GetRNGstate/PutRNGstate) or an external package.
class MersenneTwister {
  static constexpr int N = 624;
  static constexpr int M = 397;
  static constexpr std::uint32_t MATRIX_A = 0x9908b0df;
  static constexpr std::uint32_t UPPER_MASK = 0x80000000;
  static constexpr std::uint32_t LOWER_MASK = 0x7fffffff;
  static constexpr double i2_32m1 = 2.328306437080797e-10;

  std::uint32_t i_seed[N + 1];
  bool initial;

  static double fixup(double obj) {
    if (obj == 0.0) return 0.5 * i2_32m1;
    if ((1.0 - obj) <= 0.0) return 1.0 - 0.5 * i2_32m1;
    return obj;
  }

  static int init_scrambling(int seed) {
    for (int i = 0; i < 50; i++) seed = seed * 69069 + 1;
    return seed;
  }

  void rng_init(int seed) {
    for (std::size_t i = 0; i <= N; i++) {
      seed = seed * 69069 + 1;
      i_seed[i] = static_cast<std::uint32_t>(seed);
    }
  }

  void fixup_seeds() {
    if (initial) {
      i_seed[0] = N;
      initial = false;
    }
    if (i_seed[0] == 0) i_seed[0] = N;
    bool all_zero = true;
    for (std::size_t i = 1; i <= N; i++) {
      if (i_seed[i] != 0) { all_zero = false; break; }
    }
    if (all_zero) rng_init(1);
  }

  static void MT_sgenrand(std::uint32_t *mt, int seed, int *mti) {
    for (int i = 0; i < N; i++) {
      mt[i] = static_cast<std::uint32_t>(seed) & 0xffff0000;
      seed = 69069 * seed + 1;
      mt[i] |= (static_cast<std::uint32_t>(seed) & 0xffff0000) >> 16;
      seed = 69069 * seed + 1;
    }
    *mti = N;
  }

  double MT_genrand() {
    std::uint32_t y;
    std::uint32_t *mt = i_seed + 1;
    static const std::uint32_t mag01[2] = {0x0, MATRIX_A};
    int mti = static_cast<int>(i_seed[0]);

    if (mti >= N) {
      if (mti == N + 1) MT_sgenrand(mt, 4357, &mti);
      int kk;
      for (kk = 0; kk < N - M; kk++) {
        y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
        mt[kk] = mt[kk + M] ^ (y >> 1) ^ mag01[y & 0x1];
      }
      for (; kk < N - 1; kk++) {
        y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
        mt[kk] = mt[kk + (M - N)] ^ (y >> 1) ^ mag01[y & 0x1];
      }
      y = (mt[N - 1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
      mt[N - 1] = mt[M - 1] ^ (y >> 1) ^ mag01[y & 0x1];
      mti = 0;
    }

    y = mt[mti++];
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680;
    y ^= (y << 15) & 0xefc60000;
    y ^= (y >> 18);
    i_seed[0] = static_cast<std::uint32_t>(mti);

    return static_cast<double>(y) * 2.3283064365386963e-10;
  }

public:
  explicit MersenneTwister(int seed) : initial(true) {
    rng_init(init_scrambling(seed));
    fixup_seeds();
  }

  double runif() { return fixup(MT_genrand()); }

  double runif(double lower, double upper) {
    return lower + (upper - lower) * runif();
  }

  // matches R_unif_index's contract (uniform integer in [0, n)) without
  // R's rejection-sampling machinery; fine for pso's neighborhood sampling
  std::size_t unif_index(std::size_t n) {
    return static_cast<std::size_t>(runif() * static_cast<double>(n));
  }
};

} // namespace etr

#endif
