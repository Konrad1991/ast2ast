#include <chrono>
#include <cstddef>
#include <cstdio>
#include <vector>

using clock_type = std::chrono::steady_clock;

int main() {
  const std::size_t n0 = 98, n1 = 98;
  const std::size_t total = n0 * n1;
  const int reps = 5000;

  std::vector<double> src(total), dst(total);
  for (std::size_t i = 0; i < total; i++) src[i] = static_cast<double>(i);

  volatile std::size_t sink = 0;

  // (A) division-based decode: exactly StridedLayout::offset_of's shape for N=2.
  auto t0 = clock_type::now();
  for (int rep = 0; rep < reps; rep++) {
    for (std::size_t i = 0; i < total; i++) {
      std::size_t rem = i;
      std::size_t c0 = rem % n0; rem /= n0;
      std::size_t c1 = rem % n1; rem /= n1;
      std::size_t off = c0 * 1 + c1 * n0;
      dst[i] = src[off];
    }
  }
  auto t_div = clock_type::now() - t0;
  sink += static_cast<std::size_t>(dst[0]);

  // (B) corrected lookup-based decode: the REAL etr fallback path already has
  // the fully-computed flat offset materialized once at construction (via
  // the odometer) -- so at access time it's ONE indexed read, not two
  // separate per-axis reads + a multiply-add like the flawed first version
  // of this test simulated.
  std::vector<int> flat_offset(total);
  for (std::size_t i = 0; i < total; i++) {
    std::size_t rem = i;
    std::size_t c0 = rem % n0; rem /= n0;
    std::size_t c1 = rem % n1;
    flat_offset[i] = static_cast<int>(c0 * 1 + c1 * n0);
  }
  auto t1 = clock_type::now();
  for (int rep = 0; rep < reps; rep++) {
    for (std::size_t i = 0; i < total; i++) {
      dst[i] = src[static_cast<std::size_t>(flat_offset[i])];
    }
  }
  auto t_lookup = clock_type::now() - t1;
  sink += static_cast<std::size_t>(dst[0]);

  // (C) floor: direct sequential copy, no decode at all.
  auto t2 = clock_type::now();
  for (int rep = 0; rep < reps; rep++) {
    for (std::size_t i = 0; i < total; i++) {
      dst[i] = src[i];
    }
  }
  auto t_plain = clock_type::now() - t2;
  sink += static_cast<std::size_t>(dst[0]);

  auto ms = [](std::chrono::nanoseconds d) { return std::chrono::duration<double, std::milli>(d).count(); };
  printf("div-decode:    %.3f ms\n", ms(t_div));
  printf("lookup-decode: %.3f ms\n", ms(t_lookup));
  printf("plain copy:    %.3f ms\n", ms(t_plain));
  printf("sink=%zu\n", sink);
  return 0;
}
