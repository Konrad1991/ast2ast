#include <chrono>
#include <cstddef>
#include <cstdio>
#include <stdexcept>
#include <vector>

using clock_type = std::chrono::steady_clock;

// Mimics ass<msg>(cond) -- a cheap branch that (almost) never fires.
inline void ck(bool cond) { if (!cond) throw std::runtime_error("x"); }

int main() {
  const std::size_t n0 = 98, n1 = 98;
  const std::size_t total = n0 * n1;
  const int reps = 5000;

  std::vector<double> src(total), dst(total);
  for (std::size_t i = 0; i < total; i++) src[i] = static_cast<double>(i);

  // Per-axis source data, exactly like i_range/j_irregular: axis 0 is a
  // clean arithmetic sequence (start=2, step=1), axis 1 is looked up from a
  // materialized int buffer (mimicking an irregular Array<Integer>).
  std::vector<int> axis1_vals(n1);
  for (std::size_t k = 0; k < n1; k++) axis1_vals[k] = static_cast<int>(2 + (k * 7) % n1);

  volatile std::size_t sink = 0;

  // (A) "fully strided": zero per-rep construction (O(1) composition,
  // mimicked by just reading two already-known numbers), all cost is in the
  // per-element div-based decode, NO per-element checks (already validated
  // once at construction in the real code).
  auto t0 = clock_type::now();
  for (int rep = 0; rep < reps; rep++) {
    const long stride0 = 1, stride1 = static_cast<long>(n0);
    for (std::size_t i = 0; i < total; i++) {
      std::size_t rem = i;
      std::size_t c0 = rem % n0; rem /= n0;
      std::size_t c1 = rem % n1;
      std::size_t off = static_cast<std::size_t>(c0 * stride0 + c1 * stride1);
      dst[i] = src[off];
    }
  }
  auto t_strided = clock_type::now() - t0;
  sink += static_cast<std::size_t>(dst[0]);

  // (B) "fully irregular" fallback: per-rep odometer CONSTRUCTION (O(total),
  // two buffer-lookup axes, each position checked) THEN cheap flat-array
  // access.
  std::vector<int> flat_offset(total);
  auto t1 = clock_type::now();
  for (int rep = 0; rep < reps; rep++) {
    // odometer construction, mirroring create_indices' fallback loop shape
    std::size_t pos0 = 0, pos1 = 0, counter = 0;
    for (;;) {
      int val0 = axis1_vals[pos0 % n0]; // pretend axis0 is ALSO irregular here
      int val1 = axis1_vals[pos1];
      ck(val0 >= 1); ck(val1 >= 1);
      std::size_t off = static_cast<std::size_t>((val0 - 1)) * 1 + static_cast<std::size_t>((val1 - 1)) * n0;
      flat_offset[counter++] = static_cast<int>(off);
      pos0 += 1;
      if (pos0 < n0) continue;
      pos0 = 0; pos1 += 1;
      if (pos1 == n1) break;
    }
    for (std::size_t i = 0; i < total; i++) {
      dst[i] = src[static_cast<std::size_t>(flat_offset[i])];
    }
  }
  auto t_fallback = clock_type::now() - t1;
  sink += static_cast<std::size_t>(dst[0]);

  // (C) "mixed": per-rep odometer construction with ONE axis cheap-arithmetic
  // (no buffer read) and ONE axis buffer-lookup, then cheap flat access.
  auto t2 = clock_type::now();
  for (int rep = 0; rep < reps; rep++) {
    std::size_t pos0 = 0, pos1 = 0, counter = 0;
    for (;;) {
      int val0 = static_cast<int>(2 + pos0); // axis0 strided: start=2, step=1
      int val1 = axis1_vals[pos1];           // axis1 irregular: buffer lookup
      ck(val0 >= 1); ck(val1 >= 1);
      std::size_t off = static_cast<std::size_t>((val0 - 1)) * 1 + static_cast<std::size_t>((val1 - 1)) * n0;
      flat_offset[counter++] = static_cast<int>(off);
      pos0 += 1;
      if (pos0 < n0) continue;
      pos0 = 0; pos1 += 1;
      if (pos1 == n1) break;
    }
    for (std::size_t i = 0; i < total; i++) {
      dst[i] = src[static_cast<std::size_t>(flat_offset[i])];
    }
  }
  auto t_mixed = clock_type::now() - t2;
  sink += static_cast<std::size_t>(dst[0]);

  auto ms = [](std::chrono::nanoseconds d) { return std::chrono::duration<double, std::milli>(d).count(); };
  printf("fully strided (O(1) build + div access):        %.3f ms\n", ms(t_strided));
  printf("fully irregular (O(n) build, 2 lookups + access): %.3f ms\n", ms(t_fallback));
  printf("mixed (O(n) build, 1 lookup + access):            %.3f ms\n", ms(t_mixed));
  printf("sink=%zu\n", sink);
  return 0;
}
