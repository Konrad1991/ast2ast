#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"
#include <chrono>

using namespace etr;
using clock_type = std::chrono::steady_clock;

template <typename T>
double ms_of(std::chrono::nanoseconds d) { return std::chrono::duration<double, std::milli>(d).count(); }

int main() {
  Integer nx(100);
  Integer ny(100);
  const std::size_t inner_n = static_cast<std::size_t>((nx.val - 2) * (ny.val - 2));
  const int reps = 5000;

  Array<Double, Buffer<Double>> temp_local = matrix(Double(0.0), nx, ny);
  for (std::size_t idx = 0; idx < temp_local.size(); idx++) temp_local.set(idx, static_cast<double>(idx));

  // Ascending ranges -> both dims should be detected as strided.
  Array<Integer, Buffer<Integer>> i_range = colon(Integer(2), (nx - Integer(1)));
  Array<Integer, Buffer<Integer>> j_range = colon(Integer(2), (ny - Integer(1)));

  // Irregular (non-arithmetic) index arrays of the same length, same dims.
  Array<Integer, Buffer<Integer>> i_irregular(SI{i_range.size()});
  Array<Integer, Buffer<Integer>> j_irregular(SI{j_range.size()});
  {
    // A permutation, not an arithmetic sequence, but still valid indices
    // into a 100x100 array with the same output extent as i_range/j_range.
    for (std::size_t k = 0; k < i_irregular.size(); k++) {
      int v = 2 + static_cast<int>((k * 7) % i_irregular.size());
      i_irregular.set(k, v);
    }
    for (std::size_t k = 0; k < j_irregular.size(); k++) {
      int v = 2 + static_cast<int>((k * 7) % j_irregular.size());
      j_irregular.set(k, v);
    }
  }

  PRINT_STREAM << "=== Static type check ===" << std::endl;
  auto sv_strided = subset(temp_local, i_range, j_range);
  printT<decltype(sv_strided)>();

  PRINT_STREAM << std::endl << "=== Runtime variant check: fully strided (i_range, j_range) ===" << std::endl;
  PRINT_STREAM << "dim0 holds StridedLayout: " << std::boolalpha
               << std::holds_alternative<StridedLayout<2>>(sv_strided.d.repr) << std::endl;

  auto sv_mixed = subset(temp_local, i_range, j_irregular);
  PRINT_STREAM << "mixed (i_range strided, j_irregular not) holds StridedLayout: " << std::boolalpha
               << std::holds_alternative<StridedLayout<2>>(sv_mixed.d.repr) << std::endl;

  auto sv_fallback = subset(temp_local, i_irregular, j_irregular);
  PRINT_STREAM << "fully irregular holds StridedLayout: " << std::boolalpha
               << std::holds_alternative<StridedLayout<2>>(sv_fallback.d.repr) << std::endl;

  PRINT_STREAM << std::endl << "=== ConstHolder storage-path check for `sv_strided + Double(0.0)` ===" << std::endl;
  auto expr = sv_strided + Double(0.0);
  printT<decltype(expr.d)>();
  PRINT_STREAM << "l (SubsetView operand) owns via shared_ptr: " << std::boolalpha
               << expr.d.l.debug_owns_via_shared_ptr() << std::endl;
  PRINT_STREAM << "l (SubsetView operand) owns via inline value: " << std::boolalpha
               << expr.d.l.debug_owns_via_inline_value() << std::endl;
  PRINT_STREAM << "r (Double(0.0) operand) owns via shared_ptr: " << std::boolalpha
               << expr.d.r.debug_owns_via_shared_ptr() << std::endl;
  PRINT_STREAM << "r (Double(0.0) operand) owns via inline value: " << std::boolalpha
               << expr.d.r.debug_owns_via_inline_value() << std::endl;

  PRINT_STREAM << std::endl << "=== Timed, now-verified comparison of different subset shapes ===" << std::endl;
  Array<Double, Buffer<Double>> c(SI{inner_n});

  auto t0 = clock_type::now();
  for (int rep = 0; rep < reps; rep++) c = subset(temp_local, i_range, j_range) + Double(0.0);
  auto t_strided = clock_type::now() - t0;

  auto t1 = clock_type::now();
  for (int rep = 0; rep < reps; rep++) c = subset(temp_local, i_range, j_irregular) + Double(0.0);
  auto t_mixed = clock_type::now() - t1;

  auto t2 = clock_type::now();
  for (int rep = 0; rep < reps; rep++) c = subset(temp_local, i_irregular, j_irregular) + Double(0.0);
  auto t_fallback = clock_type::now() - t2;

  // 1-D strided, for comparison against the 2-D strided case (N=1 vs N=2
  // decomposition -- only 1 axis, no carry/second-dim work at all).
  Array<Double, Buffer<Double>> v1d(SI{inner_n});
  for (std::size_t idx = 0; idx < v1d.size(); idx++) v1d.set(idx, static_cast<double>(idx));
  Array<Integer, Buffer<Integer>> k_range = colon(Integer(1), Integer(static_cast<int>(inner_n)));
  Array<Double, Buffer<Double>> c1d(SI{inner_n});
  auto sv_1d = subset(v1d, k_range);
  PRINT_STREAM << "1-D range holds StridedLayout: " << std::boolalpha
               << std::holds_alternative<StridedLayout<1>>(sv_1d.d.repr) << std::endl;
  auto t3 = clock_type::now();
  for (int rep = 0; rep < reps; rep++) c1d = subset(v1d, k_range) + Double(0.0);
  auto t_1d_strided = clock_type::now() - t3;

  PRINT_STREAM << std::endl;
  PRINT_STREAM << "2-D fully strided (both AxisStride):      " << ms_of<void>(t_strided)  << " ms" << std::endl;
  PRINT_STREAM << "2-D mixed (1 strided, 1 irregular):       " << ms_of<void>(t_mixed)    << " ms" << std::endl;
  PRINT_STREAM << "2-D fully irregular (fallback both dims): " << ms_of<void>(t_fallback) << " ms" << std::endl;
  PRINT_STREAM << "1-D fully strided:                        " << ms_of<void>(t_1d_strided) << " ms" << std::endl;

  return static_cast<int>(c.get(0).val + c1d.get(0).val) % 2;
}
