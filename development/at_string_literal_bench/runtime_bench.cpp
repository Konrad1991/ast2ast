#define STANDALONE_ETR
#include "../../inst/include/etr_bits/Core.hpp"
#include "../../inst/include/etr_bits/Subsetting.hpp"
#include "at_tagged.hpp"
#include "at_labeled.hpp"
#include "at_controls.hpp"
using namespace etr;

#include <chrono>
#include <iostream>

// Isolates the per-call runtime cost of each design (does NOT test the
// compile-time/instantiation-count question -- that's gen.sh). One call
// site per variant, looped many times, identical work otherwise: does the
// try/catch wrapper and/or the extra runtime label argument cost anything
// in the hot path compared to today's unwrapped at()?

constexpr std::size_t SZ = 1000;
constexpr std::size_t ITERS = 50'000'000;

template <typename F>
double time_it(const char* label, F&& f) {
  auto t0 = std::chrono::steady_clock::now();
  double acc = f();
  auto t1 = std::chrono::steady_clock::now();
  double ms = std::chrono::duration<double, std::milli>(t1 - t0).count();
  std::cout << label << ": " << ms << " ms  (sink=" << acc << ")\n";
  return ms;
}

// One try/catch total, at the (simulated) outer function boundary. Each
// risky operation just stores a pointer to its label before running --
// a single mov, not a try scope -- so the outer catch can attribute the
// error without any per-call-site try/catch at all.
thread_local const char* g_current_expr = "";

int main() {
  Array<Double, Buffer<Double>> a(SI{SZ});
  for (std::size_t i = 0; i < SZ; i++) a.d.set(i, Double((double)i));

  double baseline_ms = time_it("baseline (at, unwrapped, today)", [&]() {
    Double acc(0.0);
    for (std::size_t i = 0; i < ITERS; i++) {
      acc = acc + at(a, Integer((int)(i % SZ) + 1));
    }
    return get_val(acc);
  });

  double copy_ms = time_it("copy     (at_copy, no try/catch, own name)", [&]() {
    Double acc(0.0);
    for (std::size_t i = 0; i < ITERS; i++) {
      acc = acc + at_copy(a, Integer((int)(i % SZ) + 1));
    }
    return get_val(acc);
  });

  double trycatch_ms = time_it("trycatch (at_trycatch_only, wrap, no label)", [&]() {
    Double acc(0.0);
    for (std::size_t i = 0; i < ITERS; i++) {
      acc = acc + at_trycatch_only(a, Integer((int)(i % SZ) + 1));
    }
    return get_val(acc);
  });

  double labeled_ms = time_it("labeled  (at_labeled, runtime arg)", [&]() {
    Double acc(0.0);
    for (std::size_t i = 0; i < ITERS; i++) {
      acc = acc + at_labeled(a, "a[i]", Integer((int)(i % SZ) + 1));
    }
    return get_val(acc);
  });

  double tagged_ms = time_it("tagged   (at_tagged<Expr>, NTTP)", [&]() {
    Double acc(0.0);
    for (std::size_t i = 0; i < ITERS; i++) {
      acc = acc + at_tagged<"a[i]">(a, Integer((int)(i % SZ) + 1));
    }
    return get_val(acc);
  });

  double ctx_ms = time_it("ctx      (at_copy + thread_local label store)", [&]() {
    Double acc(0.0);
    for (std::size_t i = 0; i < ITERS; i++) {
      g_current_expr = "a[i]";
      acc = acc + at_copy(a, Integer((int)(i % SZ) + 1));
    }
    return get_val(acc);
  });

  std::cout << "\ncopy/baseline:     " << (copy_ms / baseline_ms) << "x\n";
  std::cout << "ctx/baseline:      " << (ctx_ms / baseline_ms) << "x\n";
  std::cout << "trycatch/baseline: " << (trycatch_ms / baseline_ms) << "x\n";
  std::cout << "labeled/baseline:  " << (labeled_ms / baseline_ms) << "x\n";
  std::cout << "tagged/baseline:   " << (tagged_ms / baseline_ms) << "x\n";
}
