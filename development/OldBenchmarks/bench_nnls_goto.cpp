#define STANDALONE_ETR

#include "../../inst/include/etr.hpp"
#include "../nnls_goto.hpp"
#include <chrono>
#include <iostream>
#include <random>
#include <cmath>

using namespace etr;
using clock_type = std::chrono::steady_clock;

// Random NNLS test problem: A (m x n) uniform, x_true >= 0 (some entries
// forced to 0 so the active-set logic actually has work to do), b = A*x_true.
static void make_problem(int m, int n, unsigned seed,
                          std::vector<double>& araw, std::vector<double>& braw) {
  std::mt19937 rng(seed);
  std::uniform_real_distribution<double> unif(-1.0, 1.0);
  araw.resize(static_cast<std::size_t>(m) * n);
  for (auto& v : araw) v = unif(rng);

  std::vector<double> x_true(n, 0.0);
  std::uniform_real_distribution<double> pos(0.0, 2.0);
  for (int j = 0; j < n; ++j) {
    if (j % 2 == 0) x_true[j] = pos(rng);
  }

  braw.assign(m, 0.0);
  for (int j = 0; j < n; ++j) {
    for (int i = 0; i < m; ++i) {
      braw[i] += araw[static_cast<std::size_t>(j) * m + i] * x_true[j];
    }
  }
}

static double ms(std::chrono::nanoseconds d) {
  return std::chrono::duration<double, std::milli>(d).count();
}

int main() {
  struct Size { int m; int n; int reps; };
  std::vector<Size> sizes = {
    {20, 10, 2000},
    {50, 20, 500},
    {100, 40, 100},
    {200, 80, 20},
  };

  for (const auto& sz : sizes) {
    std::vector<double> araw, braw;
    make_problem(sz.m, sz.n, 42, araw, braw);

    Array<Double, Buffer<Double>> A = numeric(Integer(sz.m * sz.n));
    A.dim = std::vector<std::size_t>{static_cast<std::size_t>(sz.m), static_cast<std::size_t>(sz.n)};
    for (std::size_t i = 0; i < araw.size(); ++i) A.set(i, Double(araw[i]));

    Array<Double, Buffer<Double>> b = numeric(Integer(sz.m));
    for (std::size_t i = 0; i < braw.size(); ++i) b.set(i, Double(braw[i]));

    // nnls_goto: DSL-style engine, subset()/at() throughout.
    auto t0 = clock_type::now();
    for (int r = 0; r < sz.reps; ++r) {
      auto x_goto = nnls_goto(A, b);
      (void)x_goto;
    }
    auto t_goto = clock_type::now() - t0;

    // nnls: shipped hand-optimized engine, raw pointer arithmetic.
    auto t1 = clock_type::now();
    for (int r = 0; r < sz.reps; ++r) {
      auto x_core = etr::nnls(A, b);
      (void)x_core;
    }
    auto t_core = clock_type::now() - t1;

    auto x_goto = nnls_goto(A, b);
    auto x_core = etr::nnls(A, b);
    double max_diff = 0.0;
    for (int j = 0; j < sz.n; ++j) {
      double vg = Double(at(x_goto, Integer(j + 1))).val;
      double vc = Double(at(x_core, Integer(j + 1))).val;
      max_diff = std::max(max_diff, std::abs(vg - vc));
    }

    std::cout << "m=" << sz.m << " n=" << sz.n << " reps=" << sz.reps << "\n"
              << "  nnls_goto (subset-based): " << ms(t_goto) << " ms total, "
              << ms(t_goto) / sz.reps << " ms/solve\n"
              << "  nnls (hand-optimized):    " << ms(t_core) << " ms total, "
              << ms(t_core) / sz.reps << " ms/solve\n"
              << "  ratio goto/core:          " << ms(t_goto) / ms(t_core) << "x\n"
              << "  max |x_goto - x_core|:    " << max_diff << "\n\n";
  }

  return 0;
}
