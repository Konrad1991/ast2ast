#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"
#include <chrono>
#include <iostream>

using namespace etr;

using clock_type = std::chrono::steady_clock;

int main() {
  Integer nx(100);
  Integer ny(100);
  Integer dx(1);
  Integer dy(1);
  Double dt(0.01);
  Double k(0.1);
  Integer steps(500);

  Array<Double, Buffer<Double>> temp;

  std::chrono::nanoseconds t_boundary{0};
  std::chrono::nanoseconds t_index_prep{0};
  std::chrono::nanoseconds t_laplacian{0};
  std::chrono::nanoseconds t_assign{0};

  std::function<void(Array<Double, Buffer<Double>>& temp, Integer& nx, Integer& ny)> apply_boundary_conditions;
  std::function<void(Array<Double, Buffer<Double>>& temp, Integer& nx,
                     Integer& ny, Integer& dx, Integer& dy, Double& dt, Double& k)> update_temperature;

  temp = matrix(Double(0.0), nx, ny);
  subset(temp, idiv(nx, Integer(2)), idiv(ny, Integer(2))) = Double(100.0);

  apply_boundary_conditions = [&](Array<Double, Buffer<Double>>& temp, Integer& nx, Integer& ny) -> void {
    auto t0 = clock_type::now();
    subset(temp, Integer(1), Logical(true)) = Double(0.0);
    subset(temp, nx, Logical(true)) = Double(0.0);
    subset(temp, Logical(true), Integer(1)) = Double(0.0);
    subset(temp, Logical(true), ny) = Double(0.0);
    t_boundary += clock_type::now() - t0;
    return(Evaluate());
  };
  update_temperature = [&](Array<Double, Buffer<Double>>& temp, Integer& nx, Integer& ny, Integer&
                          dx, Integer& dy, Double& dt, Double& k) -> void {
      Array<Integer, Buffer<Integer>> i;
      Array<Integer, Buffer<Integer>> j;
      Array<Double, Buffer<Double>> laplacian;

      auto t0 = clock_type::now();
      i = colon(Integer(2), (nx - Integer(1)));
      j = colon(Integer(2), (ny - Integer(1)));
      auto t1 = clock_type::now();
      t_index_prep += t1 - t0;

      laplacian = (subset(temp, i + Integer(1), j) - Double(2.0) * subset(temp, i, j) +
        subset(temp, i - Integer(1), j)) /
        power(dx, Double(2.0)) +
        (subset(temp, i, j + Integer(1)) - Double(2.0) * subset(temp, i, j) + subset(temp, i, j - Integer(1))) /
        power(dy, Double(2.0));
      auto t2 = clock_type::now();
      t_laplacian += t2 - t1;

      subset(temp, i, j) = subset(temp, i, j) + k * dt * laplacian;
      auto t3 = clock_type::now();
      t_assign += t3 - t2;

      return(Evaluate());
    };
  {
    auto step__bound__ = length_seq(steps);
    for(Integer step = 1; step <= step__bound__; step = step + Integer(1)) {
      apply_boundary_conditions(temp, nx, ny);
      update_temperature(temp, nx, ny, dx, dy, dt, k);
    }
  }

  // Floor check: raw Buffer<Double>::get/set, no subset()/expression-tree
  // involved at all, over an equally-sized buffer for the same number of
  // "steps" sweeps.
  std::chrono::nanoseconds t_raw{0};
  {
    const std::size_t inner_n = static_cast<std::size_t>((nx.val - 2) * (ny.val - 2));
    Array<Double, Buffer<Double>> a(SI{inner_n});
    Array<Double, Buffer<Double>> b(SI{inner_n});
    for (std::size_t idx = 0; idx < inner_n; idx++) a.set(idx, static_cast<double>(idx));

    auto t0 = clock_type::now();
    for (int rep = 0; rep < steps.val; rep++) {
      for (std::size_t idx = 0; idx < inner_n; idx++) {
        b.set(idx, a.get(idx));
      }
    }
    t_raw = clock_type::now() - t0;
  }

  // Bisect: one subset() read + trivial op vs. the same trivial op on a
  // plain array with no subset() involved.
  std::chrono::nanoseconds t_subset_op{0};
  std::chrono::nanoseconds t_plain_op{0};
  {
    const std::size_t inner_n = static_cast<std::size_t>((nx.val - 2) * (ny.val - 2));

    Array<Double, Buffer<Double>> temp_local = matrix(Double(0.0), nx, ny);
    for (std::size_t idx = 0; idx < temp_local.size(); idx++) temp_local.set(idx, static_cast<double>(idx));
    Array<Integer, Buffer<Integer>> i_local = colon(Integer(2), (nx - Integer(1)));
    Array<Integer, Buffer<Integer>> j_local = colon(Integer(2), (ny - Integer(1)));
    Array<Double, Buffer<Double>> c(SI{inner_n});

    auto t0 = clock_type::now();
    for (int rep = 0; rep < steps.val; rep++) {
      c = subset(temp_local, i_local, j_local) + Double(0.0);
    }
    t_subset_op = clock_type::now() - t0;

    Array<Double, Buffer<Double>> a(SI{inner_n});
    for (std::size_t idx = 0; idx < inner_n; idx++) a.set(idx, static_cast<double>(idx));
    Array<Double, Buffer<Double>> d(SI{inner_n});

    auto t1 = clock_type::now();
    for (int rep = 0; rep < steps.val; rep++) {
      d = a + Double(0.0);
    }
    t_plain_op = clock_type::now() - t1;
  }

  // Bisect further: subset() construction cost alone vs. per-element access
  // cost alone (SubsetView built once, reused for 500 reps).
  std::chrono::nanoseconds t_construct_only{0};
  std::chrono::nanoseconds t_access_only{0};
  std::size_t sink = 0;
  {
    const std::size_t inner_n = static_cast<std::size_t>((nx.val - 2) * (ny.val - 2));

    Array<Double, Buffer<Double>> temp_local = matrix(Double(0.0), nx, ny);
    for (std::size_t idx = 0; idx < temp_local.size(); idx++) temp_local.set(idx, static_cast<double>(idx));
    Array<Integer, Buffer<Integer>> i_local = colon(Integer(2), (nx - Integer(1)));
    Array<Integer, Buffer<Integer>> j_local = colon(Integer(2), (ny - Integer(1)));

    auto t0 = clock_type::now();
    for (int rep = 0; rep < steps.val; rep++) {
      sink += subset(temp_local, i_local, j_local).size();
    }
    t_construct_only = clock_type::now() - t0;

    Array<Double, Buffer<Double>> c(SI{inner_n});
    auto sv = subset(temp_local, i_local, j_local);
    auto t1 = clock_type::now();
    for (int rep = 0; rep < steps.val; rep++) {
      c = sv + Double(0.0);
    }
    t_access_only = clock_type::now() - t1;
  }

  auto ms = [](std::chrono::nanoseconds d) { return std::chrono::duration<double, std::milli>(d).count(); };
  std::cout << "apply_boundary_conditions: " << ms(t_boundary)         << " ms\n";
  std::cout << "index prep (i, j):         " << ms(t_index_prep)       << " ms\n";
  std::cout << "laplacian expression:      " << ms(t_laplacian)        << " ms\n";
  std::cout << "temp[i,j] <- assignment:   " << ms(t_assign)           << " ms\n";
  std::cout << "raw get/set (no subset):   " << ms(t_raw)              << " ms\n";
  std::cout << "single subset + op:        " << ms(t_subset_op)        << " ms\n";
  std::cout << "plain array + op:          " << ms(t_plain_op)         << " ms\n";
  std::cout << "subset construction only:  " << ms(t_construct_only)   << " ms (sink=" << sink << ")\n";
  std::cout << "subset access only:        " << ms(t_access_only)      << " ms\n";

  return 0;
}
