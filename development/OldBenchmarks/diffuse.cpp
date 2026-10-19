#include <Rcpp.h>
// [[Rcpp::depends(ast2ast)]]
// [[Rcpp::plugins(cpp2a)]]
#include "etr.hpp"
#include <chrono>

using clock_type = std::chrono::steady_clock;

// [[Rcpp::export]]
SEXP diffuse_rcpp(SEXP nxSEXP, SEXP nySEXP, SEXP dxSEXP, SEXP dySEXP, SEXP dtSEXP, SEXP kSEXP, SEXP stepsSEXP) {
  etr::Integer nx(etr::CheckSEXP<etr::Integer>(nxSEXP, "nx"));
  etr::Integer ny(etr::CheckSEXP<etr::Integer>(nySEXP, "ny"));
  etr::Integer dx(etr::CheckSEXP<etr::Integer>(dxSEXP, "dx"));
  etr::Integer dy(etr::CheckSEXP<etr::Integer>(dySEXP, "dy"));
  etr::Double dt(etr::CheckSEXP<etr::Double>(dtSEXP, "dt"));
  etr::Double k(etr::CheckSEXP<etr::Double>(kSEXP, "k"));
  etr::Integer steps(etr::CheckSEXP<etr::Integer>(stepsSEXP, "steps"));
  etr::Array<etr::Double, etr::Buffer<etr::Double>> temp;
  std::function<void(etr::Array<etr::Double, etr::Buffer<etr::Double>>& temp, etr::Integer& nx, etr::Integer& ny)>apply_boundary_conditions;
  std::function<void(etr::Array<etr::Double, etr::Buffer<etr::Double>>& temp, etr::Integer& nx,
                     etr::Integer& ny, etr::Integer& dx, etr::Integer& dy, etr::Double& dt, etr::Double& k)>update_temperature;

  std::chrono::nanoseconds t_boundary{0};
  std::chrono::nanoseconds t_index_prep{0};
  std::chrono::nanoseconds t_laplacian{0};
  std::chrono::nanoseconds t_assign{0};

  temp = etr::matrix(etr::Double(0.0), nx, ny);
  etr::subset(temp, etr::idiv(nx, etr::Integer(2)), etr::idiv(ny, etr::Integer(2))) = etr::Double(100.0);

  apply_boundary_conditions = [&]( etr::Array<etr::Double, etr::Buffer<etr::Double>>& temp, etr::Integer& nx, etr::Integer& ny ) -> void {
    auto t0 = clock_type::now();
    etr::subset(temp, etr::Integer(1), etr::Logical(true)) = etr::Double(0.0);
    etr::subset(temp, nx, etr::Logical(true)) = etr::Double(0.0);
    etr::subset(temp, etr::Logical(true), etr::Integer(1)) = etr::Double(0.0);
    etr::subset(temp, etr::Logical(true), ny) = etr::Double(0.0);
    t_boundary += clock_type::now() - t0;
    return(etr::Evaluate());
  };
  update_temperature = [&]( etr::Array<etr::Double, etr::Buffer<etr::Double>>& temp, etr::Integer& nx, etr::Integer& ny, etr::Integer&
                          dx, etr::Integer& dy, etr::Double& dt, etr::Double& k ) -> void {
      etr::Array<etr::Integer, etr::Buffer<etr::Integer>> i;
      etr::Array<etr::Integer, etr::Buffer<etr::Integer>> j;
      etr::Array<etr::Double, etr::Buffer<etr::Double>> laplacian;

      auto t0 = clock_type::now();
      i = etr::colon(etr::Integer(2), (nx - etr::Integer(1)));
      j = etr::colon(etr::Integer(2), (ny - etr::Integer(1)));
      auto t1 = clock_type::now();
      t_index_prep += t1 - t0;

      laplacian = (etr::subset(temp, i + etr::Integer(1), j) - etr::Double(2.0) * etr::subset(temp, i, j) +
        etr::subset(temp, i - etr::Integer(1), j)) /
        etr::power(dx, etr::Double(2.0)) +
        (etr::subset(temp, i, j + etr::Integer(1)) - etr::Double(2.0) * etr::subset(temp, i, j) + etr::subset(temp, i, j - etr::Integer(1))) /
        etr::power(dy, etr::Double(2.0));
      auto t2 = clock_type::now();
      t_laplacian += t2 - t1;

      etr::subset(temp, i, j) = etr::subset(temp, i, j) + k * dt * laplacian;
      auto t3 = clock_type::now();
      t_assign += t3 - t2;

      return(etr::Evaluate());
    };
  {
    auto step__bound__ = etr::length_seq(steps);
    for(etr::Integer step = 1; step <= step__bound__; step = step + etr::Integer(1)) {
      apply_boundary_conditions(temp, nx, ny);
      update_temperature(temp, nx, ny, dx, dy, dt, k);
    }
  }

  // Floor check: raw Buffer<Double>::get/set, no subset()/expression-tree
  // involved at all, over an equally-sized buffer for the same number of
  // "steps" sweeps -- isolates the cost inherent to element access itself
  // from whatever the subset()/BinaryOp tree layers on top of it.
  std::chrono::nanoseconds t_raw{0};
  {
    const std::size_t inner_n = static_cast<std::size_t>((nx.val - 2) * (ny.val - 2));
    etr::Array<etr::Double, etr::Buffer<etr::Double>> a(etr::SI{inner_n});
    etr::Array<etr::Double, etr::Buffer<etr::Double>> b(etr::SI{inner_n});
    for (std::size_t idx = 0; idx < inner_n; idx++) a.set(idx, static_cast<double>(idx));

    auto t0 = clock_type::now();
    for (int rep = 0; rep < steps.val; rep++) {
      for (std::size_t idx = 0; idx < inner_n; idx++) {
        b.set(idx, a.get(idx));
      }
    }
    t_raw = clock_type::now() - t0;
  }

  // Bisect: isolate (1) one subset() read composed with one trivial op vs
  // (2) the same trivial op on a plain array with no subset() involved --
  // tells us whether SubsetView itself or BinaryOperation composition depth
  // (or neither, in which case it's specifically the 6-deep nesting) is the
  // multiplier above the raw floor.
  std::chrono::nanoseconds t_subset_op{0};
  std::chrono::nanoseconds t_plain_op{0};
  {
    const std::size_t inner_n = static_cast<std::size_t>((nx.val - 2) * (ny.val - 2));

    etr::Array<etr::Double, etr::Buffer<etr::Double>> temp_local = etr::matrix(etr::Double(0.0), nx, ny);
    for (std::size_t idx = 0; idx < temp_local.size(); idx++) temp_local.set(idx, static_cast<double>(idx));
    etr::Array<etr::Integer, etr::Buffer<etr::Integer>> i_local = etr::colon(etr::Integer(2), (nx - etr::Integer(1)));
    etr::Array<etr::Integer, etr::Buffer<etr::Integer>> j_local = etr::colon(etr::Integer(2), (ny - etr::Integer(1)));
    etr::Array<etr::Double, etr::Buffer<etr::Double>> c(etr::SI{inner_n});

    auto t0 = clock_type::now();
    for (int rep = 0; rep < steps.val; rep++) {
      c = etr::subset(temp_local, i_local, j_local) + etr::Double(0.0);
    }
    t_subset_op = clock_type::now() - t0;

    etr::Array<etr::Double, etr::Buffer<etr::Double>> a(etr::SI{inner_n});
    for (std::size_t idx = 0; idx < inner_n; idx++) a.set(idx, static_cast<double>(idx));
    etr::Array<etr::Double, etr::Buffer<etr::Double>> d(etr::SI{inner_n});

    auto t1 = clock_type::now();
    for (int rep = 0; rep < steps.val; rep++) {
      d = a + etr::Double(0.0);
    }
    t_plain_op = clock_type::now() - t1;
  }

  // Bisect further: is the ~32ms SubsetView tax construction cost (a fresh
  // heap allocation per dimension every subset() call, since i/j's size
  // never equals the full dim extent so the direct-alias branch never
  // fires) or per-element access cost? (A) calls subset() 500 times and
  // discards the result -- pure construction cost, no element access at
  // all. (B) constructs the SubsetView ONCE, then reuses it for 500 reps
  // of the same op+assign -- pure per-element access cost, construction
  // paid only once.
  std::chrono::nanoseconds t_construct_only{0};
  std::chrono::nanoseconds t_access_only{0};
  std::size_t sink = 0;
  {
    const std::size_t inner_n = static_cast<std::size_t>((nx.val - 2) * (ny.val - 2));

    etr::Array<etr::Double, etr::Buffer<etr::Double>> temp_local = etr::matrix(etr::Double(0.0), nx, ny);
    for (std::size_t idx = 0; idx < temp_local.size(); idx++) temp_local.set(idx, static_cast<double>(idx));
    etr::Array<etr::Integer, etr::Buffer<etr::Integer>> i_local = etr::colon(etr::Integer(2), (nx - etr::Integer(1)));
    etr::Array<etr::Integer, etr::Buffer<etr::Integer>> j_local = etr::colon(etr::Integer(2), (ny - etr::Integer(1)));

    // (A) construction only, result discarded (sink prevents dead-code elimination)
    auto t0 = clock_type::now();
    for (int rep = 0; rep < steps.val; rep++) {
      sink += etr::subset(temp_local, i_local, j_local).size();
    }
    t_construct_only = clock_type::now() - t0;

    // (B) construct once, reuse for the per-element op+assign 500 times
    etr::Array<etr::Double, etr::Buffer<etr::Double>> c(etr::SI{inner_n});
    auto sv = etr::subset(temp_local, i_local, j_local);
    auto t1 = clock_type::now();
    for (int rep = 0; rep < steps.val; rep++) {
      c = sv + etr::Double(0.0);
    }
    t_access_only = clock_type::now() - t1;
  }

  auto ms = [](std::chrono::nanoseconds d) { return std::chrono::duration<double, std::milli>(d).count(); };
  Rcpp::Rcout << "apply_boundary_conditions: " << ms(t_boundary)         << " ms\n";
  Rcpp::Rcout << "index prep (i, j):         " << ms(t_index_prep)       << " ms\n";
  Rcpp::Rcout << "laplacian expression:      " << ms(t_laplacian)        << " ms\n";
  Rcpp::Rcout << "temp[i,j] <- assignment:   " << ms(t_assign)           << " ms\n";
  Rcpp::Rcout << "raw get/set (no subset):   " << ms(t_raw)              << " ms\n";
  Rcpp::Rcout << "single subset + op:        " << ms(t_subset_op)        << " ms\n";
  Rcpp::Rcout << "plain array + op:          " << ms(t_plain_op)         << " ms\n";
  Rcpp::Rcout << "subset construction only:  " << ms(t_construct_only)   << " ms (sink=" << sink << ")\n";
  Rcpp::Rcout << "subset access only:        " << ms(t_access_only)      << " ms\n";

  return(etr::Cast(temp));
}
