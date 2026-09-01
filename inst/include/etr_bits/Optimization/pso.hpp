#ifndef PSO_ETR_HPP
#define PSO_ETR_HPP

#ifndef STANDALONE_ETR
#include <R_ext/Random.h>
#include <Rmath.h>
#endif

namespace etr {

namespace pso_internally {

template<typename T> inline auto extract_boundaries(const T& obj) {
  if constexpr (IsScalarLike<Decayed<T>>) {
    return c(obj);
  } else if  constexpr (IsArray<Decayed<T>>) {
    return obj;
  } else {
    ass<"Invalid argument for boundary">(false);
  }
}

template<typename T> inline auto extract_scalars(const T& obj) {
  if constexpr (IsScalarLike<Decayed<T>>) {
    return obj;
  } else if  constexpr (IsArray<Decayed<T>>) {
    ass<"Vector with length 1 expected">(obj.size() == 1);
    return obj.get(0);
  } else {
    ass<"Invalid argument for boundary">(false);
  }
}

} // end pso_internally

template<typename F, typename Arg1, typename Arg2, typename Arg3, typename Arg4, typename Arg5, typename Arg6>
inline auto pso(
  const F& f,
  const Arg1& lb_, const Arg2& ub_,
  const Arg3& ngen_, const Arg4 npop_,
  const Arg5 error_threshold_,
  const Arg6& global_) {
  using R = return_type_t<F>;
  Array<R, Buffer<R>> lb = pso_internally::extract_boundaries(lb_);
  Array<R, Buffer<R>> ub = pso_internally::extract_boundaries(ub_);
  Integer ngen = pso_internally::extract_scalars(ngen_);
  Integer npop = pso_internally::extract_scalars(npop_);
  Integer npar = Integer(lb.size());
  const std::size_t npop_t = static_cast<std::size_t>(get_val(npop));
  const std::size_t npar_t = static_cast<std::size_t>(get_val(npar));
  Double error_threshold = pso_internally::extract_scalars(error_threshold_);
  Logical  global= pso_internally::extract_scalars(global_);
  ass<"Length mismatch of lower and upper boundary">(lb.size() == ub.size());
  ass<"the boundaries have to have at least one value">(lb.size() >= 1);
  for (std::size_t i = 0; i < lb.size(); i++) {
    if (get_val(lb.get(i)) > get_val(ub.get(i))) {
      ass<"The lower boundary is not smaller then the upper boundary for at least one value">(false);
    }
  }
  ass<"npop has to be at least 5">(npop >= Integer(5));
  ass<"ngen has to be at least 10">(ngen >= Integer(10));
  Array<R, Buffer<R>> swarm(SI{npop_t * npar_t});
  swarm.dim = std::vector<std::size_t>{npar_t, npop_t};
  Array<R, Buffer<R>> v(SI{npop_t * npar_t});
  v.dim = std::vector<std::size_t>{npar_t, npop_t};
  Array<R, Buffer<R>> swarm_bests = numeric(npop);
  Array<R, Buffer<R>> swarm_errors = numeric(npop);
  const Double initial_cog = Double(2.5);
  const Double final_cog = Double(0.5);
  const Double initial_soc = Double(0.5);
  const Double final_soc = Double(2.5);
  Double w = Double(0.5);
  const Double w_max = Double(0.9);
  const Double w_min = Double(0.4);

  const auto runif = [](const double l, const double u) { return Rf_runif(l, u); };

  GetRNGstate();

  for (std::size_t i = 0; i < npop_t; i++) {
    for (std::size_t j = 0; j < npar_t; j++) {
      swarm.set(i*npar_t + j, runif(get_val(lb.get(j)), get_val(ub.get(j))));
    }
    Array<R, Borrow<R, BorrowTrait>> particle(
      &swarm.d.p_val[i * npar_t], npar_t, std::vector<std::size_t>{npar_t}
    );
    swarm_errors.set(i, f(particle));
    swarm_bests.set(i, swarm_errors.get(i));
  }
  
  Double global_best_error = swarm_errors.get(0);
  Integer global_best = Integer(0);
  for (std::size_t i = 1; i < npop_t; i++) {
    if (static_cast<bool>(global_best_error > swarm_errors.get(i))) {
      global_best_error = swarm_errors.get(i);
      global_best = Integer(static_cast<int>(i));
    }
  }
  Integer k = Integer(3);
  const std::size_t k_t = static_cast<std::size_t>(get_val(k));

  [[maybe_unused]] Array<Integer, Buffer<Integer>> neighberhood(SI{k_t * npop_t});
  neighberhood.dim = std::vector<std::size_t>{k_t, npop_t};
  std::vector<int> pool(npop_t);
  const auto calc_neighberhood = [&]() {
    for (std::size_t c = 0; c < npop_t; c++) {
      for (std::size_t r = 0; r < k_t; r++) neighberhood.set(c * k_t + r, Integer(-1));
      const std::size_t nn =
        static_cast<std::size_t>(R_unif_index(static_cast<double>(k_t))) + 1;
      std::iota(pool.begin(), pool.end(), 0);
      std::size_t remaining = npop_t;
      for (std::size_t r = 0; r < nn; r++) {
        const std::size_t pick =
          static_cast<std::size_t>(R_unif_index(static_cast<double>(remaining)));
        neighberhood.set(c * k_t + r, Integer(pool[pick]));
        pool[pick] = pool[--remaining];
      }
    }
  };
  calc_neighberhood();

  const auto correct_below_lb = [&](double* target) {
    for (std::size_t j = 0; j < npar_t; j++) {
      const double thr = get_val(lb.get(j));
      if (target[j] < thr) target[j] = thr;
    }
  };
  const auto correct_above_ub = [&](double* target) {
    for (std::size_t j = 0; j < npar_t; j++) {
      const double thr = get_val(ub.get(j));
      if (target[j] > thr) target[j] = thr;
    }
  };

  for (std::size_t idx = 0; idx < npar_t * npop_t; idx++) v.set(idx, R(0.0));
  Array<R, Buffer<R>> swarm_best_params(SI{npar_t * npop_t});
  swarm_best_params.dim = std::vector<std::size_t>{npar_t, npop_t};
  for (std::size_t idx = 0; idx < npar_t * npop_t; idx++)
    swarm_best_params.set(idx, R(swarm.d.p_val[idx]));
  Array<R, Buffer<R>> global_best_vec(SI{npar_t});
  global_best_vec.dim = std::vector<std::size_t>{npar_t};
  for (std::size_t d = 0; d < npar_t; d++)
    global_best_vec.set(d, R(swarm.d.p_val[static_cast<std::size_t>(get_val(global_best)) * npar_t + d]));

  Integer convergence_check = Integer(0);
  Integer no_improvement = Integer(0);
  const double ngen_d = static_cast<double>(get_val(ngen));
  const bool use_global = static_cast<bool>(global);

  for (int iter = 1; iter < static_cast<int>(get_val(ngen)); iter++) {
    if (iter == 1 || get_val(convergence_check) != 0) calc_neighberhood();

    w = Double(get_val(w_max) - iter * (get_val(w_max) - get_val(w_min)) / ngen_d);
    const double cog = get_val(initial_cog)
      - (get_val(initial_cog) - get_val(final_cog)) * (iter + 1) / ngen_d;
    const double soc = get_val(initial_soc)
      - (get_val(initial_soc) - get_val(final_soc)) * (iter + 1) / ngen_d;

    for (std::size_t i = 0; i < npop_t; i++) {
      double* pos   = &swarm.d.p_val[i * npar_t];
      double* vel   = &v.d.p_val[i * npar_t];
      double* pbest = &swarm_best_params.d.p_val[i * npar_t];

      // best neighbour by personal-best error; its current position is the social pull
      std::size_t best_nb = i;
      double best_nb_err = get_val(swarm_bests.get(i));
      for (std::size_t r = 0; r < k_t; r++) {
        const int nb = get_val(neighberhood.get(i * k_t + r));
        if (nb < 0) continue;
        const double e = get_val(swarm_bests.get(static_cast<std::size_t>(nb)));
        if (e < best_nb_err) { best_nb_err = e; best_nb = static_cast<std::size_t>(nb); }
      }
      const std::size_t social = use_global
        ? static_cast<std::size_t>(get_val(global_best)) : best_nb;
      const double* social_pos = &swarm.d.p_val[social * npar_t];

      const double r1 = unif_rand();
      const double r2 = unif_rand();
      for (std::size_t d = 0; d < npar_t; d++) {
        vel[d] = get_val(w) * vel[d]
               + cog * r1 * (pbest[d]      - pos[d])
               + soc * r2 * (social_pos[d] - pos[d]);
        pos[d] += vel[d];
      }
      correct_below_lb(pos);
      correct_above_ub(pos);

      Array<R, Borrow<R, BorrowTrait>> particle(
        pos, npar_t, std::vector<std::size_t>{npar_t}
      );
      const double err = get_val(f(particle));
      const bool ok = std::isfinite(err);

      if (ok && err < get_val(swarm_bests.get(i))) {
        swarm_bests.set(i, R(err));
        for (std::size_t d = 0; d < npar_t; d++) pbest[d] = pos[d];
      }
      if (ok && err < get_val(global_best_error)) {
        global_best = Integer(static_cast<int>(i));
        for (std::size_t d = 0; d < npar_t; d++) global_best_vec.set(d, R(pos[d]));
        global_best_error = Double(err);
        no_improvement = Integer(0);
      } else {
        no_improvement = no_improvement + Integer(1);
      }
      convergence_check = convergence_check + Integer(1);
    }

    if (static_cast<bool>(global_best_error < error_threshold)) break;
  }

  PutRNGstate();
  return global_best_vec;
}

} // namespace etr
#endif
