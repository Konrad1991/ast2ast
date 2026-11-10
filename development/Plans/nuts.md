# NUTS / HMC as built-in function (release 1.2)

Decided 2026-11-05 (Konrad): built-in `nuts()` in C++ (like `lbfgsb()`),
part of 1.2 (changed from 1.3 the same day); 1.2 cleanup (NEWS, vignette, -Wall) afterwards.

Status quo:
- gradient / value_and_gradient / hessian; cvode + ida fwd/adjoint/Hessian
- hand-written NUTS in the DSL: development/examples/ode/nuts_lotka_volterra.R
  (median 9.7 ESS/s vs Stan 7.2) -> reference implementation
- sparse Hessian (ode_sensitivities.md step 6) not needed -> Laplace

## Steps
1. failure handling without DSL tryCatch
   - RAII tape guard (try_catch.md step 1)
   - `etr::numeric_error : std::runtime_error`, thrown by cvode/ida failures
   - nuts catches numeric_error only -> divergent, reject;
     bounds checks / stop() propagate to R
   - later also for lbfgsb (line search)
   Status 2026-11-05: DONE (untested by Konrad). etr::TapeGuard (Scalars.hpp)
   replaces all 7 manual rewinds; etr::numeric_error + numeric_ass (Types.hpp)
   for the 14 integration / IDACalcIC / backward failures in a2a_sundials.hpp;
   C++ test at the end of test_tape_reuse (src/tests_Jac.cpp).
2. API: `nuts(log_post, theta0, data, options)`
3. chains: single thread is the base (chains sequentially);
   pmap optional (`nuts_options(n_threads = 1L)`, > 1 -> pmap);
   one seed per chain, identical results in both variants
4. output: draws + diagnostics (divergent, treedepth, step size, n_leapfrog);
   ESS / Rhat via posterior package
5. later: constrained params (transforms + log-Jacobian), dense mass matrix

Status steps 2-4 2026-11-05: DONE (Claude, scratch install; full suite 1989 green).
- etr_bits/Sampling/Nuts.hpp: nuts_options, nuts_result, nuts_detail (Chain, Windows,
  StepAdaptation, Evaluator); multinomial NUTS as the DSL reference, Stan's init_stepsize
  and windowed warmup (75 / 25 doubling / 50); chain k: seeds seed + 2k, seed + 2k + 1
- R: nuts / nuts_options in FunctionRegistry.R (check_scalar_ad_fn shared with gradient &
  co.; f must be thread safe), types in TypeParser.R; tests inst/tinytest/test_nuts.R
- LV (development/examples/ode/nuts_lotka_volterra.R model): 1 chain 33 s (DSL: 30 s),
  ~40 leapfrog / it, Rhat 1.00, ESS > 1100 for 4 x 1500
- open: 4 chains on 4 threads take 65 s per chain -> RcppThread pins workers to logical
  CPUs 0..3 = 2 physical cores (as pmap, project_pmap_scaling_causes)
- docs DONE 2026-11-05: vignettes/Sampling.Rmd, translate.Rd "Sampling", NEWS (1.2)
- example: development/examples/ode/nuts_builtin_lotka_volterra.R (4 chains, 64 s wall /
  256 s CPU, min ESS 1166, Rhat 1.00, 0 divergent; Stan per chain ~8 ESS/s, nuts ~9-10)

## Decisions 2026-11-05 (Konrad)
- no separate hmc(): NUTS = HMC with automatic trajectory length;
  hand-written HMC stays an example (development/examples/ode/hmc_lotka_volterra.R)
- Laplace not in 1.2:
  - a) posterior ~ N(MAP, H^-1) via lbfgsb + hessian: only as example in development/
    (LV, compare with the nuts draws = ode_performance.md C0); no laplace() builtin for now
  - b) random effects integrated out (TMB-like; nested optimisation, d log det H =
    3rd order, sparse Hessian) -> 1.3
- 1.2 is closed after: nuts docs + NEWS, ODE sensitivity vignette, decision on SMT
  pinning (keep + document vs own std::thread), -Wall warnings, cvode no-active-input bug,
  roadmap update

## Idea: diagnostic warnings (later, not 1.2)
- print.nuts_result warns like Stan on:
  - Rhat > 1.01 / low ESS
  - step sizes differing strongly between chains (e.g. > 5x)
  - divergent transitions, max treedepth hit
- motivating case 2026-11-06: LV example, prior sd 2.35 on log scale, random theta0
  -> 3 of 4 chains in a local mode (lp ~ -410, a ~ 4.5), step sizes
  0.007 / 0.013 / 0.075 / 0.015, pooled mean a = 4.09 (truth 1.1);
  sd ~ 1 runs fine, sd 4.7 was fast too (mode hit depends on start)
```r
s <- (log(100) - log(0.01)) / (2 * 1.96)
log_prior <- sum(dnorm(theta, 0, s, log = TRUE))
theta0 <- rnorm(4, 0, 1)
```

## Verification
- LV model vs Stan (development/examples/ode/hmc_vs_stan.R): ESS/s, 0 divergences
- tinytest: standard normal / correlated normal moments
- n_threads = 1 and > 1 give identical draws
