# Stan Math as alternative backend instead of etr (analysis 2026-11-06)

Status: analysis only, no decision. Items marked "verify" are from memory, not checked.

## Question

Generated C++ calls `etr::` today. Could `stan::math` (Stan's C++ library) replace etr,
fully or partly?

## Short answer

- Full replacement: no. etr's core is R semantics + safety, Stan Math has neither.
- Scalar swap (`etr::Array<stan::math::var>`): possible, but little gain,
  high cost (compile time, CRAN, second SUNDIALS).
- Useful: Stan Math as test oracle (development only) and as source of
  algorithms to port (BSD-3, with attribution).

## What Stan Math is

- header-only C++17 library, deps: Eigen, Boost, SUNDIALS, TBB
- on CRAN via StanHeaders (+ RcppEigen, BH, RcppParallel for TBB)
- AD types:
  - `var`: reverse mode, pointer to `vari` in an arena, global tape per thread
    (`ChainableStack`, thread_local)
  - `fvar<T>`: forward mode
  - `fvar<var>` / `fvar<fvar<var>>`: Hessians, HVPs
  - `var_value<Eigen::Matrix>`: matrix as one tape node (SoA)
- content:
  - ~all densities `*_lpdf/_lpmf` + `_rng`, with gradients w.r.t. all args
  - special functions with derivatives (lgamma, digamma, incomplete beta/gamma, Bessel, ...)
  - linalg with specialised adjoints: cholesky_decompose, mdivide_*, eigendecompose_sym,
    matrix_exp, log_determinant, ...
  - ODE: ode_rk45, ode_ckrk45, ode_adams, ode_bdf (CVODES fwd sensitivities),
    ode_adjoint_tol_ctl (CVODES adjoint); dae (IDAS)
  - algebra_solver (Newton/KINSOL, Powell), integrate_1d, reduce_sum, map_rect
  - embedded Laplace approximation (newer releases, verify version)

## What etr has and Stan Math does not

| area | etr | Stan Math |
|---|---|---|
| R semantics | 1-based, recycling, R subsetting (neg./logical idx), dims kept, int/logical types, NA, int overflow -> NA | none (0-based Eigen/std::vector) |
| R memory | borrow: zero-copy views on SEXP | copy into Eigen |
| errors | bounds checks + `current_line()` -> R line | `std::domain_error` with Stan function names |
| Jacobians | coloured forward AD: `jacobian_banded`, `jacobian_sparse` | dense only |
| ODE linear solvers | sundials dense / band (Krylov planned) | dense only -> limit ~ hundreds of states |
| optimisation | lbfgsb, pso, nnls, uniroot | none (only in Stan services, not in Math) |
| parallelism | pmap on RcppThread, identical draws per seed | reduce_sum / map_rect on TBB |
| types | structs (`type()`), layouts, collections | none |
| sampling | `nuts()` built in | NUTS is in Stan services, not in Math |

So the R layer (Array, Buffer, Borrow, Subsetting, type inference) would stay.
Only the scalar and AD layer could be swapped.

## Variants

### A: full replacement (generated code uses Eigen + stan::math directly)

- type inference + code generation rewritten for Eigen types
- R semantics re-implemented on top of Eigen -> what etr already is
- verdict: rewrite of the project, no gain

### B: scalar swap (etr front, Stan AD underneath)

- `ReverseDouble` -> `stan::math::var`, `Dual` -> `fvar<double>`, Hessian -> `fvar<var>`
- etr block ops (matmul, solve, chol, crossprod, trisolve, cvode/ida as AD block)
  must become Stan `vari`s or calls on `Eigen::Map` -> conversions at every block op
- coloured Jacobians, sundials sensitivities, nuts: depend on etr tape internals
  (SoA tape, TAPE_INTERN, ZERO_ID, TapeGuard, rewind) -> rewrite
- gain: densities + special functions with gradients for free
- verdict: high effort, the gain also comes from porting individual functions

### C: Stan Math as optional extra (`stan_*()` functions next to etr)

- e.g. `stan_lgamma`, `stan_beta_lpdf` with conversion ReverseDouble <-> var at the call
- two tapes in one gradient: values + adjoints copied between tapes
- verdict: ugly, two AD systems in one program

### D: development-only test oracle

- standalone C++ (try_stuff.cpp-style) with StanHeaders: same function in etr and Stan,
  compare value, gradient, Hessian
- no CRAN dependency, nothing in DESCRIPTION
- verdict: cheap, useful for AD correctness (cf. project_ad_test_coverage_hardening)

### E: port algorithms (BSD-3, keep copyright notice)

- when a function is needed (lgamma derivative, incomplete gamma, a density),
  read the Stan implementation and port it to etr
- verdict: best gain/cost; license compatible with GPL (>= 2) (verify notice requirements)

## Costs / risks of a Stan dependency (variants B, C)

1. compile time: `translate()` compiles on demand; stan/math.hpp is heavy
   (Stan models typically 20-60 s, several GB RAM, verify) -> poor JIT experience
2. second SUNDIALS: StanHeaders bundles its own SUNDIALS (6.x, verify), ast2ast links
   the sundials package -> two versions in one process, symbol clashes possible
3. CRAN: StanHeaders/rstan version coupling broke repeatedly; Eigen/Boost warnings;
   TBB on macOS; large install size. Already enough CRAN/Mac trouble
   (project_cran_mac_repro_difficulty)
4. threads: Stan tape needs a `ChainableStack` per thread; doable with RcppThread,
   but a second thread_local tape next to etr's
5. errors: Stan messages don't know the R line -> pillar "safety/error quality" weakened
6. control: AD internals (rewind, local tape, block ops, colouring) no longer in our hands

## Performance (measured, development/Plans/ode_performance.md)

- LV gradient through CVODES: Stan 0.47 ms, ast2ast ~0.4 ms -> no speed gain
- NUTS: ast2ast 9.4 ESS/s, Stan 8.1 ESS/s (median 3 seeds)
- Stan's strength: large dense linalg with `var_value<Matrix>`; etr has block ops too.
  Not compared yet (mnist / matmul benchmark would show it).

## Fit with project goals

- motive = understanding + breadth (project_new_areas_priority): own AD is part of that
- roadmap targets large ODEs (banded/sparse, colouring) -> Stan has the dense limit
  that ast2ast wants to overcome
- pitch "write models in R, get gradients/Jacobians" works with etr

## Recommendation

- no Stan backend (A, B, C)
- D: optional test oracle in development/, when AD bugs or new functions appear
- E: Stan Math as reference implementation to port from, function by function

## When to reconsider

- many densities / special functions needed quickly (e.g. GLM/hierarchical models for NUTS)
- AD maintenance becomes the bottleneck
- Stan Math available without bundled SUNDIALS and with fast compile (precompiled header)

## Open questions (Konrad)

- is D worth a small harness now, or only on demand?
- which densities / special functions are next on the list? (decides E)
- measure: current translate() compile time vs a minimal StanHeaders program?
