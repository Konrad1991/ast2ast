# Roadmap: large ODE systems and loss functions

## Direction

1. numeric algorithms (compiled R DSL)
2. numeric methods: nonlinear equations, uniroot, nnls, later KINSOL
3. optimization framework -> gradients (reverse AD)
4. ODE/DAE solving via sundials -> Jacobians (forward AD)

AD is the common core. Parallelism (`pmap`) is part of all points.
Safety (bounds checks, `current_line()`) is an existing pillar.

Pitch: "Write numerical models in R; ast2ast compiles them and derives
gradients and Jacobians for optimizers and stiff solvers."

ast2ast takes care that its output can be used by C and C++ libraries.

## Versions (decided 2026-10-21)

- 1.1: TODO.md A-C (bugs, small issues, docs)
- 1.1.1: sundials as dependency (cvode, ida) incl. TODO.md D2 options
- 1.2: new areas (below) + CVODES, IDAS, ARKODE; branch `dev-1.2` (from `sundials`, 2026-10-21)

## Short term

1. sundials package -> CRAN
2. ast2ast 1.1 -> CRAN (pmap, RcppThread)
3. sundials as dependency of ast2ast; full dense-matrix functionality

Why not deSolve:
- extracting information from the DLL
- special handling of the error system (C++ code)
- hard to ensure banded is not used
- overall overcomplicated

## Decision 2026-10-08: classes, not types

Banded/sparse are result classes (like the result of `uniroot`), not DSL types:
- construct, field access, write values, pass to solver, return to R
- no operators, no type promotion, no mixed ops, no AD through sparse linalg

Focus: solve large ODE systems and large loss functions.
Goal: run Jan's model (see test case) in ast2ast.

Chain:
1. sundials -> CRAN (+ IDA)
2. ast2ast uses sundials, dense -> Jan's model as baseline
3. static colouring -> `jacobian_banded`
4. dynamic colouring -> `jacobian_sparse` + KLU
5. Jan's model fast -> demo

## Colouring

Prerequisite for `jacobian_banded`/`jacobian_sparse` (compressed forward AD).
- banded: static, `mu + ml + 1` colours
- sparse: detect pattern -> greedy colouring -> fill CSC

Where it fails:
- dense rows -> n colours, no gain. Jan's border (global pools)!
  ```r
  ydot[[n]] <- sum(y)   # one dense row -> forward colouring needs n colours
  ```
  -> dense rows via reverse sweep, rest via coloured forward (bidirectional)
- pattern depends on y (`if (y[[i]] > 0) ...`) -> missing entries, silent
  -> union of both branches (static) or runtime check + `current_line()` error
- accidental zeros (`a * y`, a = 0 at t0) -> detect pattern structurally (bitsets), not numerically
- no AD possible (uniroot/lbfgsb in RHS) -> coloured finite differences

## Container vs. computation

`jacobian_banded`/`jacobian_sparse` = storage + pattern only. Filled by:
1. coloured forward AD (default)
2. bidirectional (dense rows)
3. coloured finite differences
4. user by hand

Pattern is fixed at construction; writes outside -> error.
Why: KLU reuses the symbolic factorization; fits "class, not type" (no insertion).

```r
J <- jac_sparse(n, n, i = rows, j = cols)   # pattern fixed here (triplet)
J[[1, 1]] <- -k1 * y[[2]]
J[[5, 7]] <- 1   # not in pattern -> error via current_line()

J <- jac_banded(n, mu, ml)   # outside the band -> error
```

Later, not v1: first call sets the pattern, later calls must match.

Storage formats: see `new_buffer_types.md`.

## Test case: E. coli translation model

DaRUS doi:10.18419/darus-4628 (MATLAB, `ode15s` + singular mass matrix -> index-1 DAE).
Code is dense throughout; Jacobian structure is band (12-codon window) + border (global pools).

1. dense port, baseline; must match the MATLAB results
   - needs: IDA in sundials, missing data files (xlsx, .mat, FASTA)
   - `strmatch` codon lookup -> precomputed `codon_idx` in R
   - `matrix_N * c_Codon` -> `c_Codon[codon_idx]`
2. sparse step: colouring, `jacobian_sparse`, KLU

## Jacobian types

- `jacobian` -> matrix (exists)
- `jacobian_banded` -> banded matrix (`SUNMatrix_Band`, pattern = `mu`/`ml`)
- `jacobian_sparse` -> sparse matrix (`SUNMatrix_Sparse`, CSC, KLU)
- use `sunindextype` (sundials pkg is int64); fill `SUNMatrix` directly

Not opaque: shared like `new_type`, fields accessible (`J$data`, `J$indexvals`, ...).

Usage in the solver:
- `cvode(..., jacobian = "dense" | "banded" | "sparse")` -> generated
- `cvode(..., jacobian = jac_user_defined)`:
  ```r
  jac_user_defined <- function(...) {
    jacobian(ode_system, "banded")
  }
  ```

Krylov (matrix-free, no Jacobian type):
- `cvode(..., jacobian = "krylov")` -> SPGMR, Jv via finite differences (SUNDIALS)
- Jv via forward AD: one dual sweep, exact
- user-defined Jv function -> returns vector
- needs preconditioner for stiff systems; start with `CVBandPrecInit`

## New areas (decided 2026-10-20)

Prerequisite: RNGs + distributions (`rng_distributions.md`).

Order:
1. Hessian (forward-over-reverse AD)
   - standard errors / confidence intervals after a likelihood fit, Newton
2. HMC / NUTS sampler
   - needs only the gradient (reverse AD) + RNG
3. Random-effects models
   - user writes the joint negative log-likelihood in R
   - random effects integrated out via Laplace approximation
     (AD gradient + Hessian of the inner problem)
   - large models need a sparse Hessian (see banded/sparse above)
4. ODE events (CVODE rootfinding)
   - only for state-dependent events; known times (spline knots,
     dosing) -> integrate piecewise and restart

Later / maybe:
- stochastic simulation (Gillespie, Euler-Maruyama)
- method of lines for PDEs (-> banded Jacobian)
- constrained optimization (augmented Lagrangian around `lbfgsb()`)
- quadrature / FFT via R's C API (`Rdqags`, `fft_work`),
  only worth it inside compiled loops
