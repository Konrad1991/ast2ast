# Hessian

Status (2026-10-21): steps 1-4 done (etr_bits/Hessian.hpp, test_hessian.R,
full suite green); next: step 5 (block ops). Branch `dev-1.2`.

Goal: `H = d2 f / dx2` of a scalar `f`, exact (no finite differences).
Use cases: standard errors / confidence intervals after a likelihood fit
(`solve(H)`), Newton steps, later Laplace approximation (random effects).

## Options

1. Forward-over-reverse on the existing tape (recommended)
   - record `f(x)` once as for `deriv()` (`derivative = "reverse"`)
   - per column j: tangent forward sweep over the tape (`val_dot`, seed `e_j`),
     then a second-order reverse sweep (`adj`, `adj_dot`); `adj_dot` of x = `H[, j]`
   - f runs once, then n tape sweeps; no new scalar type on the R side
   - work: tangent + second-order rule for every `ROp` and every `BlockOp`
     (MatMul, solve, chol, det, crossprod, trisolve, ...)
   - branches are fixed at x (same as the reverse gradient)
2. Hyper-dual numbers (forward-over-forward)
   - new scalar type (`val, d1, d2, d12`), new `real_type`
   - every block op needs a specialisation, as for `Dual`
   - n (n + 1) / 2 runs of f
3. Finite differences of the reverse gradient
   - trivial, approximate (~1e-7); only as fallback

Option 1 scales best and keeps the translation side unchanged.

## Sketch (option 1)

Tape arrays added for the Hessian sweeps: `val_dot`, `adj_dot` (allocated only
by `hessian()`). Scalar rules, e.g. `c = a * b`:

```
forward:  c_dot = a_dot * b + a * b_dot
reverse:  a_adj     += c_adj * b
          a_adj_dot += c_adj_dot * b + c_adj * b_dot
```

Unary `c = g(a)`: `a_adj_dot += c_adj_dot * g'(a) + c_adj * g''(a) * a_dot`.

## Steps

1. tangent forward sweep for the scalar `ROp`s
2. second-order reverse sweep for the scalar `ROp`s
3. `hessian(f, x[, data])` and `hvp(f, x, v[, data])`: C++ driver + registry
   entries; block ops not done yet -> runtime error "not supported in hessian yet"
4. tests vs finite differences and analytic Hessians (Rosenbrock, logistic
   regression negative log-likelihood), NA handling
5. block ops one by one: MatMul, CrossProd, TCrossProd -> Solve, BackSolve,
   ForwardSolve -> Chol, Det
6. later: sparse Hessian (colouring) for random effects

Done when: steps 1-5, `hessian()` / `hvp()` documented (own short vignette), NEWS entry.

## Decisions (Konrad, 2026-10-21)

- option 1 (forward-over-reverse); option 2 rejected (too much work)
- syntax like `jacobian`: `hessian(f, x[, data])`
- also in plain translations, as `jacobian` (25ed258, only on `sundials` / `dev-1.2`)

- block ops in v1, ordered easy -> hard (below)
- `hvp(f, x, v)` exposed too

## Notes

- effort per `BlockOp::Kind`:
  - MatMul, CrossProd, TCrossProd: easy (bilinear, a few extra dgemms)
  - Solve, BackSolve, ForwardSolve: medium (reuse the LU / triangle,
    `X_dot = A^-1 (B_dot - A_dot X)`)
  - Chol, Det: hardest (second-order rules for the factorisation)
- `hvp(f, x, v)`: Hessian-vector product in one sweep (= one column with seed v);
  free internally, only worth exposing for large n (Newton-CG, Laplace traces)
