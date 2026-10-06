# ODE/DAE API — TODO

Goal: `ode()` / `dae()` in the DSL, wrapping **Sundials** (not our own solver).

## Backend
- Sundials: CVODE (ODE), IDA (DAE), CVODES/IDAS (sensitivities), KLU (sparse).
- How to ship, best first:
  1. `LinkingTo: sundialr` — only if it exposes headers + linkable lib. Verify.
  2. companion pkg `ast2ast.sundials` that vendors Sundials once; `LinkingTo` it.
  3. `SystemRequirements: libsundials-dev` + `configure` fallback (non-CRAN / HPC).
  4. vendored subset, paropt-style (fallback; accept size NOTE).
- KLU/sparse: build-flag gated. dense + band always available.
- FATODE rejected: no DAEs, LS_Solver uses module-global `save` state (not
  thread-safe), large-system path is UMFPACK.

## API sketch
```
ode(rhs, y0, times, params, method="bdf"|"adams", jac=NULL,
    linsol="dense"|"band"|"klu", sens=NULL|"forward"|"adjoint")
dae(res, y0, yp0, times, params, ...)   # residual form -> IDA
```
- rhs/res: translated fn(), borrow-array sig `(t,y,ydot,p)` / `(t,y,yp,res,p)`.
  No SEXP per step (like paropt's `OS` / `wrapper_ode_system`).
- jac default = `jacobian(rhs, y)` from `derivative="forward"` -> exact compiled
  Jacobian for the Newton iteration and for sensitivities.
- parallel orchestrator runs N integrations, no R in the loop.

## Later
- paropt could be re-expressed on top of this instead of vendoring its own CVODE.
