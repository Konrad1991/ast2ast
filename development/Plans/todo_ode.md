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

## Sensitivities via CVODES / IDAS (idea, 2026-10-16)

Forward sensitivities: `etr::Double` only, no AD.
- CVODES/IDAS build the sensitivity RHS by difference quotients when no `fS`
  is given; same user RHS as `cvode()`, result dy/dtheta
- params have to be a double vector (`CVodeSetSensParams(p, pbar, plist)`
  perturbs `p[i]` directly); structs need a way to mark which slots are theta
- cost: n_theta extra systems -> fine for few parameters

Adjoint: reverse AD.
- backward RHS needs `J^T lambda` = one reverse sweep: record f(y(t)) on the
  tape, seed the outputs with lambda, sweep back; copy the adjoints from the
  tape into the `N_Vector` (O(n))
- same sweep gives `(df/dtheta)^T lambda` for the gradient quadrature if theta
  is on the tape
- RHS instantiated twice via the functor template (`ETR_REAL`): `etr::Double`
  for the forward solve, `ReverseDouble` for the backward one (as runtime
  colouring); `ReverseTape::rewind()` per call
- stiff backward problem needs `-J^T`: forward-AD J transposed, not n reverse sweeps
- memory: CVODES checkpoints of the forward solution
