# Splines

## Goal

Time-varying parameters in ODE fits (paropt migration): a parameter is given
at knots, the rhs needs its value at any `t`. Splines are set up once per
parameter vector and reused across all rhs calls of one ODE solve.

## Existing

- `cmr(point, x, y)`: Catmull-Rom, stateless, slopes computed locally per call.
  Fast, but can overshoot between knots.

## Design

- Same pattern as `Norm`/`Unif` (see `rng_distributions.md`):
  built-in types in the `new_type` machinery, fixed slots, free functions.
- Constructor does the setup (slopes / coefficients), evaluation is a free
  function.
- Pass to the rhs via `ref()` (read-only, a copy would be correct but costly).

```r
s |> type(Spline) <- spline_natural(knots_t, knots_y)
value <- invoke(s, t)
```

## paropt use case

- Inputs: rhs as R function, lower/upper bounds as data.frames
  (column = parameter, rows = time points), states data.frame
  (row 1 = initial values, other rows = measured states).
- Optimizer (`pso()`) passes one flat parameter vector.
  It is split following the layout of the bounds:
  1 row -> scalar, more rows -> knot values of a spline.
- Per loss evaluation:
  1. split parameter vector
  2. build one spline per time-varying parameter (setup once)
  3. solve the ODE; every rhs call evaluates `invoke(s, t)`
  4. compare with measured states
- The splines must stay alive between rhs calls of one ODE solve.
  Setup per rhs call would redo the slope / tridiagonal work every time.
  So the spline objects live in the loss function and reach the rhs via
  `ref()` (e.g. as part of the parameter struct handed to `cvode()`).
- Stateless `cmr()` avoids this, but only for local Hermite methods.

## Methods

| Method | Smooth | Overshoot | Setup | R |
|---|---|---|---|---|
| constant | no | no | none | `approx(method = "constant")` |
| linear | C0 | no | none | `approx()` |
| Catmull-Rom | C1 | yes | none (local) | `cmr()` in ast2ast |
| monotone Hermite (Fritsch-Carlson) | C1 | no | O(n), slope limiter | `splinefun(method = "monoH.FC")` |
| natural cubic | C2 | yes | O(n), tridiagonal solve | `splinefun(method = "natural")` |

- Hermite splines (Catmull-Rom, monoH.FC): cubic per interval, defined by
  values + slopes at both ends. Slopes are local, no linear system.
- monoH.FC = Catmull-Rom-like slopes + limiter:
  - flat secant -> both slopes 0
  - secant sign change -> slope 0 at that knot
  - `alpha^2 + beta^2 > 9` -> scale both by `3 / sqrt(alpha^2 + beta^2)`
- C2 splines: slopes chosen globally so the second derivative is continuous.
  Tridiagonal system, Thomas algorithm, O(n), once per knot set.
- Evaluation is the same for all cubic ones: find interval, evaluate cubic.

## Why overshoot matters

Bounds hold at the knots only. Catmull-Rom / natural can leave them between
knots, e.g. a rate constant in [0, 1] becomes negative. Linear and monoH.FC
stay between neighbouring knot values.

## R sources (~/Documents/r-source/src)

- `library/stats/src/approx.c` (linear, constant)
- `library/stats/src/splines.c` (natural, fmm, periodic)
- `library/stats/src/monoSpl.c` (monoH.FC slope limiter)

## Open points

- Which methods in v1.
- Names of the constructors. Evaluation: `invoke(s, t)`.
- monoH.FC: R corrects a slope from both adjacent intervals in sequence;
  a purely local variant is not R-identical.
- AD w.r.t. knot values (needed for `lbfgsb()`): linear, Catmull-Rom and
  natural are linear in `y`, monoH.FC limiter is non-smooth.
- Extrapolation outside the knots (R: `rule = 1` NA, `rule = 2` constant).
