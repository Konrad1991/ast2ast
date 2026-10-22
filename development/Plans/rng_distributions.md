# RNGs and distributions

## Goal

Users can write stochastic algorithms (tabu search, simulated annealing, ...)
in ast2ast. Random numbers are bit-identical to R.

## Design

- No global seed state. Engines and distributions are instances.
- A distribution owns its engine (by value, on the stack).
- Drawing via a free function `draw(n, k)` (S3 style, no method syntax).
- `Norm`/`Unif` are built-in types in the same machinery as `new_type`
  (fixed slots + built-in functions).

```r
n |> type(Norm) <- Norm(MersenneTwister(1234))
x <- draw(n, 19)
```

- C++: `Norm<Engine>` / `Unif<Engine>` templated on the engine.
  An engine only needs `double runif()`. More engines from Konrad1991/Random
  can be added later without touching the distributions.

## Scope v1

- Engine: `MersenneTwister` only.
  Already in `inst/include/etr_bits/Optimization/MersenneTwister.hpp`
  (copied from Random, R-identical).
- `Unif`, `Norm` with `draw(n, k)`.
- `sample`: with replacement, without replacement, with `prob`.
- `dunif`, `punif`, `qunif`, `dnorm`, `pnorm`, `qnorm` (with `log`).
- `pso()` switches to the new engine.

Not in v1: other engines, other distributions, `useHash` path of `sample`.

## R sources (~/Documents/r-source/src)

- `runif(a, b)`: `a + (b - a) * u` (`nmath/runif.c`)
- `rnorm`: default INVERSION (`nmath/snorm.c:265`)

  ```c
  u1 = unif_rand();
  u1 = (int)(BIG*u1) + unif_rand();   // BIG = 2^27
  return qnorm(u1/BIG, 0.0, 1.0, 1, 0);
  ```

- `sample` index: default REJECTION via `rbits()` (`main/RNG.c:878-900`)
- `sample` variants: `main/random.c` (no replace: partial Fisher-Yates ~527,
  walker / `ProbSampleReplace` / `ProbSampleNoReplace` 312-407)
- d/p/q: `nmath/{d,p,q}{unif,norm}.c`, helpers in `dpq.h`

## Open points

- Same seed in two distributions = identical streams (document it).
- Passing a distribution by value to an inner fn repeats draws.
  Needs `ref()` or a warning.
- Mixing distributions is not R-identical (each owns its own stream).
- `MersenneTwister::unif_index()` uses ROUNDING, not R's REJECTION.
- Derivatives of d/p/q for forward/reverse AD.

## Decisions 2026-10-22

- snake_case like `uniroot_result`: `mersenne_twister`, `normal`, `uniform`
  (`norm` clashes with base R's matrix norm; name not final)
- `mersenne_twister` is a type of its own; constructor = type name
- slots (`mean`, `sd`, ...) read-only
- no passing from / returning to R
- copy of a distribution or engine (by value) -> warning at translation time
- `unif_index()` -> R's REJECTION (changes `pso()` results)
- derivatives of d/p/q in v1 (needed for HMC)
  -> changed: only `dnorm`/`dunif` (with AD); p/q when a use case shows up
- stage 1: engine + `normal`/`uniform` + `draw()`, incl. `qnorm` port (AS241);
  done when `draw(normal(mersenne_twister(1L)), 10L)` == `set.seed(1); rnorm(10)`

## Done when

- `MersenneTwister`, `Unif`, `Norm`, `sample`, d/p/q work in the DSL.
- Tests bit-identical to `set.seed(s)` + `runif`/`rnorm`/`sample`
  (all three `sample` variants) and to R's d/p/q.
- `pso()` uses the new engine.
