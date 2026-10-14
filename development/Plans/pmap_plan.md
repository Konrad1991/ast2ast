# pmap — parallel map (plan, not started)

Frozen 2026-09-02. Don't code yet.

## DSL
`pmap(f, ncores, x, ...)` — like `map`, 2nd arg = core count, clamped [1, hw_max].
No options().

## R (FunctionRegistry.R)
Clone `map`. `cpp_name="etr::pmap"`, `deriv_possible=FALSE`
(-> auto-blocked in forward/reverse via TypeInference.R:143, no custom check).
Data vectors + f param-matching start at arg 3. Validate arg 2 = numeric scalar.

## C++ (Functionals.hpp)
`pmap(f, ncores, first, rest...)`, `nthr = clamp(get_val(ncores), 1, hw_max)`.
Result buffer allocated once on main thread. Then per return-type branch:
- scalar R: `parallelFor(0,n, res.set(i, f(map_at...)), nthr)`. Distinct i, no race.
- Array R: run i==0 serial (fixes dim + resize), then `parallelFor(1,n,...)`.
  rank/extent mismatch -> `atomic<bool>` flag, throw on main thread after.
- Collection/new_type: same shape as scalar (`res[i] = f(...)`), also parallel.

## Header/dep — DONE 2026-10-06 (serial tests green)
- DESCRIPTION: RcppThread -> Imports + LinkingTo. `#include <RcppThread.h>` behind
  `#ifndef STANDALONE_ETR`.
- PRINT_STREAM (Reflection.hpp:9, Printing.hpp:8): `Rcpp::Rcout` -> `RcppThread::Rcout`.
- `ass` (Types.hpp ~78/~100) + `etr::stop` (Helper.hpp ~427): no RcppThread stop();
  collapse to the STANDALONE branch -> `throw std::runtime_error`. Pool rethrows
  from wait(); .Call boundary handles main-thread case.
- Collection SEXP ctor `Rf_error`: leave (main thread only).
- generated code: `Rcpp::depends(ast2ast, RcppThread)`.
- debug try/catch + `Rcpp::stop` only wraps the outer fn (main thread) -> fine.
  Caveat: `current_line()` is thread_local, worker errors report the pmap line.

## warn() in workers (decided 2026-10-06)
`Rcpp::warning` touches R API. Option A: collect warnings per thread,
emit them on the main thread after `wait()`.

## thread_safe flag (new registry field)
Some builtins touch R API / R RNG -> unsafe in a worker.
- Add `thread_safe = FALSE` to registry (separate from deriv_possible).
- `pso` is thread_safe = TRUE now (own Mersenne Twister, no R RNG). uniroot + nnls audited safe.
- `lbfgsb` thread_safe = FALSE (field DONE 2026-10-06): R's optim.c driver uses R_alloc/error,
  lbfgsb.c keeps f2c `static` state (not even reentrant).
  Decided 2026-10-06: stay with R's optimizers, ensmallen dropped. Later: vendor
  R's lbfgsb.c (statics -> state struct, R_alloc -> std::vector), then thread_safe = TRUE.
- pmap infer/check: reject if f transitively calls a `thread_safe=FALSE` builtin.
- Do NOT pre-generate random values and pass them in — too fiddly.

## Future (not v1)
AD-per-thread is actually fine when tape-id values never cross the thread
boundary (e.g. f runs a self-contained optimization, returns plain double).
`deriv_possible=FALSE` is the safe default now; relax later per-case.

## Tests
- pmap vs map parity: scalar / vector / new_type return.
- pmap under derivative="forward" -> rejected.
- pmap where f calls a thread_safe = FALSE builtin -> rejected.
- ncores 0 / negative / huge -> clamped, correct.
