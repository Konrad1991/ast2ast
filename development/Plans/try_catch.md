# tryCatch in the DSL

Branch: dev-1.2. Decided 2026-10-30 (Konrad): option b) general tryCatch,
not a cvode-specific "return NA" option.

## Goal
Errors raised inside translated code (cvode failure, bounds checks, stop())
can be caught inside the translated function instead of always jumping back
to R.

Done when:
- `tryCatch(expr, error = function(e) alt)` translates and works in all
  translations (plain / forward / reverse)
- tape is consistent after a caught error inside gradient() / hessian() /
  value_and_gradient() / cvode adjoint
- NUTS example rejects on cvode failure; 20 sd guard removed
- tests green

## Status quo
- etr::ass / ass_fail / etr::stop -> throw std::runtime_error
  (no Rcpp::stop: must work in worker threads)
- debug = TRUE: whole body wrapped in try/catch -> Rcpp::stop with
  current_line() (R/CreateNodeAST.R:470)
- otherwise Rcpp's .Call wrapper / pmap pool rethrows to R
- nothing in between can catch

## Step 1: RAII tape guard (prerequisite) -- DONE 2026-11-05 (etr::TapeGuard, see nuts.md)
An exception during recording leaves the tape with half a recording
(no rewind). Today harmless (error ends at R level, tape cleared on next
entry); with tryCatch the function keeps running on a broken tape.

- struct tape_guard { tape_start, blocks_start; dtor rewinds unless released }
- replace manual rewinds:
  - Derivatives.hpp:537
  - a2a_sundials.hpp:721 (vjp), 909 (vjp2)
  - Hessian.hpp:618 (with_recorded_scalar_f), 740 (value_and_gradient_impl)
- reverse translation (whole function on the tape): error inside expr ->
  rewind to the tape position before expr
- SUNDIALS memory: already RAII (workspaces / shared_ptr), check only
- tests: C++ test that throws mid-recording, tape size afterwards unchanged

## Step 2: syntax + types (decisions for Konrad)
Proposal, R-like:
```r
lp <- tryCatch(log_post(theta, data), error = function(e) -Inf)
```
Open:
1. Only `error = function(e) ...` or also `finally`? (proposal: error only)
2. Type of the error branch must match expr -> cast like if/else, or error?
3. Message access: `conditionMessage(e)` -> character (proposal: yes)
4. Statement form (`tryCatch({ ... }, error = ...)` without value) allowed?
5. Catch only std::runtime_error or std::exception (bad_alloc)?
   (proposal: std::exception)
6. warning = ...? (proposal: no, warnings are not exceptions here)

## Step 3: codegen
- registry entry "tryCatch" (special node, like if)
- C++: immediately invoked lambda
  ```cpp
  auto res = [&]() -> T {
    etr::tape_guard g;
    try { auto v = expr; g.release(); return v; }
    catch (const std::exception& e) { std::string msg = e.what(); return alt; }
  }();
  ```
- current_line(): save before, restore in catch
- type inference: common type of expr and handler body
- checks: handler must be `function(e)` with one argument; e only usable in
  conditionMessage(e)

## Step 4: threads
- pmap: catch inside a worker = fine (same thread)
- uncaught: pool rethrow unchanged
- warning_slot: check that no state leaks after a caught error

## Step 5: tests (tinytest, function() syntax)
- stop("x") caught -> handler value
- message via conditionMessage(e)
- bounds error caught
- nested tryCatch
- caught error inside gradient()/hessian()/value_and_gradient(): following
  derivatives correct
- cvode failure (extreme parameters) caught -> -Inf
- reverse translation: caught error, gradient of the rest correct
- type mismatch expr/handler -> translate error

## Step 6: NUTS example
- `lp <- tryCatch(..., error = function(e) -Inf)` -> divergent, reject
- remove 20 sd guard, compare ESS / divergences with the current version

## Order
1 -> 2 (Konrad decides) -> 3 -> 5 -> 6. Step 4 alongside 3.
