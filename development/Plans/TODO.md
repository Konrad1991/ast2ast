# Stabilization findings (novice-user / bad-error sweep, 2026-08-27)

## C. Silent divergence from R (no error, quietly wrong)

| # | Trigger | Result | R gives |
|---|---------|--------|---------|
| C2 | `2000000000L + 2000000000L` | `-294967296` (signed overflow, UB) | `NA` + warning |

# Inner-fn + jacobian + forward AD (found 2026-09-04 building development/pollu.R)

1. **FIXED** — `action_transpile_inner_functions` merge loop (`R/TraverseNodeAST.R`)
   copied outer-registry fns into an inner fn's registry but did not append
   `deriv_possibles` / `valid_fn_contexts`. Under `derivative = "forward"/"reverse"`
   an inner fn that calls a merged-in fn then hits `deriv_possible()` -> `NA` ->
   `TypeInference.R:143` `if (!NA && ...)` -> "missing value where TRUE/FALSE
   needed". Fixed by appending both vectors in the loop.
   TODO: regression test — inner fn calls a sibling inner fn, `translate(derivative = "forward")`.

2. Codegen capture scope: a doubly-nested `fn()` that calls a fn defined in its
   **grandparent** scope is emitted with a `[&grandparentFn]` capture in a scope
   where that name isn't visible -> uncompilable C++. Should hoist the fn / thread
   the capture through the intermediate lambda, or reject with a clear error.

3. `jacobian(f, x[, data])` requires `f` to be an `fn` **local to the block that
   calls jacobian**. A function reachable only via the outer-scope merge is
   rejected ("The first argument to jacobian has to be a function"). So `jacobian`
   can't be used inside a nested `fn()` unless the residual is (re)defined in that
   same block; wrapping the `jacobian` call in another sibling `fn` doesn't help
   (same scope relationship). Likely also affects `map`/`Reduce`/`Filter`/`lbfgsb`.
   Either accept merged/outer fns as the function arg, or document the restriction.

## Decision (2026-09-04): strict scoping for functions — post-1.0

Fix for 2 + 3: make inner `fn`s ordinary values. An inner fn sees only what is
defined in its own block or passed in as a parameter — same rule variables
already obey. No lexical reach-out (siblings/parent blocks) for functions, and
none for variables either (lexical for functions only would be inconsistent;
lexical for everything has type-inference edge cases — capture by-ref vs
by-value, mutable outer state, interaction with `ref`/`const` and AD seeding).

Consequences:
- Delete the `diffs` registry-merge loop in `action_transpile_inner_functions`
  (the mechanism behind bugs 1-3).
- Add function-valued parameters: an `fn` can be passed to another `fn`; a
  function param infers to `fn_node`, so `jacobian`/`map`/`Reduce`/`Filter`/
  `lbfgsb` accept it with no special case.
- Callers thread the dependency chain by hand (`g` gets `ode`, `finite_differences`
  gets `g`, `newton_raphson` gets all three). Breaks current examples that rely
  on implicit sibling visibility (e.g. development/implicit_euler.R) — update them.
- Consistent single rule: "defined here or passed in, else unknown".

## Blog post idea (2026-09-04): deSolve compiled-RHS bridge

Write an ODE RHS in R -> `translate(output = "XPtr", getsource = TRUE)` -> splice
the generated function into a C-ABI template (`derivs(int *neq, double *t,
double *y, double *ydot, ...)`), Borrow-wrap the `double*` args, `sourceCpp` +
`dyn.load` -> pass to `deSolve::ode(func = "...", dllname = ..., initfunc = ...)`.
Same solver/tolerances/grid as an R-RHS `ode()` call; only the RHS language
differs -> cleanest possible "what does compiling the RHS buy you" benchmark, and
deSolve is the standard R ODE package.

Old example: `~/Documents/ast2ast-examples/deSolve/deSolve.R` (Lorenz, 2024).
Needs: update to 1.0 syntax (`argtypes()` block + `types_f =`, not top-level
`|> type()`); verify the C template's `Vec<double, Borrow<double>>` matches what
1.0 `getsource` emits; add the `microbenchmark(R-RHS ode vs compiled-RHS ode)`.

Ideally first build a helper — `compile_for_desolve(f, npar)` returning something
`ode()` takes directly — so the blog snippet is 3 lines not a page of ABI
boilerplate. (Part of the planned package-compile helper.) Blog post, not README.
Sibling ideas, same shape: MCMC log-posterior compile (pairs with AD), Kalman
filter likelihood (uses linalg).

# Named vector layout (2026-09-09): `layout()` — design only, not started

`new_type(State, layout(double, names = c("prey", "pred")))` so an ODE RHS
reads `y[["prey"]]` not `y[[1L]]`. String index is always a literal (no
character variables), so it's rewritten to an integer index at translation
time — identical C++, bad name -> R `stop()`. Full doc + steps:
`development/named_vector_layout.md`. Konrad implements, Claude advises.
