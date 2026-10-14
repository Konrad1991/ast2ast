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

# Reserved names clash with generated C++ (found 2026-10-13, make_package)

A user fn/type named `foo_t`, `foo_impl` or `foo_namespace` clashes with the
names generated for a fn `foo` (functor `foo_t`, namespace `foo_impl`).
Also today with `translate()`. Fix: reject these suffixes in fn/type names.
```r
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(a) { argtypes(a |> type(double)); returntype(double); a }
  g_t <- function(a) { argtypes(a |> type(double)); returntype(double); a }  # clashes with functor g_t
  g(a) + g_t(a)
}
```

# Three-valued logic and NA^0 differ from R, silently (found 2026-10-13, review)

R: `NA & FALSE` is FALSE, `NA | TRUE` is TRUE, `NA^0` is 1. ast2ast gives NA
for all three (runtime NA; NA literals are rejected at translation, on purpose).
Fix in the operators `&`, `|`, `&&`, `||`, `^` (Logical/Double).
```r
f <- function(x, b) {
  argtypes(x |> type(vec(double)), b |> type(vec(logical)))
  returntype(vec(double))
  r <- numeric(3L)
  r[1L] <- as.numeric(b[[1L]] & FALSE)   # R: 0, ast2ast: NA
  r[2L] <- as.numeric(b[[1L]] | TRUE)    # R: 1, ast2ast: NA
  r[3L] <- x[[1L]]^0                     # R: 1, ast2ast: NA
  r
}
translate(f)(c(NA_real_, 1), c(NA, TRUE))
```

# Empty subset: unclear error message (found 2026-10-13, review)

Empty vectors are an error on purpose (Konrad). But `x[x > 0]` without a
match depends on the data, not the code -- the message should say so.
```r
f <- function(n) {
  argtypes(n |> type(int))
  x <- numeric(n)
  x[x > 0]   # "Empty index for at least one dimension"
}
translate(f)(3L)   # better: no element of x matches the condition
```

# Structs/collections from R are copied on every call (idea, 2026-10-13)

At the R -> C++ boundary a new_type / collection argument is converted from the
R list slot by slot (copy); ref() has no effect there (warning). Vectors have
borrow_vec/borrow_mat, structs have nothing comparable. Costly when an outer fn
with a large struct is called often from R. Inside (inner fns) ref() already
works for structs and collections.
Idea: a borrowed struct whose vector slots are Borrow views on the R list's vectors.
```r
f <- function(p) {
  argtypes(p |> type(Big))   # every call copies all slots of p
  p$x[[1L]]
}
```
Plan: scalars copied, vector/matrix slots as Borrow on the list element SEXP,
nested structs recursive. Issues: slot type must match exactly (no coercion),
writes change the R object (like borrow_vec), struct template needs a second
parameter for the storage (owned/borrowed).

# FIXED — make_package: no derivative per entry point (found 2026-10-13)

Fixed: make_package(..., derivative = list(g = "reverse")); test in test_make_package.R.

R/MakePackage.R passes derivative = NULL -> every entry point is etr::Double.
Already possible: one .cpp per entry point, own `using ETR_REAL`, helpers in
`<entry>_impl`, structs in anonymous namespace -> no ODR clash.
Missing: API for derivative per entry point + passing it to translate_internally.
```r
make_package("R/model.R", entry_points = c("loss", "grad"))
# grad needs etr::Dual / etr::ReverseDouble, gets etr::Double
```

# is.na(NaN) is FALSE (found 2026-10-13, Randomization)

R: `is.na(NaN)` is TRUE. ast2ast only flags R's NA (`ISNA`), owned and
borrowed data alike.
```r
f <- function(x) {
  argtypes(x |> type(vec(double)))
  returntype(int)
  n <- 0L
  for (i in seq_len(length(x))) if (is.na(x[[i]])) n <- n + 1L
  n
}
translate(f)(c(1, NA, NaN))   # 1, R gives sum(is.na(c(1, NA, NaN))) == 2
```

# FIXED — Type declaration without assignment emits `x;` (found 2026-10-13, tsf)

Fixed in block_node$stringify (R/Nodes.R): statements with operator `type`
are skipped; test in test_translate.R.

A pure declaration `x |> type(double)` as a statement emits an empty C++
statement -> `-Wunused-value` on every package install (hidden by
sourceCpp's quiet output with translate()). Fix: block_node skips pure type
declarations when stringifying (also the current_line label).
```r
f <- function(a) {
  argtypes(a |> type(double))
  for (i in 1L:3L) {
    pred |> type(double)   # C++: "pred;" -> warning: statement has no effect
    pred <- a * i
  }
  a
}
```

# cvode/ida: max steps as argument (2026-10-14, branch sundials)

SUNDIALS default is 500 steps per output interval; Jan's model hit
`IDA_TOO_MUCH_WORK` at t = 0.036. Now hardcoded `a2a::max_num_steps = 1e6`
in `inst/include/a2a_sundials.hpp` (not disabled: the solver loop cannot be
interrupted from R). Later: optional argument, e.g. `maxsteps` like deSolve
(default 5000) -> needs variable arity (`num_args = c(7, 8)` / `c(8, 9)`).
