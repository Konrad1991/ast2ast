# Runtime sparsity pattern for coloured jacobians

Status: plan (2026-10-10), not started. Follows `coloured_forward_ad.md`.
Decided (Konrad, 2026-10-10): B (reverse tape) + codegen option 1 (second copy
of f and its callees for the pattern pass).
Step 1 done (2026-10-10): `colouring_from_tape(x, f_rev)` and
`jacobian_runtime_coloured(f, f_rev, x[, extra])` in Derivatives.hpp,
`ReverseTape::rewind()`; C++ test `src/tests_coloured_jacobian.cpp`
(helper with whole state n = 200: 3 colours; arrow: 2; if: exact per branch;
matmul: conservative dense). Next: step 2 (R pre-pass, copies).
Steps 2-3 done (2026-10-12), changed vs. below: API is
`jacobian(f, x, colouring = "static", n = ...)` / `colouring = "runtime"`;
greedy copies: every inner fn `g` -> `g_COLOURED` (ReverseDouble, by suffix)
once a runtime jacobian exists (`add_coloured_copies`, R/Colouring.R); call
emits `etr::runtime_colouring(f_COLOURED)` as last arg. Custom types: every
`T` gets `T_COLOURED` (ReverseDouble slots, converting ctor from `T` instead
of SEXP ctor/cast; `coloured_known_types`). `_COLOURED` is always reserved.
Limit: copy that fails in reverse mode -> error (later: drop it).

## Why

The compile-time analysis (`R/Colouring.R`) resolves banded loops, but goes
dense as soon as the state is passed as a whole: helper functions (`lap(y)`),
structs built from the state (`s <- unpack(y)`), `ref()` outputs
(`fill(out, y)`). Jan's model (`ODE.m`) is exactly that: unpack state ->
per-module rhs -> sum -> pack. So it would always be dense.

A runtime pattern follows the executed code, through every function, struct
and `ref()`, and is exact for the current `x`.

## Shape

```
pattern(x)  -> rows: which x[j] each f(x)[i] depends on   (one extra evaluation)
colour      -> greedy, as color_columns(), in C++          (O(nnz))
sweeps      -> jacobian_coloured_impl, unchanged
```

Recomputed on every Jacobian call: then value-dependent `if`s are no problem
(the pattern belongs to the branch taken at this `x`). One extra evaluation is
cheap against `n` sweeps; CVODE/IDA ask for J rarely.

## Two ways to get pattern(x)

### A: tracer scalar type

`Tracer { double val; sorted index set }`; `y[[j]]` starts as `{j}`, `a op b`
-> union, math fns -> union of args, constants -> `{}`, comparisons on `val`.

- new scalar type through all ops (like `Dual`, simpler: no derivative math)
- new buffer type (non-trivial element, like `ReverseDouble`)
- block ops (`%*%`, `solve`, `chol`, ...) need their own rules

### B: reverse-mode tape

The `ReverseDouble` tape already is the dependency graph of one evaluation.
Run `f` once in reverse mode, then per output walk the tape back to the `Var`
nodes it reaches -> that row of the pattern.

- no new scalar type, no new op rules: every op that supports reverse AD works
- `BlockOp` nodes: conservatively, every output cell depends on every input of
  the block (exact for `%*%` can come later)
- cost: tape recording + one graph walk per output (bitset of visited nodes);
  fine for "once per Jacobian"

## Shared problem: f in a second scalar type

Inner functions are emitted as `std::function` with fixed types
(`Array<Dual, ...>`). The pattern pass needs `f` (and everything `f` calls,
transitively) also for `Tracer` (A) or `ReverseDouble` (B). Options:

1. codegen emits a second copy of `f` and its callees for that type
   (`f__pattern`), only when `jacobian(..., colouring = TRUE)` needs it
2. inner functions become templates over the scalar type (generic lambdas);
   larger change, also touches recursion and passing functions around

## API

```
jacobian(f, x, colouring = TRUE)            # runtime pattern
jacobian(f, x, colouring = TRUE, n = 100L)  # compile time if resolvable, else runtime
```

Compile-time stays the fast path (no extra evaluation); runtime replaces the
current dense fallback + warning.

## Proposal

B + option 1: no new scalar type, reuses tested reverse AD, smallest surface.
A only if the tape walk turns out too slow for large `n`.

## Implementation steps (B-1)

Each step is self-contained and testable on its own.

1. C++, no codegen: `pattern_from_tape(f_rev, x)`
   - `x` as `ReverseDouble::Var`, record `f_rev(x)`
   - per output: walk the tape back over the operands (`BlockOp`: all inputs
     of the block), collect the reached `Var`s -> row of the pattern
   - greedy colouring in C++ -> `etr::Colouring` -> existing sweeps
   - test with hand-written C++ lambdas against plain `jacobian`
2. R pre-pass: copies
   - for `jacobian(f, x, colouring = TRUE)`: all inner fns `f` calls,
     transitively
   - a copy `name__pattern` of each, calls inside renamed to the copies
     (R expression substitution on `fn$AST`); inline `fn(...)` the same way
   - always emitted with `colouring = TRUE` (also with `n`): whether the
     compile-time pattern resolves is only known after parsing; an unused
     copy only costs compile time
3. Translation
   - a `fn_node` may carry its own `real_type` (`ReverseDouble`);
     `action_transpile_inner_functions` uses it instead of the outer one
     (already parametrised by `real_type`)
   - the `jacobian` call gets the copy as an extra C++ argument
4. Tests: helper fns, structs built from the state, `ref()` outputs, an
   excerpt shaped like Jan's model; against plain `jacobian` and the harness

## Verify

- pattern(x) equals the compile-time pattern on the harness cases
  (`development/test_colouring.R`) and FD ground truth
- helper fns / structs / `ref()` cases: coloured, J equals plain `jacobian`
- Jan's model excerpt translated to the DSL: pattern banded + coupling rows

## Open decisions

- A or B
- codegen option 1 or 2
- does `deriv_possible` differ between forward and reverse for anything `f` uses
  (B needs reverse support for every op in `f`)
