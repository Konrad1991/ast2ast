# Counter design: R semantics instead of borrow/ref/const

Status: thought experiment (2026-10-14). Decision: not as a replacement (too
late, borrow/ref/const are on CRAN). Konrad's simplified version: types;
read-only args as const&; written args copied; environments as the only
references, entries with fixed types, passed as non-const references.
Additive part planned: `new_type_environment.md`. Counterpart to the
current memory model (vignettes/MemoryModel.Rmd: borrow = don't copy the data,
ref = don't copy the object) and to named_vector_layout.md.

## Principle

The DSL behaves like R; memory is the compiler's job, not the user's.
- every vector/matrix/list is a value: assignment and argument passing are
  (logical) copies, the caller never sees writes of the callee
- the only shared mutable object is an environment, as in R
- copies are avoided by copy-on-modify, like R itself

## User-facing language

No `borrow_*`, no `ref()`, no `const()`. Types only.

```r
f <- function(y, p) {
  argtypes(y |> type(vec(double)), p |> type(Params))
  y[[1]] <- 0        # local copy (only now), caller's y unchanged
  y
}

# shared mutable state: an environment with typed fields
new_env_type(Work, slots(buf |> type(vec(double)), n_calls |> type(int)))
fill <- function(w) {
  argtypes(w |> type(Work))   # environments are always references
  w$buf[[1]] <- 1             # visible to the caller, as in R
  w$n_calls <- w$n_calls + 1L
}
```

## Implementation

1. Copy-on-modify buffers
   - owned arrays hold a reference-counted buffer (like R's refcnt / NAMED)
   - assignment / passing increments the count, no data copy
   - a write checks `refcount == 1`; otherwise it copies first
   - the check is hoisted out of loops by the compiler (one check before
     `for (i ...) y[[i]] <- ...`), so the hot path stays a plain store
2. R boundary (SEXP arguments)
   - always borrowed read-only (replaces borrow_vec)
   - written in the body (static check, the AST is known) -> copy at entry
   - never written -> zero copy; R memory is never modified (today: borrow_vec
     without const() writes into the caller's R vector, which R never does)
3. Environments
   - `new_env_type` = new_type with reference semantics: `std::shared_ptr` to a
     struct with typed slots; passing an env passes the pointer
   - created from R: `new.env()` + fields checked against the slots at the boundary
4. Solver memory (cvode/ida)
   - rhs is a pure function, as in deSolve: `rhs(t, y, p)` returns ydot
   - y: borrowed read-only from N_Vector, like an R argument (copy only if written)
   - ydot: the returned vector is written into the N_Vector (one memcpy per
     call, or constructed in place when the return value is a fresh local)

## Jan's model in this design

```r
ode_rhs <- function(t, y, p) {
  argtypes(t |> type(double), y |> type(State), p |> type(DaeParams))
  returntype(State)
  y[["EFD"]] <- y[["Eut"]] - y[["EFT"]] - sum(y[132:169])  # copy here (needed: y is the solver's)
  y[["EFG"]] <- p$met$c_EFG_t - y[["EGT"]] - y[["EGD"]]
  f <- module_rates(y, p)                                   # no copy: read-only
  f[["EFG"]] <- -f[["EGT"]] - f[["EGD"]]
  f                                                         # -> ydot
}
residual <- function(t, y, yp, p) {                         # IDA: returns r
  module_rates(y, p) * -1 + p$mass * yp                    # no y_copy loop at all
}
```
- no `y_copy` loops, no borrow/ref/const in any signature
- the one real copy (y modified in ode_rhs) happens automatically
- named vectors: `State` without named_vec/named_borrow_vec split

## Annotations become assertions (optional)

Expert control without changing semantics:
- `y |> type(vec(double)) |> no_copy()` -> translation error if the compiler
  cannot prove that no copy happens (instead of today's silent semantics switch)

## Comparison

| | current design | R semantics |
|---|---|---|
| user concepts | type + borrow / ref / const | type (+ env for sharing) |
| writes visible to caller | ref(), borrow_vec without const | only via environments |
| R memory modified | possible (borrow_vec) | never |
| copies | explicit, predictable | automatic, copy-on-modify |
| runtime cost | none | refcount + write check (hoisted in loops) |
| in-place fill pattern | `out |> ref()` | env field or return value |
| teaching | memory model vignette | "it's R" |

## Costs and risks

- refcount on every array copy/assignment; a write check where it cannot be hoisted
- pmap / threads: refcounts shared across threads need atomics or per-thread
  ownership (thread_safe flag already exists)
- AD tapes (ReverseDouble, Dual): copy-on-modify must keep tape ids consistent
- copies become invisible -> performance surprises; mitigated by no_copy() and
  a translation-time report of where copies happen
- breaking change: borrow_*/ref()/const() are on CRAN -> keep them as
  deprecated hints for a transition period

## Effort (rough)

- Buffer with refcount + copy-on-write: core of etr, large
- write analysis for R arguments: medium (AST traversal)
- new_env_type: medium (new_type with shared_ptr)
- cvode/ida pure rhs: small
