# ast2ast stabilization findings

Six grouped issues from a documentation-only exploration (README, `translate.Rd`,
both vignettes). Each was reduced to a minimal reproducer against the current
GitHub HEAD. The recurring pattern: the **R-side static/contract layer** produces
excellent, located, semantic errors; the gaps are (a) raw C++ exceptions reaching
the user unlocated, (b) a compile bug when returning a borrowed array, (c) a few
missing input guards, (d) cosmetic checker bugs, and (e) doc/behaviour mismatches
where the parser is often *better* than documented.

Note: `stop()` is supported and works — it lowers to `etr::stop(...)` and surfaces
a clean, `tryCatch`-able R error. Mentioned only because it demonstrates a working
R-error boundary already exists in the runtime.

---

## Issue 4 — Static-checker cosmetics: duplicate emission and a typo

The static checker's wording is good; a few messages are emitted multiple times or
contain a typo. Likely a single dedupe-at-collection fix covers the duplicates.

**4a. Duplicate emission (systemic, ≥2 sites).**

```r
# unknown struct field -> printed 4x
types_f <- function() new_type(Point, slots(x |> type(double), y |> type(double)))
f <- function(p) { p$z <- p$z + 1; return(p) }
translate(f, args_f = function(p) p |> type(Point), types_f = types_f)
#> Point has no field named z   (x4)

# single-bracket on a collection -> printed 3x
f <- function(n) { pts <- vector(mode="Point", n); pts[1L]$x <- 5.0; return(pts[[1L]]$x) }
translate(f, args_f = function(n) n |> type(int), types_f = types_f)
#> Found unsupported subsetting: pts[1L]   (x3)
```

**4b. Helpful message as bare string, then a vaguer framed error on top.**

```r
g <- function(x) return(sum(x))
translate(g, args_f = function(x) x |> type(vecc(double)))
#> [1] "Found unsupported data structure: vecc, for variable: x"     <- useful, bare string
#> Error: Types for arguments are invalid                            <- vaguer, framed
```

**4c. Typo** "uninitialzed" → "uninitialized" (shared string; appears for both
top-level and inner-function undefined variables).

---

## Issue 6 — Document the borrow / ref / XPtr calling convention (with the raw-pointer use case)

The docs show the XPtr calling convention only for a nullary `void()` function.
The advertised in-place idiom (`borrow_vec |> ref()`, `output = "XPtr"`) has no
example of the actual C-side signature, and a plausible `void(*)(SEXP)` guess
yields `std::bad_alloc` at call time.

Because `Borrow` is a class wrapper around a **raw pointer**, the XPtr function does
not take a `SEXP` — it takes the (borrow-wrapped) pointer. The motivating use case
is a C/C++ library wrapping its own pointer in `Borrow` and passing it to the
generated function with zero copy — e.g. wrapping SUNDIALS state pointers for an
ODE right-hand-side function.

**Suggested direction:** add a worked example to the vignette showing:
- the correct C-side function-pointer signature for a borrowed-vector argument,
- how a caller constructs a `Borrow<Double>` around an existing raw pointer,
- the SUNDIALS-style zero-copy ODE handoff as the motivating scenario.

This turns borrow/ref/XPtr from "documented but not usable from the docs alone"
into a followable pattern.

---

## Issue 7 — `for (i in a:b)` materializes a full array just to drive a counter

`seq_len(n)` used directly as a `for`-loop's sequence already gets special-cased
codegen: a plain scalar counter, no allocation. `:` (colon) does not — it always
routes through `etr::colon()`, which eagerly allocates and fills a full
`Array<DataType, Buffer<...>>` of length `b-a+1` before the loop body runs at
all, purely to drive a counter that never needed to exist as an array.

```r
f1 <- function(rounds) {
  s <- 0.0
  for (i in seq_len(rounds)) s <- s + 1.0
  return(s)
}
f2 <- function(rounds) {
  s <- 0.0
  for (i in 1:rounds) s <- s + 1.0
  return(s)
}
```

Generated code for the `seq_len` loop (`inst/include/etr_bits/Allocation.hpp`,
`length_seq`) — a direct counting loop:

```cpp
for(etr::Integer i = 1; i <= etr::length_seq(rounds); i = i + etr::Integer(1)) { ... }
```

Generated code for the `:` loop — routes through `colonInternal`
(`inst/include/etr_bits/Allocation.hpp:135-163`), which allocates
`Array<DataType, Buffer<DataType, RBufferTrait>> ret(SI{length})` and fills it
element-by-element *before* the range-based `for` even starts:

```cpp
for(const auto i : etr::colon(etr::Double(1.0), rounds)) { ... }
```

Measured impact (Leibniz-formula pi loop, `development/Benchmarks/bench_leibniz.R`,
rounds = 10,000,000, median of 5 reps): the `seq_len` form ran in **~17ms**, the
otherwise-identical `:` form in **~77ms** — a **~4.5x** difference from this one
codegen gap, on a loop that does no other array work.

**Suggested direction:** give `for (i in a:b)` the same direct-counter codegen
`seq_len` already has when the sequence expression is a colon binary_node
(a compile-time-visible pattern, no new runtime machinery needed) — skip
`etr::colon()`/array materialization entirely and lower straight to a scalar
counting loop, matching `length_seq`'s existing approach.

---

### What worked well (no issue needed)

Reverse- and forward-mode AD (incl. through loops and, with `const` args, through
inner `fn` calls); custom types, vector/matrix slots, nested structs, chained
`$` access, and collections; `uniroot`/`nnls` via `fn()`; mutual recursion across
inner functions; `stop()` propagating a clean R error; 3-D arrays; `solve` on
struct slots (the clean, vector-returning path). The contract/static layer is the
strongest part of the package — precise, located, and semantic.
