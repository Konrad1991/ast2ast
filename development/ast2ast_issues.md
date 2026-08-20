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

### What worked well (no issue needed)

Reverse- and forward-mode AD (incl. through loops and, with `const` args, through
inner `fn` calls); custom types, vector/matrix slots, nested structs, chained
`$` access, and collections; `uniroot`/`nnls` via `fn()`; mutual recursion across
inner functions; `stop()` propagating a clean R error; 3-D arrays; `solve` on
struct slots (the clean, vector-returning path). The contract/static layer is the
strongest part of the package — precise, located, and semantic.
