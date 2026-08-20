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

## Issue 1 — Raw C++ exceptions reach the user unlocated (no variable name, no line)

Several runtime error paths throw raw C++/STL exceptions whose `what()` string is
shown directly to the R user, with no variable name, no line, and (in one case) a
misleading category. These are the least actionable errors in the package and
contrast sharply with the clean static-layer messages elsewhere.

**1a. Single-index subsetting of a 2-D object.** `solve(A, B)` with a *matrix*
RHS returns a matrix; single-indexing it then throws:

```r
library(ast2ast)
g <- function() {
  A <- matrix(c(2, 0, 0, 2), 2, 2)
  b <- matrix(c(4, 6), 2, 1)   # column vector as 2x1 matrix
  x <- solve(A, b)             # x is 2x1 matrix
  return(x[1])                 # single index on a 2-D object
}
translate(g)()
#> Error: Too less index arguments for array rank
```

A *vector* RHS returns a vector and indexes fine (`solve(A, c(4,6))[1]` works), so
the shape divergence from base R is the trap. Message is unlocated and reads
"Too less" (→ "Too few").

**1b. Collection out-of-bounds** leaks libstdc++ internals:

```r
types_f <- function() new_type(Point, slots(x |> type(double), y |> type(double)))
f <- function(n) {
  pts <- vector(mode = "Point", n)
  pts[[n + 1L]]$x <- 99.0
  return(pts[[1L]]$x)
}
translate(f, args_f = function(n) n |> type(int), types_f = types_f)(3L)
#> Error: vector::_M_range_check: __n (which is 3) >= this->size() (which is 3)
```

**1c. Numeric base-type mismatch** (`REAL`/`INTEGER` macro strictness) reports the
wrong expected type and names neither the argument nor the slot:

```r
# integer matrix passed where double expected
g <- function(A, B) return(A %*% B)
f <- translate(g, args_f = function(A,B){ A|>type(mat(double)); B|>type(mat(double)) })
f(matrix(1:6,2,3), matrix(1:6,2,3))
#> Error: R object is not of type numeric

# character in a double slot -> reports "integer" (wrong), no slot/class name
types_f <- function() new_type(Point, slots(x |> type(double), y |> type(double)))
f <- function(p) { p$x <- p$x + 1; return(p) }
fcpp <- translate(f, args_f = function(p) p |> type(Point), types_f = types_f)
fcpp(structure(list(x = "oops", y = 2), class = "Point"))
#> Error: R object is not of type integer
```

**Suggested direction:** route these throw-sites through an R-framed error that
carries the variable/slot name (the per-variable name-as-template idea covers 1a
and 1b) and, for 1c, name the offending argument/slot and the correct expected
base type. The class-level guard `Expected an object of class 'Point'` already
shows the good standard for struct inputs; per-slot/per-arg validation should match it.

---

## Issue 3 — `fn()` robustness: missing arg leaks internal error; arity mismatch misreported

**3a. Missing `return_value`** (or any `fn` argument) leaks an internal error from
ast2ast's own translation code, unrelated to the user's mistake:

```r
f <- function(x) {
  g <- fn(args_f = function(a) a |> type(double),
          block = function(a) { return(a * 2) })   # no return_value
  return(g(x))
}
translate(f, args_f = function(x) x |> type(double))
#> Error in is.infinite(obj) : default method not implemented for type 'pairlist'
```

Add an argument-presence guard on `fn()` (require `args_f`, `return_value`,
`block`) before body parsing, e.g. "fn: return_value is required".

**3b. `args_f` / `block` arity disagreement** is reported as a call-site "wrong
number of arguments", pointing at the wrong line and blaming the call rather than
the inconsistent definition:

```r
f <- function(x) {
  g <- fn(args_f = function(a, b) { a |> type(double); b |> type(double) },  # 2 args
          return_value = type(double),
          block = function(a) { return(a * 2) })                              # 1 arg
  return(g(x))
}
translate(f, args_f = function(x) x |> type(double))
#> Error: ... return(g(x)) ... Wrong number of arguments for: g
```

A dedicated check ("fn g: args_f declares 2 arguments but block takes 1") would
point at the actual defect.

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

## Issue 5 — Doc/behaviour mismatches (parser is often better than documented)

Several documented rules no longer match behaviour — mostly because the parser
improved. All doc-side fixes except where noted.

**5a. `fn` single-statement block braces.** Docs (vignette + `translate.Rd`) say the
block "must be wrapped in `{ }`, even for a single statement", but the improved
parser accepts it without:

```r
g <- fn(args_f = function(a) a |> type(double),
        return_value = type(double),
        block = function(a) return(a * 2))   # no braces -> works, returns 6
```

Relax the wording.

**5b. `const()` violation.** Docs say `const()` produces a *C++ compile-time* error;
it is actually caught earlier and more cleanly by the static checker
("You cannot assign to a constant variable"). Update wording — the current
behaviour is better than documented.

**5c. `cmr` evaluation-point argument.** Docs say the first argument must be a
"vector of length one", but a length-1 vector triggers a spurious warning while a
true scalar works silently:

```r
# length-1 vector -> warns
#> Warning: time point has more than one element only the first one is used
# true scalar (x <- 2.5) -> no warning, correct result
```

Reconcile: the doc should likely say scalar, and/or the warning should not fire for
length-1 input.

**5d. Undocumented `fn` argument-binding rule.** Passing a non-variable *expression*
(e.g. `x[[1L]]`, `a + b`) to an inner function requires the parameter to be `const`;
non-const params bind only to bare variables. The error is excellent (names arg
position, function, rule, and the exact fix) — this is purely a docs gap:

```r
#> Argument Nr. 1 to function sq accepts only variables. If you want to use an
#> expression (e.g. variable + variable) you have to declare the argument as const
```

Document the lvalue/rvalue binding distinction for `fn` parameters.

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
