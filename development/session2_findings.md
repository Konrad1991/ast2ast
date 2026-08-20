# ast2ast — second exploration pass (post "fn checks" update)

## diag — documentation gap (not a bug)

`diag` is registered as a strict 3-arg **constructor** `diag(x, nrow, ncol)`;
extraction is the separate `get_diag(matrix)`. So every R-habit single-arg form
fails with only an arity error:

```r
diag(v)      #> Wrong number of arguments for: diag
diag(A)      #> Wrong number of arguments for: diag   (use get_diag(A))
diag(3L)     #> Wrong number of arguments for: diag
diag(v,3L,3L)#> works -> diagonal matrix
```

The docs list both `diag` and `get_diag` under linear algebra without signatures.
Suggest documenting: `diag(x, nrow, ncol)` constructs; `get_diag(matrix)`
extracts. (`get_diag` on a non-matrix already gives a good error: "You can only
call get_diag on a matrix".)

## Matrix print routes through the array printer (cosmetic)

`print(matrix)` emits a spurious/malformed N-D slice header before the grid:

```
print(matrix(1:6, 2, 3)) ->
, , 1, 1            <- should not appear for a 2-D matrix
1   3   5
2   4   6
```

3-D array print is correct (`, , 1` / `, , 2` slice headers). Only the 2-D case
inherits a wrong header. Data is correct; header line is the defect.

## Negative indexing — misleading message (minor)

`v[-1L]` (R's drop-first) gives `In 'return(v[-1L])': Error: out of boundaries`.
Negative/omission indexing is reasonably unsupported, but the message reads as an
OOB access rather than "negative indexing is not supported". Attribution now names
the statement, which helps.

## UPDATE — root cause of the int/double element issue is type back-propagation

Deeper probing reworked the earlier "int-element -> double-element" finding into
two distinct issues with a shared origin in type inference.

### Boundary map (all minimal)

| source            | target            | result |
|-------------------|-------------------|--------|
| `iv[i]` (IntRef)  | `dv[j]` (DblRef)  | **FIXED** — was: compile fail `no match for operator= (DoubleRef = IntegerRef)` |
| `lv[i]` (LogRef)  | `dv[j]` (DblRef)  | **FIXED** — was: compile fail `DoubleRef = LogicalRef` |
| `iv[i]` (IntRef)  | `d` (Dbl scalar)  | **FIXED** — was: compile fail `Double = IntegerRef` |
| int **scalar**    | `dv[j]` (DblRef)  | works |
| `dv[i]` (DblRef)  | `iv[j]` / int scalar | works, does NOT truncate (see below — separate, still-open bug) |
| `dv[i] + iv[j]`   | (arithmetic)      | works (promotes fine) |
| `dv <- iv`        | whole vector      | works |

The `Ref`-layer compile gap is fixed: `LogicalRef`/`IntegerRef`/`DoubleRef`/
`DualRef::operator=` now accept any other `*Ref` source, and the
`as_logical`/`as_integer`/`as_numeric` casts (`Casts.hpp`) were fixed the same
way (both had the identical "two chained user-defined conversions" root
cause). Tests: `inst/tinytest/test_subsetting.R`.

### The deeper cause: inference silently widens a declared type

`iv <- integer(3)` alone generates `Array<Integer, ...>` (correct). But if the
same function later contains `iv[1] <- dv[1]` (a double element), the **whole
variable is re-inferred to `Array<Double, ...>`** — visible in `getsource`:

```
iv <- integer(3); iv[1] <- 5L            -> Array<Integer, Buffer<Integer>>  (correct)
iv <- integer(3); iv[1] <- dv[1]         -> Array<Double,  Buffer<Double>>   (widened!)
```

Consequences:

- `iv[2] <- 99L` then stores into a double, and `iv[1] <- dv[1]` returns `5.7`,
  not `5` — **no truncation**, because `iv` is not integer anymore.
- This contradicts the documented rule "Once a variable has a type, it cannot
  change its base type or structure." Here a later mixed assignment changes it.

### Two possible intended designs (author's call)

1. **Types are fixed (per docs):** then `iv[1] <- dv[1]` into an `integer` `iv`
   should be a **static error** ("cannot assign double to integer variable"),
   in the same family as the const-assignment error — not a silent widen.
2. **Mixed assignment allowed:** then the silent widening of an
   `integer()`-constructed variable should at least be documented as an
   exception to the immutability rule. (The `Ref`-layer compile gap this
   option also named is now fixed — see the table above.)

Either way, the current outcome (silent widening + lost truncation) is
inconsistent with the docs. The user-facing workaround today is whole-vector
conversion (`tmp <- int_vec` into a pre-declared double vector) rather than
element-wise.

---

## Correction & contrast: struct slots enforce types correctly (locals do not)

A double->int **struct slot** assignment was briefly suspected of dropping a
field; on inspection it is correct. `r$i <- r$d` (double into an `int` slot)
truncates properly (`5.7 -> 5`), preserves both slots, and the generated struct
declares both members with their declared types and marshals both back. The
earlier "missing field" was a `print` display artifact; `str()` shows both.

This is a useful contrast for the local-variable widening bug above: **slots have
fixed, enforced types** (double->int truncates, type kept), while **plain locals
get silently widened** to double by a later mixed assignment. Since the slot path
does the correct thing, the local-variable widening looks like an inference bug
rather than intended behavior — the two paths should agree (either both truncate
with a fixed type, or both error).

---

## Additional findings (this pass)

### Polymorphic-in-R functions are single-form here (doc theme)

Several base-R functions that are overloaded/polymorphic accept only one form in
ast2ast, and the rejection is a bare arity/invalid error with no hint of the
supported form. A short "supported signature / use X instead" note in the docs
would remove the friction. Confirmed cases:

- `max(a, b)` / `min(a, b)` (two scalars): `Wrong number of arguments for: max`.
  Supported form is the reduction `max(c(a, b))` (verified -> 7).
- `diag(v)` / `diag(A)` / `diag(3L)`: arity error. Constructor is
  `diag(x, nrow, ncol)`; extraction is `get_diag(matrix)`.
- `crossprod(X, Y)` (two-arg): arity error (from first pass). Only `crossprod(X)`
  is supported; use `t(X) %*% Y`.
- `pmax`/`pmin`: `Invalid function pmax` — not supported at all (not in the docs
  list, so correct; noting for completeness).

### String variables unsupported — good message, undocumented

`msg <- "hello"` gives `You cannot assign characters to variables` (attributed).
Correct behavior — strings are only valid as `print`/`stop` literals — but the
docs don't state that string *variables* are unsupported. Suggest a one-line note.

### Confirmed working (no issue) this pass

- Reverse AD through transcendentals (`sin`/`exp`/`^`), through `if` branches
  (correct per-branch derivative), and through loop-carried accumulation
  (Horner) — all exact.
- Nested collection-of-structs with chained subset (`poly$pts[[i]]$x`) in a loop.
- `for (i in seq_along(v))` as a loop iterator; `rep`; int counter in double
  arithmetic (`i * 0.5`); logical-mask indexing `v[mask]`; matrix row extraction
  `M[i, ]`; while/repeat/next/break; `%%`/`%/%`; seq_len/seq_along alone.
- Double->int **struct slot** assignment truncates and preserves type correctly.

### which() missing while which.max/which.min present (minor asymmetry)

`which(v > 3)` gives `Invalid function which`, though `which.max`/`which.min`
are supported. A user seeing the two cousins will likely expect `which`. Either
add it or note the omission.

### borrow_mat return also works

`borrow_mat(double)` mutated in place and returned compiles and runs (`2 4 / 3 5`),
confirming the borrow-return (`na_p`) fix covers matrices too, not just vectors.

---

## Sharpened diagnosis: only locally-constructed variables get widened

Testing all four ways a variable's type originates:

| origin                        | double assigned to int target | behaviour |
|-------------------------------|-------------------------------|-----------|
| `type(vec(int))` **argument** | `iv[1] <- 2.5`                | truncates -> 2 (correct) |
| `int` **struct slot**         | `r$i <- r$d`                 | truncates -> 5 (correct) |
| `integer(n)` **local**        | `iv[1] <- dv[1]`             | **widened to double**, no truncation |
| `integer(n)` **local**, then int-only use | `iv[1] <- 5L`     | stays Integer (correct) |

So the type is correctly fixed and enforced for arguments and slots; **only a
locally-constructed variable gets its declared base type silently widened** when a
later statement assigns a foreign-typed element into it. That is the single
offending path. The compile failure (`DoubleRef = IntegerRef`) is the flip side:
when inference does NOT widen (e.g. target is a separately-typed double and source
is an int element), no cross-type `Ref` conversion exists. Both stem from the
local-inference layer treating element-level cross-type assignment inconsistently
with how arguments/slots handle it.

## numeric(0) / zero-length allocation is a hard error

`v <- numeric(0L)` -> `In 'v <- numeric(0L)': size ... has to be a positive
integer`. R allows empty vectors (common as accumulator seeds). ast2ast requires
a positive size. Consistent with static sizing (vectors can't grow anyway), but a
divergence worth a doc note; message is clear and attributed.

## More confirmed-good (attributed runtime errors)

- Singular `solve`: `In 'return(solve(A, b))': solve: matrix is exactly singular`.
- Inf/-Inf/NaN propagate per IEEE; `is.nan`/`is.finite` on computed values correct.

---

## Structure change vector -> scalar is silently allowed

`v <- numeric(3); v <- 7.0` reassigns `v` from a length-3 vector to a scalar and
`getsource` shows the variable simply re-bound (`v = etr::Double(7.0)`), returning
`7`. R semantics arguably allow this (rebinding), but it violates the documented
"Once a variable has a type, it cannot change its base type or structure" rule
(here structure changes vector -> scalar). Same local-inference looseness as the
base-type widening. Consistent handling would either forbid it or document the
exception.

## Recycling not supported (good message)

`a + b` with lengths 4 and 2 -> `In 'return(a + b)': encountered non-conformable
arrays`. R would recycle; ast2ast requires conformable lengths. Defensible
(recycling hides bugs) and clearly reported; worth a doc note.

## C++/R semantic divergences (intentional, worth documenting)

A small family of defensible differences that a user porting R code could hit
silently. Suggest a "differs from R" doc subsection collecting them:

- **Integer overflow wraps** (two's complement): `2147483647L + 1L -> -2147483648`.
  R gives `NA` with a warning.
- **Recycling unsupported**: mismatched-length vector ops error
  (`non-conformable arrays`) rather than recycling.
- **`numeric(0)` / zero-length allocation is an error** (positive size required).
- **String variables unsupported** (`You cannot assign characters to variables`);
  strings only valid as `print`/`stop` literals.
- Scalars are true scalars, not length-1 vectors (already documented).

Values that DO match R: `sqrt(-x) -> NaN`, `log(0) -> -Inf`, `log(-x) -> NaN`,
Inf/-Inf/NaN propagation.

Well-attributed runtime errors this pass: singular solve, matmul non-conformable
(`cols(A) != rows(B)`), 1-D and large N-D out-of-bounds, non-conformable arrays.
(Minor cosmetic: attributed statement shows prefix/translated form, e.g.
`%*%(A, B)` for `A %*% B`, `x[1.0]` for `x[1]`.)

## Final probes

- **Forward-mode missing `unseed` still silently contaminates** (old issue #5,
  unchanged): dropping `unseed(x,i)` yields `[[3,5],[1,7]]` instead of
  `[[3,2],[1,6]]`, no error. Inherent to the explicit AD design and now
  well-documented, but the failure mode is silent wrong numbers. (Author aware.)
- **Wrong arg count from R** leaks the internal SEXP arg name:
  `argument "bSEXP" is missing, with no default` (should reference `b`). Minor
  cosmetic — it does error correctly.
- **Scalar passed where `vec(double)` expected** is accepted as a length-1 vector
  (`sum(5.0) -> 5`). Lenient at the R boundary; reasonable, not a problem.

---

# Priority summary (this pass)

**Correctness bugs (silent wrong results — highest priority):**
1. **Local variable base-type silently widened** by a later mixed-type element
   assignment (`integer()` local -> Double), losing truncation and contradicting
   the documented immutability rule. Arguments and struct slots do NOT have this
   bug — only locals. (The compile-failure flip side, `IntegerRef/LogicalRef ->
   Double` element assignment, is fixed — see top.)

**Cosmetic / message:**
2. Matrix `print` emits a malformed `, , 1, 1` array header.
3. Wrong-arg-count leaks `bSEXP` internal name.
4. Attributed statements show translated/prefix form (`x[1.0]`, `%*%(A,B)`).

**Doc gaps (single-form functions + divergences):**
5. `max`/`min` two-arg, `diag` (constructor vs `get_diag`), `crossprod` two-arg —
   arity errors with no pointer to the supported form.
6. `which` missing while `which.max`/`which.min` present.
7. String variables unsupported; `numeric(0)` disallowed; integer overflow wraps;
   recycling unsupported — collect under a "differs from R" doc section.

**Acknowledged / intentional:** static-checker multiple emission; fn args_f/block
arity at call site; forward-mode unseed contamination.

## Session 2 — extended probes

### backsolve/forwardsolve don't check for zero diagonal (silent Inf)

`backsolve(R, b)` with a zero on the triangular diagonal returns `Inf` (division
by zero) rather than erroring on the singular system:

```r
backsolve(matrix(c(0,0,1,2),2,2), c(1,1))  -> Inf 0.5   (R would error)
```

Less severe than the bounds bug (Inf is at least visible), but another silent
numeric divergence. Contrast: `solve` and `chol` both detect singular/non-PD and
give clean attributed errors. Suggest a zero-diagonal check in the triangular
solvers to match.

### Inner functions do not capture outer locals (language limitation)

An `fn` body cannot see variables from the enclosing function scope:

```r
f <- function(x) {
  k <- 10.0
  g <- fn(..., block = function(a) return(a + k))   # k not visible
  return(g(x))
}
#> In inner function g: return(a + k) -- Found uninitialzed variable: k
```

Error is clear and attributed. This is a reasonable design (closed scopes, no
lexical capture — pass values as arguments), but it is not documented; a user
familiar with R closures will expect capture. Suggest a one-line doc note that
`fn` bodies are closed scopes.

### Typo persists: "uninitialzed" -> "uninitialized"

Still present in the uninitialized-variable error (both top-level and inner-fn).

### Confirmed-good (attributed / correct)

- `chol` non-PD: `chol: the matrix is not positive definite` (attributed).
- `cmr` Catmull-Rom: hits knots exactly (`cmr(2)=1`, `cmr(3)=0`), smooth between.

### uniroot maxiter must be double, not integer literal (+ error cascade)

`uniroot(g, interval, tol, maxiter)` rejects an integer `maxiter`:

```r
uniroot(g, interval, 1e-10, 100L)
#> The fourth argument (maxiter) to uniroot has to be a scalar double
uniroot(g, interval, 1e-10, 100)     # double -> works
```

`maxiter` is conceptually an iteration count, so requiring `100` not `100L` is a
surprising constraint (same REAL/INTEGER strictness family). Accept integer, or
document. NOTE the real error correctly names the problem, but it **cascades**:
the failed type-check leaves `res` untyped, so the next line adds
`Found uninitialzed variable: res`. The genuine message is present but can be
buried by the downstream noise (easy to misread as a `res` problem). Minor: when
a statement fails type inference, suppressing the cascade of dependent-variable
errors would surface the root cause more clearly.

### nnls on a zero matrix returns all-zeros (correct)

### Composite algorithm capstone (all correct)

- **One k-means iteration** (matrix row indexing + vector accumulators +
  branching + distance + division): exact centroids `(0.25,0.25)`,`(10.25,10.25)`.
  A realistic "difficult algorithm an R user would write" — works with clean
  R-like syntax end to end.
- **Deeply nested write** `o$items[[1L]]$vals[2L] <- 99` (collection of structs,
  struct holds a vector, write into a vector element two `$` levels deep): correct.

---

## Overall assessment after two passes

The runtime-error surface that dominated pass 1 is essentially fixed: attributed
`In '<stmt>': ...` messages (reaching into inner fn bodies), named type
mismatches, cleaned collection OOB, fn presence guards, and the borrow-return
compile bug (vec + mat). AD is robust (reverse/forward, transcendentals,
branches, loop-carried, nested structs). Real composite algorithms (Gauss-Newton,
k-means) work with idiomatic syntax.

Remaining, by severity:

- **Correctness (silent wrong results):** (1) local-variable base-type widening
  — has a clean fix and a clear "correct" reference elsewhere in the codebase
  (args/slots enforce types). (2) backsolve/forwardsolve zero-diagonal -> Inf.
- **Doc:** single-form functions (max/min/diag/crossprod/which), R-divergence
  cluster (int overflow, recycling, numeric(0), string vars), fn closed scopes,
  uniroot maxiter-is-double.
- **Cosmetic:** matrix print header, bSEXP leak, prefix-form attributions,
  "uninitialzed" typo, error cascade after a failed type-check.
- **Acknowledged/intentional:** multiple emission, fn arity at call site,
  forward-mode unseed contamination.

## Round 3 probes

### Function with no explicit return yields NULL (not the last value)

`f <- function(x) { y <- x + 1 }` returns `NULL`, where R returns the last value
(6) invisibly. Minor divergence — an assignment as the final statement does not
become the return value. Most code uses explicit `return()`; worth a doc note.

### Confirmed-good (round 3)

- Operator precedence matches R, including `-2^2 == -4` (unary minus below `^`)
  and `2 + 3*4 == 14`.
- `while (TRUE) { ... break }` terminates correctly.
- Reduction-to-scalar then arithmetic (`sum(v) * 2`).

### CONSOLIDATED: the documented immutability rule is not enforced for locals

The docs state: "Once a variable has a type, it cannot change its base type or
structure." For **local variables** this is not enforced — three manifestations,
one root cause (local type inference re-derives/overwrites a variable's type from
later assignments instead of fixing it at first assignment):

1. Base-type widening: `iv <- integer(3); iv[1] <- dv[1]` -> iv becomes Double.
2. Structure change vector->scalar: `v <- numeric(3); v <- 7.0` -> scalar (returns 7).
3. Structure change scalar->vector: `x <- 5.0; x <- c(1.0,2.0)` -> vector (returns 1).

Arguments and struct slots DO enforce their types (they truncate/convert or error
appropriately). So the fix is to make local first-assignment types fixed and
subsequent conflicting assignments an error (matching the documented rule and the
arg/slot behavior) -- or, if reassignment-with-retyping is intended, update the
docs to say the rule applies only to arguments and slots, not locals.

## BUG — const propagates through struct copy assignment (but not scalar)

Copying a `const` struct into a new variable makes the copy const too, so it
cannot be modified -- breaking the natural "copy a read-only input, modify the
copy, return it" pattern:

```r
scale2 <- fn(args_f=function(q) q |> type(Vec2) |> const(),
             return_value=type(Vec2),
             block=function(q) { r <- q; r$x <- q$x*2.0; return(r) })
#> r$x <- q$x * 2.0
#> You cannot assign to a constant variable
```

Inconsistent with scalars, which do NOT propagate const:

```r
block=function(a) { b <- a; b <- b + 1.0; return(b) }   # a is const double
# -> works, returns 6 (independent mutable copy)
```

And with a non-const struct arg, `r <- q; r$x <- ...` works fine. So the bug is
specific to `const struct -> copy`: the copy inherits const-ness. It should be an
independent mutable copy, matching the scalar case and R copy semantics. This
leaves a genuinely const struct argument with no straightforward way to produce a
mutable working copy.

### Refinement: const-struct-copy has a clean workaround (type() annotation)

A fresh local struct can be declared via `r |> type(Vec2)` (no constructor call),
then fields assigned individually — this avoids the const-copy bug (verified:
const `q`, `r |> type(Vec2)`, copy fields -> x=2, y=4). Note: calling the type as
a constructor (`Vec2(0.0, 0.0)`) is NOT valid (`Invalid function Vec2`); the
annotation form is the way, and collection-of-length-1 also works. So the
const-propagation bug is a correctness/consistency issue with an available
workaround, not a blocker. Worth documenting the `type()`-declares-a-local-struct
pattern regardless, since it isn't in the docs.

### Round 3 confirmed-good

- `tcrossprod` (X %*% t(X)), `prod`, non-square transpose (2x3->3x2) all match R.
- Forward AD assembling a Jacobian row (`x1^2 + x2^3` -> 4, 27) exact.
- `type(Vec2)` annotation declares a fresh local struct (undocumented but works);
  collection-of-length-1 also works. `Vec2(...)` constructor call is invalid.

## The 1:n zero gotcha is faithfully reproduced (R semantic, not a bug)

`for (i in 1L:n)` with n=0 runs **twice** (i=1, then i=0), because `1:0` is
`c(1,0)` counting down — exactly as in R. ast2ast returns 2 for the count, i.e. it
correctly reproduces the R footgun. Not a bug (faithful translation), but since
`seq_len`/`seq_along` are supported, a doc note recommending `for (i in
seq_len(n))` (which gives 0 iterations for n=0) would help users avoid it.

## Round 4 confirmed-good

- Reverse AD for a 5-input / 3-output system: full Jacobian exact.
- Zero-iteration while (condition false at entry) -> 0.
- Deep recursion `fib(25) = 75025`.
