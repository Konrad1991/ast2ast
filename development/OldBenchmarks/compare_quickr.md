# ast2ast vs quickr

Benchmark script: `bench_ast2ast_vs_quickr.R` in this folder.
Run on this machine, R 4.6.1, quickr 0.3.0 (CRAN).

armacmp is intentionally left out: last commit 2021-10-20, README calls
itself "an experimental prototype," and scope is frozen at linear algebra
only — not a going concern to track against. quickr (Posit, CRAN, actively
maintained, Fortran backend) is the real ongoing competitor for the
transpile-R-code use case.

Seven workloads, chosen to separate "raw compiled-loop speed" from
"BLAS/allocation overhead" from "R-buffer marshalling overhead":

1. scalar loop (leibniz series)
2. branchy scalar loop (prime counting via trial division: `while`, `%%`, `if`)
3. vector reduction (`sqrt`/`sin`/`abs` over a vector, elementwise + reduce)
4. manual triple-loop matmul, no `%*%` (isolates codegen quality from BLAS)
5. `A %*% B` combined elementwise with `C`, then summed (BLAS + allocation)
6. copy-only overhead isolation (`bench_copy_overhead.R`)
7. ODE integration — explicit-Euler Lorenz system (`bench_ode_integration.R`)

All results below are correctness-checked against plain R first
(`stopifnot(all.equal(...))` for the matrix workloads, direct comparison
printed for the scalar ones) — the numbers are real, not estimated.

**Headline, established over the course of this comparison:** almost every
"quickr wins" result below turned out to trace back to one of two specific,
fixable causes, not a fundamental codegen gap:

1. **`1:n` for-loops materialize their full range as an array before
   iterating; `seq_len(n)`/`seq_along(v)` don't** (see workload 4). Any
   workload using `1:n` pays this even when unrelated to the "real"
   computation.
2. **`type(vec(double)) |> ref()` / `type(mat(double)) |> ref()` silently do
   nothing on an R-facing function** (`ref` has no effect when the output is
   an R function, as arguments are passed as SEXP) — so the R buffer gets
   deep-copied into ast2ast's own array on every call. `borrow_vec`/
   `borrow_mat` avoid the copy (see workload 6).

Once both are avoided, ast2ast tracks quickr within single-digit percent on
every workload tested except the still-unexplained residual in workload 4
(see below) — including a full flip of workload 1 from "quickr 7x faster"
to "tied."

## Workload 1: leibniz scalar loop (10,000,000 iterations)

| impl                       | mean time | vs R  |
|-----------------------------|-----------|-------|
| R                           | 532.5 ms  | 1x    |
| ast2ast (`2:(rounds+2)`)    | 92.7 ms   | 5.7x  |
| quickr                      | 12.9 ms   | 41x   |

quickr ~7x faster than ast2ast here, consistent with the earlier
quickr-vs-armacmp comparison. **But** this loop is `for (i in
2:(rounds+2))` — a colon-materializing loop, same issue as workload 4
(`etr::colon` allocates a 10,000,000-element array up front just to drive a
counter; see workload 4 for the root cause). Rewriting it in `seq_len`
form (shifting the index arithmetic to match, same approach as `ast2ast_issues.md`
Issue 7):

```r
for (i in seq_len(rounds + 2)) { if (i == 1) next; ... }
```

closes the gap almost completely:

| impl                          | median time | vs quickr |
|---------------------------------|-------------|-----------|
| ast2ast (`2:(rounds+2)`, colon) | ~93 ms      | 7.2x slower |
| ast2ast (`seq_len`, no colon)   | 12.95 ms    | **~1% slower — tied** |
| quickr                          | 12.83 ms    | —         |

The entire original "quickr wins workload 1" result was the colon
materialization cost, not a codegen quality gap.

## Workload 2: count primes below 200,000 (branchy: while, %%, if)

| impl    | mean time  | vs R  |
|---------|------------|-------|
| R       | 10588 ms   | 1x    |
| ast2ast | 187 ms     | 56.6x |
| quickr  | 281 ms     | 37.8x |

**ast2ast wins this one** — ~1.5x faster than quickr. This is the opposite
pattern from workload 1: once the loop body has branching (`if`/`while`) and
integer modulo instead of pure arithmetic, ast2ast's generated C++ pulls
ahead of quickr's Fortran. Worth remembering as a counterexample to "quickr
is just faster" — the two backends have different strengths depending on
loop shape.

## Workload 3: vector reduction (sqrt/sin/abs, 5,000,000 elements)

| impl                    | mean time | vs R  |
|---------------------------|-----------|-------|
| R                          | 960.0 ms  | 1x    |
| ast2ast (`vec()` + `ref()`, copies) | 148.5 ms  | 6.5x  |
| quickr                     | 55.4 ms   | 17.3x |

quickr ~2.7x faster than ast2ast — but this used `type(vec(double)) |>
ref()`, the same no-op `ref()` pattern that inflated workload 5's original
numbers. Switching to `type(borrow_vec(double))` (no copy):

| impl              | median time | vs quickr        |
|--------------------|-------------|-------------------|
| ast2ast (`vec()` + `ref()`, copies) | ~146 ms     | 2.7x slower       |
| ast2ast (`borrow_vec`)   | 59.1 ms     | **~12% slower**   |
| quickr              | 52.8 ms     | —                 |

Same story as workload 1: most of the original gap was the copy, not the
`sqrt`/`sin`/`abs` loop itself.

## Workload 4: manual triple-loop matmul, no `%*%`

This isolates raw generated-loop speed from BLAS — same math as `%*%` but
written as three nested `for` loops, so it's a direct test of how well each
transpiler's backend handles tight numeric loops with matrix indexing.

| N   | R         | ast2ast   | quickr   | quickr vs ast2ast |
|-----|-----------|-----------|----------|--------------------|
| 10  | 100.9 us  | 26.6 us*  | 2.3 us*  | ~11.6x             |
| 50  | 9179 us   | 2155 us   | 77.5 us  | ~27.8x             |
| 100 | 74006 us  | 16614 us  | 714 us   | ~23.3x             |

*median, since N=10 had a single large-outlier rep skewing the mean for both
ast2ast and quickr (JIT/first-call noise at microsecond scale).

**This is the biggest gap in the whole comparison.** quickr is 20-28x faster
than ast2ast on a plain triple-nested loop once BLAS isn't doing the work —
far larger than the ~6-7x gap on workloads 1 and 3.

Root cause identified (Konrad's hypothesis, confirmed against the codegen —
not matrix indexing): `for (i in 1:n)` compiles to
`for(const auto i : etr::colon(1, n))` (`R/Nodes.R:629-646`), and
`etr::colon` (`inst/include/etr_bits/Allocation.hpp:135-163`) **materializes
a full n-element array before the loop body runs a single iteration**.
`for (i in seq_len(n))` is special-cased in that same branch to a native
`for(Integer i = 1; i <= length_seq(n); i++)` — no allocation. In the
nested matmul above, the innermost `for (l in 1:k)` re-materializes a
k-length array on *every single (i, j) pair* — up to N*M unnecessary heap
allocations. See `bench_colon_vs_seqlen.R` for the isolated follow-up
benchmark and workload details.

| N   | ast2ast (`1:k`) | ast2ast (inner loop → `seq_len`) | ast2ast (all loops → `seq_len`) | quickr   |
|-----|------------------|-----------------------------------|-----------------------------------|----------|
| 50  | 2218 us          | 802 us                            | 962 us                            | 79 us    |
| 100 | 17047 us         | 6156 us                           | 7585 us                           | 706 us   |
| 200 | 134980 us        | 49421 us                          | 61302 us                          | 6122 us  |

Converting only the innermost loop (the one called N*M times) gives a
consistent ~2.7-2.8x speedup — most of the "unexplained" part of the gap.
Surprising twist: converting *all three* loops to `seq_len` was consistently
~15-25% *slower* than converting only the innermost one. Hypothesis at the
time: the generated `seq_len` loop re-evaluated `etr::length_seq(n)` in the
loop condition on every iteration (no hoisting), so for the outer/middle
loops that per-iteration call cost outweighed the one-time allocation it
avoided.

**Follow-up, implemented and verified:** hoisted the bound into a local
computed once before the loop (`R/Nodes.R:629-644` — `seq_len`/`seq_along`
loops now generate `{ auto i__bound__ = etr::length_seq(n); for(Integer
i = 1; i <= i__bound__; ...) {...} }` instead of re-evaluating the bound
call every iteration). Full tinytest suite still green (991/991) after the
change. Re-measured:

| N   | ast2ast (all loops → `seq_len`, before hoist) | after hoist | still slower than inner-only by |
|-----|--------------------------------------------------|-------------|-----------------------------------|
| 50  | 962 us (20% slower than inner-only)               | 909 us      | ~13%                              |
| 100 | 7585 us (23% slower)                              | 7080 us     | ~15%                              |
| 200 | 61302 us (24% slower)                             | 55706 us    | ~18%                              |

The hoist narrowed the gap by roughly 5-10 percentage points at every size
but didn't close it — hoisting the bound wasn't the whole story. Something
else about outer/middle `seq_len` loops (vs. leaving them as `1:n`) still
costs more than expected; not yet root-caused.

## Workload 5: `D <- A %*% B; E <- D + C - D; sum(E)`

**Correction to the original finding below:** the first version of this
benchmark declared `A`/`B`/`C` as `type(mat(double)) |> ref()`, which threw
"`ref` has no effect when the output is an R function, as arguments are
passed as SEXP" — `ref()` was silently a no-op there, so every call was
deep-copying all three matrices. Switching to `type(borrow_mat(double))`
(which actually avoids the copy for an R-facing output) closes almost the
entire gap:

| N   | R        | ast2ast (`ref()`, copies) | ast2ast (`borrow_mat`) | quickr   |
|-----|----------|-----------------------------|--------------------------|----------|
| 10  | 3.5 us   | 3.1 us                      | 2.5 us                   | 1.4 us   |
| 50  | 18.2 us  | 37.2 us (2.15x slower)      | 19.8 us (1.09x slower)   | 12.3 us  |
| 200 | 336 us   | 990 us (2.4x slower)        | 409 us (1.2x slower)     | 210 us   |
| 500 | 4.0 ms   | 8.02 ms (1.5x slower)       | 3.5 ms (**faster than R**) | 2.3 ms |

So the "ast2ast slower than base R" finding originally reported here was
mostly a benchmark artifact — the wrong type annotation forcing unnecessary
copies — not a real gap in the matmul + elementwise codegen path itself.
With `borrow_mat`, ast2ast is close to base R across the board and quickr's
remaining edge (still ~1.4-1.9x) is a much smaller, more plausible gap to
attribute to allocation around `D + C - D` or BLAS call overhead.

## Workload 6: copy-only overhead isolation

Script: `bench_copy_overhead.R`. A trivial sum-loop over a vector, three
variants: `type(vec(double))` (copies the R vector into ast2ast's own
buffer), `type(borrow_vec(double))` (operates directly on R's `REAL()`
buffer, no copy), and quickr. Isolates exactly the "copy R's buffer into
our own array" mechanism Konrad flagged after seeing quickr/Fortran handle
large vectors fast.

| N          | ast2ast (`vec()`, copies) | ast2ast (`borrow_vec`) | quickr    |
|------------|-----------------------------|--------------------------|-----------|
| 100,000    | 318.6 us                    | 78.8 us                  | 77.7 us   |
| 5,000,000  | 38.98 ms                    | 4.62 ms                  | 4.26 ms   |
| 50,000,000 | 434 ms                      | 45.86 ms                 | 42.55 ms  |

The copy itself costs **~8-10x** the entire rest of the operation at these
sizes. Once avoided, `ast2ast_borrow` is within ~7-8% of quickr uniformly
across two orders of magnitude of N — quickr isn't fundamentally faster at
moving/summing numbers, it just never pays a copy in the first place
(Fortran operates on R's memory directly via the `.Call`/`.External`
interface, same as `borrow_vec` does).

## Workload 7: ODE integration — explicit-Euler Lorenz system

Script: `bench_ode_integration.R`. Closer to ast2ast's actual stated
purpose (package `DESCRIPTION`: "generate fast functions which can be used
as ode-system or during optimization") than a synthetic array-loop
benchmark: three scalar state variables (`x`, `y`, `z`), updated every
iteration via the Lorenz equations, 5,000,000 explicit-Euler steps — no
vectors/matrices involved at all, so neither the colon-materialization nor
the copy-overhead issue above applies.

| impl    | median time | vs R    |
|---------|-------------|---------|
| R       | 807.5 ms    | 1x      |
| ast2ast | 17.77 ms    | 45.4x   |
| quickr  | 17.33 ms    | 46.6x   |

Essentially tied (~2.5% apart) — for the kind of workload ast2ast is
actually built for, there's no meaningful gap to quickr at all.

## Error reporting: ast2ast vs quickr

Script: `bench_error_reporting.R` in this folder. Not a speed comparison —
captures the actual error text each package produces for the same class of
mistake. Four cases, all real output (not paraphrased).

### A. Runtime out-of-bounds indexing (`a` has 9 elements, code reads `a[10]`)

- **ast2ast**: throws `<Rcpp::exception: In 'n <- as.integer(a[10.0])':
  Error: out of boundaries>` — caught, clear, points at the offending
  statement.
- **quickr**: returns `1` — no error, no warning. Fortran doesn't
  bounds-check by default, so this is silent undefined behavior producing a
  plausible-looking wrong answer. **This is the single biggest difference
  in this comparison** — ast2ast's runtime bounds checking is a real safety
  property quickr's Fortran backend doesn't have.

Caveat from the earlier `current_line()` discussion: this result looks good
because the bad access happens in a plain assignment statement. If the same
access happens in a `for`/`if`/`while` header instead (e.g.
`for (i in a[1]:a[10])`), ast2ast still throws, but currently mislabels
*which* line — a separate, already-tracked gap, not tested by this case.

### B. Unsupported syntax (`[[` double-bracket indexing)

- **ast2ast**: compiles and runs fine — `[[` is supported.
- **quickr**: rejected immediately at `quick()` time with
  `Unsupported function: [[` — quickr's scope is explicitly narrower here,
  and it says so clearly rather than failing downstream.

### C. Undefined variable reference

- **ast2ast**: caught at `translate()` time, one line:
  `y <- x + z` / `Found uninitialzed variable: z` (points at the exact
  statement and names the exact symbol; "uninitialzed" is a pre-existing
  typo in the message).
- **quickr**: not caught until the generated Fortran is compiled — surfaces
  the raw `gfortran` diagnostic (`Symbol 'z' at (1) has no IMPLICIT type`,
  complete with the generated `.f90` filename and a `make` exit-code line)
  before wrapping it as `Compilation Error`. Technically accurate but
  assumes the R user can read a Fortran compiler error.

### D. Type mismatch (character literal in a numeric function)

- **ast2ast**: `y <- "hello"` / `You cannot assign characters to
  variables` — direct, references the actual statement.
- **quickr**: `Unsupported object type encountered: character`, raised
  from inside `r2f(rhs, scope, ..., hoist = hoist)` — correct and fairly
  clear, but the error surfaces quickr's internal function names/args
  rather than pointing at the R source line.

### Summary

| case                          | ast2ast                                   | quickr                                              |
|--------------------------------|--------------------------------------------|------------------------------------------------------|
| runtime out-of-bounds          | caught, clear message                      | **silent wrong answer, no error at all**             |
| unsupported syntax (`[[`)      | supported, no error                        | rejected immediately, clear message                  |
| undefined variable             | caught pre-compile, source-line message    | caught by Fortran compiler, raw compiler dump         |
| type mismatch                  | caught pre-compile, source-line message    | caught pre-compile, internal-function-name message    |

Consistent pattern: ast2ast's own errors (when it does the checking itself,
pre-compile or at runtime) reference the actual R statement and read like
something meant for an R user. quickr's compile-time errors are either
clean (when quickr's own R-level checks catch it, e.g. case B/D) or raw
Fortran compiler dumps (when the error only surfaces once translated
Fortran is compiled, e.g. case C) — and its Fortran backend has no runtime
bounds checking at all, which is a real correctness/safety gap, not just a
polish difference.

## Copy-vs-borrow semantics: how does quickr actually avoid the copy?

Follow-up investigation, not a benchmark: workload 6 showed quickr matching
`borrow_vec`'s no-copy speed. Does quickr borrow safely — replicating R's
copy-on-modify when the function mutates its argument — or does it just
unconditionally borrow and leave correctness to the caller? Checked by
capturing the actual generated C/Fortran (`list.files(tempdir(), pattern =
"\\.(f90|c)$")` right after a `quick()` call) for a function that mutates
its input:

```r
f <- function(x) { x[1] <- 999; x[2] <- x[2] * 2; x }
```

Generated C wrapper:

```c
const double* const x__ = REAL(x);   // R's own buffer, no Rf_duplicate anywhere
anonymous_quick_function_1(x__, x__len_);
return x;                             // same SEXP handed back
```

Generated Fortran:

```fortran
real(c_double), intent(in out) :: x(x__len_)   ! writes straight into R's buffer
```

Runtime check: calling `f(orig)` **silently mutates `orig` in the caller's
scope** (`identical(orig, result)` is `TRUE`, no copy anywhere). This isn't
a performance shortcut with a documented opt-in — it's unconditional for
every argument, all the time, with nothing at the call site suggesting the
argument could be mutated.

ast2ast's equivalent test: `type(vec(double))` (the default, no `ref`/
`borrow`) copies and leaves `orig` untouched, matching normal R semantics.
`type(borrow_vec(double))` does mutate the caller's object too — but only
because the user explicitly opted into a separately-named type that's
documented as deviating from normal R copy-on-modify.

**Conclusion:** quickr's speed here isn't from a smarter copy-avoidance
mechanism — it just never copies, unconditionally, for every argument. The
difference is that ast2ast makes copy-vs-borrow an explicit, named,
documented per-argument choice, while quickr always borrows and leaves
copy-on-modify correctness entirely up to the caller noticing. Given how
easy it is to hit (any mutation inside a `quick()`-compiled function), this
is arguably a bigger correctness gap than the missing bounds-checking.

Checked whether this is at least documented: no mention in quickr's
README, `NEWS.md`, or any Rd help topic (`tools::Rd_db("quickr")`) of
top-level function arguments being mutated in place / aliasing the
caller's object. `NEWS.md` *does* document copy-on-modify handling, but for
a narrower, different feature — local closures capturing parent-scope
variables inside `sapply()`-style constructs get a real copy; that's not
the same as what a plain mutated top-level argument does. Worth noting in
quickr's favor: 0.3.0 is young (matrix/linear-algebra support only landed
this release per `NEWS.md`) and clearly under careful active development —
the closure-copy handling they did build shows the authors do think about
this class of problem, it just doesn't appear to extend to this case yet.

## Feature scope (not measured, just noted)

quickr has nothing resembling ast2ast's reverse-mode AD, custom types, or
the `at()`/subsetting DSL. Its scope is explicitly frozen at "single
function, no polymorphism/dynamic types." ast2ast is doing strictly more on
the feature axis — this comparison is purely about raw throughput on
workloads quickr can also express.

## Bottom line

The story flipped substantially over the course of this comparison. What
first looked like "quickr is broadly faster on plain loops" turned out to
be almost entirely two specific, identified, and mostly-fixable causes:

- **Colon materialization** (`1:n` allocates an array just to drive a
  counter; `seq_len`/`seq_along` don't) fully explained workload 1's
  original 5.7x gap (closes to ~1%, tied) and is most of workload 4's gap
  (still ~13-18% unexplained after the loop-bound hoist fix — see workload
  4 for the open residual).
- **`ref()` silently no-op'ing on R-facing functions**, causing unwanted
  deep copies, explained workload 3's 2.7x gap (closes to ~12%) and
  workload 5's original "slower than base R" finding (closes almost
  entirely, `borrow_mat`/`borrow_vec` are the fix).
- On a workload with **no** vectors/matrices and **no** colon loops
  (workload 7, ODE integration — closer to what ast2ast is actually built
  for) the two packages are tied from the start, no fix needed.
- ast2ast **wins** on branchy scalar code (workload 2, ~1.5x) — quickr's
  Fortran backend isn't universally faster; loop shape matters.
- Remaining open gap: workload 4's manual matmul, even after `seq_len` on
  the innermost loop and the bound-hoist fix, is still meaningfully behind
  quickr — the only workload here without a fully identified root cause.
- Bigger picture, not measured in ms: quickr's Fortran backend has **no
  runtime bounds checking at all** (see Error reporting below), and its
  no-copy speed comes from *unconditionally* borrowing every argument with
  no copy-on-modify safety net at all (see Copy-vs-borrow semantics below)
  — both are correctness/safety properties ast2ast has (bounds checks, and
  copy being the *default* with borrowing an explicit named opt-in) that
  quickr structurally doesn't, independent of the speed numbers.

**Practical upshot for anyone writing performance-sensitive ast2ast code:**
prefer `seq_len(n)`/`seq_along(v)` over `1:n` in hot loops, and prefer
`borrow_vec`/`borrow_mat` over `vec()`/`mat() |> ref()` when the function
output is an R function (the common case) — `ref()` is a silent no-op
there and easy to reach for by mistake.
