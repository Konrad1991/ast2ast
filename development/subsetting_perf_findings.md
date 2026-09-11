# Strided subsetting perf investigation (2026-09-11)

Context: [[project_strided_subsetting_runtime]] shipped a runtime stride-detection fast
path for preserving subsetting. Benchmarked against quickr via `diffuse.R`
(`development/Benchmarks/diffuse.R`/`diffuse.cpp`, nx=ny=100, steps=500) — no measurable
change (ast2ast ~300ms before and after, quickr ~19ms). Investigated with a chrono-
instrumented copy of the benchmark, first via Rcpp (`diffuse_rcpp.cpp`), then standalone
(`development/try_stuff.cpp`, `#define STANDALONE_ETR`) once R's hardened build flags
became a confound.

## Ruled out, in order

1. **`fn()` call/heap-alloc overhead** ([[project_fn_call_overhead]]) — doesn't apply, the
   emitted `apply_boundary_conditions`/`update_temperature` already `returntype(void)` +
   mutate `temp` by reference.
2. **Integer division in `StridedLayout::offset_of`** — forcing `all_strided = false`
   (fallback flat-buffer path, no division) gave identical timing (228ms vs 229ms) on the
   full benchmark. Division is not the differentiator for the full formula.
3. **Per-`subset()`-call heap allocation** (per-dimension `Buffer<Integer>` materialization,
   since `i`/`j`'s size never equals the full dim extent) — isolated: construction alone
   (500 `subset()` calls, result discarded) cost 0.4-0.5ms; access alone (same `SubsetView`
   built once, reused for 500 reps) cost ~37ms. Construction is not the cost.
4. **Compiler hardening flags** (Ubuntu's gcc defaults to
   `-fstack-protector-strong -fstack-clash-protection -fcf-protection -D_FORTIFY_SOURCE=3`,
   confirmed via `gcc -dumpspecs`, silently injected even with a bare `-O2` command) —
   explicitly stripped with `-fno-stack-protector -fno-stack-clash-protection
   -fcf-protection=none -U_FORTIFY_SOURCE`: zero measurable difference (244ms vs 246ms).
5. **`-O2` vs `-O3`** — no improvement (`-O3` was if anything slightly worse: 327ms vs
   244ms on the laplacian bucket, within the range of noise/unrolling effects).

## Confirmed findings

### 1. `ConstHolder`'s rvalue constructor heap-allocates for *any* rvalue operand — FIXED

`BinaryOperation`'s constructor moves an rvalue scalar operand (e.g. `Double(0.0)` in
`x + Double(0.0)`) into a `ConstHolder<T>`, whose rvalue constructor was
`owned(std::make_shared<T>(std::move(r)))` — a full heap allocation, unconditionally,
regardless of whether `T` is a cheap scalar or a large owned array. Confirmed via
disassembly of the standalone benchmark's hot loop: `operator new` +
`_Sp_counted_ptr_inplace<Double,...>` construction appeared **every loop iteration**, for a
`Double(0.0)` operand that never changes across 500 reps.

This is not specific to subsetting — it fires for **any** binary op with an rvalue scalar
operand anywhere in translated code (`k * dt`, `x + 1`, ...), which is an extremely common
pattern. Likely the more broadly impactful of the two findings here.

**Fixed** in `inst/include/etr_bits/Core/Types.hpp`'s `ConstHolder`: now branches on
`std::is_trivially_copyable_v<T>` (chosen over `IsScalarLike<T>` because `ConstHolder` is
defined before the scalar types/concepts exist in the include chain). Trivially copyable
`T` (scalars) are stored inline via `std::optional<T>`, no allocation. Non-trivially-
copyable `T` (owning/reference-wrapping types like `Buffer`/`SubsetView`) keep the
`shared_ptr` path, needed so copying a `ConstHolder` built from a large owned array stays
O(1) (refcount bump) instead of deep-copying the array's data.

**Re-benchmarked**: no measurable change (39.5ms vs 36-39ms before, noise). Expected in
hindsight — 500 tiny allocations at ~10-30ns each is microseconds against a millisecond-
scale budget. Real, free fix, just not the lever for this benchmark.

### 2. `copy_with_temp`/`assign` recompute `other_obj.size()` every loop iteration — FIXED, confirmed via callgrind, but not the wall-clock bottleneck either

`perf`/`perf stat` are blocked in this sandbox (`perf_event_paranoid=4`, no `CAP_PERFMON`).
Used `valgrind --tool=callgrind` instead (userspace instruction-level simulation, no special
permissions needed) on two isolated standalone binaries (`perf_subset.cpp`/`perf_plain.cpp`,
same access pattern as the "single subset + op"/"plain array + op" buckets, 5000 reps for a
stable count).

`callgrind_annotate` on the subset binary showed **27.23% of all instructions** attributed
to `PreservingSubsetting.hpp`, split roughly 9.08% in `StridedLayout::size()` and 18.16% in
`offset_of()`. The `size()` share was suspicious — `size()` should only be called once per
`copy_with_temp` call, not once per element. Root cause: in `ArrayClass.hpp`, all four
`copy_with_temp`/`assign` implementations had `for (i = 0; i < other_obj.size(); i++)` —
recomputing the loop bound (and, in `assign`, an `ass<>` bound-check) **on every iteration**
instead of hoisting it once. For a plain `Buffer`, `size()` is a trivial O(1) check, so this
was harmless there — but `SubsetView::size()` goes through `std::visit` + a per-dimension
loop, making the redundant recomputation genuinely expensive, 9.08% of all instructions in
the subset case.

**Fixed**: hoisted `const std::size_t n = other_obj.size();` before the loop in all four
occurrences in `Core/ArrayClass.hpp`. Safe in every case — `other_obj` is never mutated
within these loops, only read, so its `size()` is loop-invariant by construction.

Confirmed via callgrind: subset-binary instruction count dropped from 2,115,933,930 to
1,731,683,930 (−18.2%), and the plain-vs-subset instruction-count gap narrowed from 22.0 to
12.0 per element. **But re-running the wall-clock benchmark showed no change** (39.3ms vs
37-39ms before). This is the single most informative result of the whole investigation:
instruction count and wall-clock time are decoupled here, meaning the bottleneck isn't
throughput/instruction-count-bound at all — it's latency-bound on something callgrind's
plain `Ir` counter doesn't weight (a `div`'s many-cycle latency counts as "1 instruction",
same as an `add`).

Chased that decoupling one step further with `--cache-sim=yes --branch-sim=yes`: cache-miss
rates and branch-misprediction rates are both negligible and nearly identical between the
plain and subset binaries — so it isn't cache misses or mispredicts either. Then directly
callgrind-profiled the **non-division fallback path in isolation** for the first time (only
ever tested on the full benchmark before, via `all_strided=false`): its instruction count
(1,732,233,200) is almost identical to the division path's (1,731,683,930, <0.1% apart), but
it has *more* branches (481M vs 385M) and *more* D1 cache misses (16.7M vs 13.7M) — a
different instruction mix, landing at roughly the same total cost.

**This confirms [[project_strided_subsetting_runtime]]'s point 2 finding, precisely this
time**: it is not division specifically, and it is not the fallback's buffer-lookup
specifically — *either* mechanism for resolving an index costs about the same, through
different means (div latency vs. extra branches+loads). The real cost is the existence of a
per-element resolve step at all, decoding a flat index into a memory offset from scratch on
every single `get(i)` call, regardless of which technique does the decoding.

### 3. Prior art: Eigen and xtensor solve exactly this, and don't decode a flat index at all

Researched (fork, web search) how Eigen and xtensor's expression-template evaluators walk
strided views without paying this cost:

- **Eigen**: never flattens to begin with. `Block`'s evaluator does
  `coeff(row,col) = m_argImpl.coeff(m_startRow+row, m_startCol+col)` — the row/col indices
  are threaded as *separate* loop counters through every layer of the expression tree,
  natively nested (`for j: for i: dst(i,j)=src(i,j)`), never combined into a flat index and
  decoded back.
- **xtensor**: the "stepper" pattern (`stepper_tools<row_major>::increment_stepper`,
  `core/xiterator.hpp`) maintains an explicit per-axis position vector alongside the
  iterator and advances it with the classic odometer/carry-ripple loop — `stepper.step(i)`
  is one `offset += stride[i]`. No division or modulo anywhere in the per-element path (a
  separate `unravel_index` utility using `%`/`/` exists, but only for one-off conversions,
  not the iteration hot path).

Both converged on the same answer: **maintain N per-axis running counters, advance with
carry, never decode a flat index** — the classic "stepper"/coordinate-iterator pattern. This
is exactly the direction floated earlier in this investigation (a "last position" cache on
`SubsetView`), now confirmed as the standard solution, not a novel idea — and xtensor's
version is a cleaner target to imitate than a cache: don't reduce to a flat index at the top
of the expression tree at all, keep the walk natively multi-dimensional from
`copy_with_temp`'s loop down through `SubsetView`'s leaf.

### 4. The "mixed" oddity, solved: it's dependency-chain latency, not instruction count

Runtime-verified (via `std::holds_alternative` on `SubsetView::repr`, and debug inspectors
added to `ConstHolder` confirming which storage path — `shared_ptr` vs inline `optional` —
is actually active) that the stride-detection logic itself has no bugs: fully-strided
correctly detects `StridedLayout`, mixed and fully-irregular correctly fall back to
`Buffer<int>`, and the `ConstHolder` fix is confirmed taking effect (no allocation for the
`Double(0.0)` rvalue operand).

But comparing the three 2-D shapes' wall-clock cost surfaced something that contradicted
every model built so far: **mixed (1 strided axis + 1 irregular axis) was faster than
*both* fully-strided and fully-irregular** — not in between them as any reasonable model
would predict.

First isolation attempt (bare-metal, no etr) got this backwards on its own: a corrected
version that rebuilds the lookup table every rep (matching real usage — the original
version, mistakenly, built it once outside the loop) showed fully-strided at 6.4ms vs.
mixed/fully-irregular at 55-60ms — i.e. strided *should* dominate by ~9x if the only costs
are "O(1) compose + `div` access" vs. "O(n) odometer build + array-lookup access."

Real answer came from callgrind on the actual mixed case: **it executes ~3x more
instructions than the strided case (5.04B vs 1.73B Ir) and is still faster in wall-clock.**
That rules out instruction count entirely as the explanatory variable. 38%+13% of mixed's
instructions are in `create_indices` (the odometer construction) — confirmed real and
large — but that work is simple, independent, predictable per-element arithmetic and array
writes, which a CPU executes at high throughput with no stalls. The strided path's `div`
sits inside a much narrower, deeper dependency chain (`Holder` deref → variant check →
`offset_of` → `Buffer::get` → NA-check → `Double::operator+`), with little independent work
nearby to overlap its latency against — unlike the bare-metal test's div, which had almost
no competing work and could hide its latency freely.

**Conclusion**: the cost isn't instruction count, and it isn't the specific arithmetic
(`div` vs. lookup) in isolation — it's how much of a serial dependency chain each element
access forces the CPU to wait on. This *strengthens* the case for the stepper-pattern
redesign (open item below): a per-axis running-counter increment is exactly the
"independent, easily-overlapped" shape that runs fast here, while (unlike the current
fallback) never paying an O(n) rebuild per call either.

## Open items

- **The stepper/coordinate-iterator redesign is the validated next step**, not a hoisting
  fix or an allocation fix (both already applied, wall-clock unchanged either way). This is
  a real architectural change — `SubsetView`/`StridedLayout` would need a stateful walk
  primitive that `copy_with_temp`/`assign`/iterators call `advance()` on instead of
  `get(i)` with an arbitrary `i`, matching xtensor's approach. Bigger scope than anything
  applied so far in this doc; not attempted yet.
- The `ArrayClass.hpp` size()-hoisting fix and the `ConstHolder` allocation fix are both
  real, correct, low-risk improvements worth keeping regardless of whether the stepper
  redesign happens — they just weren't this benchmark's bottleneck.
- Same `other_obj.size()`-in-loop-condition pattern may exist elsewhere in the codebase
  (grepped `i < .*\.size()` across `inst/include/etr_bits/` — dozens of hits) but most are
  on cheap O(1) sizes (plain vectors/Buffers) where recomputation is harmless; not swept.
  Worth a targeted look only where the operand could itself be a `SubsetView`/expression
  type in a hot loop (e.g. `Utilities/CeilingFloorTrunc.hpp`, `Utilities/Casts.hpp`,
  `Core/Transpose.hpp` looked like candidates, not verified).
- Diagnostic artifacts from this session live in `development/`: `perf_subset.cpp`,
  `perf_plain.cpp`, `cg_*.out` (callgrind traces), `a.out`/`perf_subset`/`perf_plain` binaries
  — all scratch, safe to delete.
