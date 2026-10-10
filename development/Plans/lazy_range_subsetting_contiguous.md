# Lazy `Range` + N-D strided (contiguous) subset view

Status (2026-10-10): implemented as affine version. Range = (start, step, len)
(`RangeSpec`); range +-* integer scalar stays a range. Strided layout used for
`subset_assign` and const subsets (nested loops, no buffer). Mutable subsets
(reads inside expressions) still use a SubsetView over an index buffer, only
filled via the strided loop.
REJECTED: a view decoding i -> offset per element (div, also Lemire fastdiv):
diffuse.R inline 147 / 134 ms vs 120 ms with index buffer.
Open: range type for variables (`i <- 2L:n` stays lazy).

## Goal

`subset(a, ...)` currently always materializes a flat `Buffer<int>` of
indices inside `create_indices` (two allocations of length = result size:
the per-dim index lists in `fill_index_lists`, plus the final `out`
buffer). For a hot iterative algorithm (nnls, matmul, linalg) that walks
columns/rows/blocks repeatedly this is O(n) allocations per outer step
and dominates the runtime.

Add a second view type that stores the access pattern symbolically
(`offset`, per-dim `len`, per-dim `stride`) instead of enumerating it.
O(1) to construct, no allocation.

This is a **pure addition**. The existing index-buffer `SubsetView` stays
as the general fallback; nothing about it changes.

## The struct (N-D)

Everything in etr talks to a view through `get(i)` / `set(i, v)` /
`size()` / `translate(i)` / `begin`/`end`. The strided view implements the
same interface without a buffer. Rank `N` = number of index args.

```cpp
template <typename O, std::size_t N, typename Trait>
struct StridedView {
  Holder<O> obj;
  std::size_t offset;                  // flat index of (start_0, ..., start_{N-1})
  std::array<std::size_t, N> len;      // extent of the selected run per dim
  std::array<std::size_t, N> vstride;  // flat stride per dim = by_k * parent_stride[k]

  std::size_t translate(std::size_t i) const {
    if constexpr (N == 1) return offset + i * vstride[0];
    std::size_t rem = i, flat = offset;
    for (std::size_t k = 0; k < N; ++k) {   // column-major decode of the result index
      flat += (rem % len[k]) * vstride[k];
      rem  /= len[k];
    }
    return flat;
  }
  auto get(std::size_t i) const { return obj.get().get(translate(i)); }
  template <typename V>
  void set(std::size_t i, const V& v) const { obj.get().set(translate(i), v); }
  std::size_t size() const {
    std::size_t s = 1; for (auto l : len) s *= l; return s;
  }
};
```

Per-access cost: one length-`N` loop with `N` divisions, no memory
traffic beyond the parent element. For rank 2 that is 2 iterations. The
`N == 1` fast path is a single multiply-add. Compared with
`SubsetView`'s `indices.get(i)` load it trades an index-buffer walk for a
little arithmetic, and removes the allocation entirely.

`at_linear` already has an `IsSubsetView` branch that calls
`obj.translate(idx)` and recurses into the parent. Widen that trait (or
add `IsStridedView` and accept both). `translate` returns a flat index in
the parent's own index space (built from the parent's dims/strides), so
subset-of-subset recursion still works.

AD is free: `get` / `set` forward to the parent buffer's own `get` / `set`,
so `Dual` and `ReverseDouble` (tape-id rebind on `set`) behave exactly as
they do through `SubsetView`. No special-casing.

## What is strided-expressible

One arithmetic progression per index dimension:

- column j of m x n           -> `offset=j*m, len={m}, vstride={1}`
- row i                       -> `offset=i,   len={n}, vstride={m}`
- contiguous slice `a[2:5]`   -> `offset=1,   len={4}, vstride={1}`
- `a[2:5, j]` (Range + scalar) -> `j` folds into `offset`
- `a[, j]` (Logical(true) + scalar)
- `a[r1:r2, c1:c2]`           -> rank-2 strided, `len={r2-r1+1, c2-c1+1}`
- `a[seq(1,9,by=2), j]`       -> `vstride[0] = 2 * parent_stride[0]`
  (only if the step form also lowers to the lazy `Range` type)

Not strided-expressible -> falls back to `SubsetView`:

- `a[idx_array, j]` — index array, contiguity only knowable at runtime

Negative step (`5:1`): out of scope for the first cut. Either assert
`by > 0` in the builder, or make `vstride` signed later.

## Prerequisite: a lazy `Range` type

`colon(1, m)` today eagerly returns `Array<Integer>` — already
materialized, so `subset` cannot tell at compile time that it is a
contiguous run.

Introduce a lazy `Range` (`{start, stop, by}`, no buffer) that survives
into `subset` as a *type*. `:` in the DSL lowers to `Range` where the
result feeds a `subset` arg; the eager `Array<Integer>` form stays for
every other use of `:`.

## Dispatch — compile time only, no tagged union

`AllScalarIndices` routes to `at` only when every index is a scalar
**and non-logical** (`a[3, 5]`). A scalar `Logical` is *not* covered by
that concept, so `a[TRUE, 3]` still reaches `create_indices` — and `TRUE`
there means "full run over that dimension", which is exactly strided.
`FALSE` is a runtime error in `create_indices` today and stays one (the
strided builder asserts `TRUE`).

`if constexpr` on the argument pack:

- every arg is `Range`, scalar `Logical(true)`, or a non-logical scalar,
  **and**
- no `Array<...>` arg

-> return `StridedView<O, N, Trait>`, building `offset` / `len` /
`vstride` from `dim` and `parent_stride = make_strides_from_vec<N>(dim)`:

| arg k                  | start_k | extent_k          | by_k |
|------------------------|---------|-------------------|------|
| non-logical scalar `v` | v - 1   | 1                 | 1    |
| `Logical(true)`        | 0       | dim[k]            | 1    |
| `Logical(false)`       | — assert, not handled here    |      |
| `Range(s,e,b)`         | s - 1   | (e - s) / b + 1   | b    |

(An all-scalar-non-logical list never reaches this overload — `at` took
it — so in practice at least one arg is a `Range` or `Logical(true)`, but
that is a consequence, not a precondition.)

then `offset += start_k * parent_stride[k]`,
`vstride[k] = by_k * parent_stride[k]`, `len[k] = extent_k`.
Result `.dim` = `len` (mirror whatever `SubsetView` does re: drop).

Otherwise -> existing `SubsetView` path, untouched.

No runtime contiguity detection, no `std::variant`, no per-access branch.

### What this gives up

`a[c(2,3,4,5), j]` misses the fast path even though it is contiguous —
its indices are an `Array<Integer>`, only knowable at runtime. Acceptable:
not a hot path, and the user can write `a[2:5, j]` to opt in. Runtime
detection (tagged union + a predictable branch on every element access) is
only worth revisiting if accelerating contiguous *arbitrary* vectors
becomes a real need.

## Scope

- new `Range` type + `:` lowering to it in subset position
- new `StridedView<O, N, Trait>` struct
- widen the `IsSubsetView` trait (or add `IsStridedView`) so `at_linear`
  accepts it
- one `subset` overload guarded by the `if constexpr` condition above
- builder: `offset` / `len` / `vstride` from `dim` + `parent_stride` + the
  ranges (table above)
- subset-of-strided compose (offset/stride compose cleanly) — can start
  by routing through `SubsetView` and add the direct compose later

Does not touch the existing `SubsetView`.
