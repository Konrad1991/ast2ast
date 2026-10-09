# Dim class: dims without heap allocation

Status: plan, not started (2026-10-09)

## Problem

`dim` is a `std::vector<std::size_t>` everywhere.
Every new array or expression allocates for it:

- `Array<T, Buffer>`: one vector allocation
- `Array<T, BinaryOperation>` (`x + y`, `a:b`): `ConstHolder<std::vector>` from an
  rvalue -> `make_shared` + vector = 2 allocations
- mutable `SubsetView`: offset buffer + dim vector

Measured in `development/bench_colon.R` (lazy colon + contiguous fast path done):

| bench       | per iteration | work                |
|-------------|---------------|---------------------|
| window_sum  | ~90 ns        | sum of 8 elements   |
| window_fill | ~55 ns        | fill 8 elements     |

Most of that is malloc/free for dims, not computation.

## Idea

`Dim` with small-buffer optimization:

- up to 7 dims inline (on the stack)
- heap only for rank > 7 (practically never)
- copy of the common case = ~64 byte memcpy, no malloc/free

Layout: 8 words = 64 bytes = one cache line.
Slot 0 holds the rank, so 7 dims fit inline.
The heap pointer shares memory with the dims (union), otherwise 72 bytes.
8 inline dims only with a compile-time rank (specialization).

```cpp
class Dim {
  std::size_t rank_ = 0;
  union {
    std::size_t inline_[7];
    std::size_t* heap_; // only rank_ > 7
  };
public:
  std::size_t size() const;
  std::size_t operator[](std::size_t i) const;
  const std::size_t* begin() const;
  const std::size_t* end() const;
};
```

Later option: specializations (e.g. compile-time rank). Not needed for step 1.

## Steps

1. Implement `Dim` (runtime variant above), same interface as a vector:
   `size()`, `operator[]`, `begin/end`, construction from `{n}` / `{nr, nc}`.
2. Store `dim` by value in `Array<T, BinaryOperation>` / `UnaryOperation`,
   not via `ConstHolder`.
   `ConstHolder` only stores trivially copyable types inline;
   `Dim` with a heap fallback isn't -> would land in `make_shared` again.
3. Switch `Array<T, Buffer>` and the views to `Dim`.
4. Migrate call sites. Most go through `dim_view(arr.get_dim())` and only use
   `size()` / `[]`, so they should keep working.
   Call sites that need a real `std::vector` (`match_dims`, `std::vector{...}`
   constructions, `get_dims_from_sexp`) must be adapted.
5. Re-run `bench_colon.R` and `2dheat.R`.

## Notes

- `struct Dim` already exists in `Core/Types.hpp` (unused): 2 dims inline,
  rest on heap. Starting point, but 7 inline is the target.
- `get_dim()` currently returns `const std::vector<std::size_t>&`;
  this signature changes. `std::span` could bridge during migration.
- Inline size: 7 dims ~ 64 bytes per array/expression node; nested
  expressions copy it, still far cheaper than malloc.
