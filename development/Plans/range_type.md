# Range type for variables

Status: implemented (2026-10-10). diffuse.R vectorized 111-113 -> 106 ms;
reads inside expressions still go through an index buffer (see
lazy_range_subsetting_contiguous.md: per-element decode was slower).
Extra: `settle_range_vars` (CreateNodeAST.R) re-checks range variables after
inference until stable, because a later downgrade (j <- i; i <- c(1L)) would
otherwise assign a vector to an IntRange in C++.

## Goal

`i <- 2L:(nx - 1L)` currently materializes into `vec(int)`. Then
`temp[i + 1L, j]` misses the strided path (affine ranges, see
`lazy_range_subsetting_contiguous.md`) and builds an index buffer.
With a range type `i` stays lazy: `(start, step, len)`, no allocation.

## Decisions (Konrad, 2026-10-10)

1. Inferred only. No user-facing `type(range)`.
2. Subset assignment on a range variable (`i[2] <- 5L`) -> `vec(int)`.
3. Range variable passed to an inner `fn()`: copy args materialize;
   `ref()` args -> the variable becomes `vec(int)`.
4. Integer ranges only. `1.5:3` stays `vec(double)`.

## Design

R side:
- new data struct `range_vec` in `Nodes.R`, modeled on `borrow_vec`:
  `get_data_struct()` = `"vector"` (all type logic treats it as a vector),
  `get_data_struct_verbose()` = `"range"`, base type always `integer`,
  `stringify()` = `etr::IntRange`.
- produced by: `:` / `seq_len` / `seq_along` with integer result,
  `range +- int scalar`, `int scalar +- range`, `int scalar * range`
  (mirrors `IsAffineRangeOp` in C++).
- a variable is `range_vec` only if all assignments to it are ranges;
  any other assignment joins to `vec(int)` silently (no widening warning,
  the range type is internal).
- decisions 2 and 3 also downgrade to `vec(int)`.

C++ side:
- `using IntRange = Array<Integer, BinaryOperation<Integer, RangeSpec, RangeTrait>>`
- that specialization needs a default ctor and copy/move assignment
  (operator= is deleted for expression arrays today). Both operands are
  trivially copyable, ConstHolder stores them inline.

## Open

- for-loop variable over a range: unchanged (already lazy).
- `length(i)`, `i[[k]]`, `sum(i)`, return of a range: check they work on
  `IntRange` (expression array) — should, via get/size.

## Verify

- `diffuse.R` vectorized variant: `temp[i + 1L, j]` must hit the strided path
  (generated code + timing vs `ast2ast_inline`).
