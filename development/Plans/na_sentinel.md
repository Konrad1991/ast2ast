# NA as R sentinels instead of an is_na flag

Goal: store NA like R. No `is_na` member, no `p_na` array.

## R's representation

| type    | NA                    | cost per operation in R          |
| ---     | ---                   | ---                              |
| double  | NaN, payload 1954     | none, hardware propagates NaN    |
| integer | `INT_MIN`             | one `x == NA_INTEGER` check      |
| logical | `int`, `INT_MIN`      | as integer                       |

- `NA_real_ + NaN` -> NA or NaN depending on platform. Same in R (accepted).
- `is.na()` = any NaN; `is.nan()` = NaN without payload 1954 (`R_IsNA`).

## Experiment (2026-10-19)

Standalone C++, models with identical semantics except the NA storage.
Bounds checks and overflow checks on unless stated. Times in ms.

| variant                         | spectral_norm | 2dheat |
| ---                             | ---           | ---    |
| etr (generated code)            | 2.1           | 6.0    |
| model with is_na flag           | 2.5           | 4.6    |
| model with sentinels            | 2.1           | 2.4    |
| sentinels, no overflow check    | 1.2           | 1.7    |
| sentinels, no bounds checks     | 2.0           | 0.8    |
| sentinels, no checks at all     | 0.95          | 0.38   |
| plain C++                       | 0.32          | 0.37   |

- flag -> sentinel: 2dheat -49 %, spectral_norm -15 %
- etr is ~30 % slower than the flag model with the same semantics -> separate topic
  (suspects: `static_cast<bool>(j < n)` via Logical, `at()` path)
- remaining gap to plain C++ = bounds + overflow checks (kept on purpose)
- Rcpp 1.1.2 also checks bounds in `operator[]`, but only warns and checks the flat index

## Current state

- `is_na`: 396 refs (343 in `Core/Scalars.hpp`)
- `p_na`: 134 refs (SoA buffers), `is_na_dot`: 56 refs (Dual)
- `SoABuffer<Scalar>` is shared by Double, Integer, Logical -> one step for all three
- `Borrow` already detects R sentinels (`is_r_na_sentinel`)
- `Logical` is `{bool val; bool is_na}`; R stores logicals as `int`

## Steps

On a branch; nothing compiles in between.

1. Double, Integer, Logical together
   - remove `is_na` members; the compiler lists every spot
   - `SoABuffer`: drop `p_na`; SEXP in/out becomes a plain copy
   - Double: NA = NaN payload 1954; `double_na_select` goes away
   - Integer: NA = `INT_MIN`; overflow check: int64 + one unsigned range compare
     (covers `INT_MIN` as well)
   - Logical: `int` with `INT_MIN` -> R layout, borrowing logicals becomes possible
   - comparisons (`==`, `<`, ...) check NaN / `INT_MIN` explicitly -> `NA == 1` is NA
   - casts: int NA -> double NA, double NaN -> int NA
   - Refs (`DoubleRef`, ...), `Borrow`, `SEXPConversions`, C++ tests
2. Dual and ReverseDouble (own buffers)
   - Dual: `val` / `dot` NaN, drop `is_na_dot`
   - ReverseDouble: NA in the tape value, drop the `uint8_t` flag
3. Measure: full suite + `development/Benchmarks/run_all.R` before / after

## Guard

`-ffast-math` implies `-ffinite-math-only`: `std::isnan()` is folded to false,
the payload is not kept. NA handling would break silently.

```cpp
#ifdef __FAST_MATH__
#error "ast2ast does not work with -ffast-math (NA is stored as NaN)"
#endif
```

## Done when

- no `is_na` / `p_na` / `is_na_dot` left
- full suite green, NA / NaN tests unchanged
- 2dheat and spectral_norm measurably faster in `run_all.R`
